namespace WhiteDungeon.Core.Update

open wraikny.Tart.Helper.Math

open wraikny.Tart.Helper.Collections

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Update

open FSharpPlus
open wraikny.Tart.Helper.Extension

open System.Collections.Generic

type SkillResult =
    {
        emits : SkillEmit []
        damages : (float32 Vec2 * float32) []
    }

module SkillResult =
    let empty = {
        emits = empty
        damages = empty
    }

    let inline emits x : SkillResult = { empty with emits = x }

    let inline damage v x : SkillResult = { empty with damages = [|v, x|] }

    let inline append a b =
        {
            emits = Array.append a.emits b.emits
            damages = Array.append a.damages b.damages
        }

    let inline addDamage v x r =
        r
        |> append (damage v x)
        


module Effect =
    let apply
        (gameSetting : GameSetting)
        (invoker : Actor)
        (effect : Effect)
        (actor : Actor)
        : (Actor * SkillResult)
        =
        let newActor, result =
            effect |> function
            | Damage v ->
                let damage =
                    gameSetting.damageCalculation
                        v
                        invoker
                        actor

                Actor.addHP -damage actor, SkillResult.empty

            | AddHP v ->
                Actor.addHP v actor, SkillResult.empty

            | DamageF f ->
                let damage = f invoker actor
                Actor.addHP -damage actor, SkillResult.empty

            | F f ->
                f invoker actor
                |> fun (a, b) ->
                    (a, SkillResult.emits b)

        let prevHp = actor.statusCurrent.hp
        let hp = newActor.statusCurrent.hp

        if abs(hp - prevHp) < 1.0f then
            newActor, result
        else
            let pos = newActor.objectBase.position
            newActor, (result |> SkillResult.addDamage pos -(hp - prevHp))


//module IDSkill =
//    let count (idSkill : 'ID IDSkill) : 'ID IDSkill option =
//        if idSkill.frame = 0u then
//            None
//        else
//            Some { idSkill with frame = idSkill.frame - 1u }


module AreaSkill =
    let move gameSetting dungeonModel (areaSkill : AreaSkill) : AreaSkill option =
        areaSkill.move |> function
        | [] -> None
        | x::xs ->
            let areaSkill =
                { areaSkill with
                    move = xs
                    emits = [||]
                    frame = areaSkill.frame - 1u
                }

            x |> function
            | Stay -> Some areaSkill
            | Move diff ->
                let nextAreaSkill, isCollided =
                    areaSkill
                    |> ObjectBase.moveXYTogether gameSetting dungeonModel diff

                if areaSkill.removeWhenHitWall && isCollided then
                    None
                else
                    Some nextAreaSkill

            | Scale diff ->
                let newAreaSkill = areaSkill |> ObjectBase.mapSize ((+) diff)

                let inline insideDungeon () =
                    ObjectBase.insideDungeon
                        gameSetting
                        dungeonModel
                        newAreaSkill

                if areaSkill.removeWhenHitWall && (not <| insideDungeon()) then
                    None
                else
                    Some newAreaSkill

            | Generate emits ->
                Some { areaSkill with emits = emits areaSkill }

    let inline isCollided (areaSkill : AreaSkill) (x) : bool =
        let objArea = ObjectBase.area x
        let skillAra = ObjectBase.area areaSkill
        Rect2.isCollided objArea skillAra
        //|> fun t ->
        //    if t then
        //        let actorId = (Actor.get x).id
        //        printfn "%A: %A\n%A\n" actorId objArea skillAra
        //    t


    let inline checkCollision (actors : seq<'a>) (areaSkill : AreaSkill) =
        { areaSkill with
            collidedActors =
                actors
                |> Seq.filter (isCollided areaSkill)
                |>> (fun x -> (Actor.get x).id)
                |> Set.ofSeq
        }

    let inline private apply (gameSetting) (areaSkill : AreaSkill) (actor : Actor) : Actor * SkillResult =
        areaSkill.collidedActors
        |> Set.contains actor.id
        |> function
        | true ->
            areaSkill.skillBase.effects
            |>> (Effect.apply gameSetting areaSkill.skillBase.invokerActor)
            |> fold (fun (a, s) f ->
                let a, s_ = f a
                (a, SkillResult.append s s_)
            ) (actor, SkillResult.empty)
        | false ->
            (actor, SkillResult.empty)


    let inline applyToActorHolders
        gameSetting
        (onApplyAreaSkill : AreaSkill -> ^a -> ^a -> ^a)
        (skills : Map<_, AreaSkill>)
        (holders : Map<'ID, ^a>) : Map<'ID, ^a> * (SkillResult []) =

        let holders = Map.toArray holders
        let skills = Map.toArray skills

        let resultHolders = List<'ID * ^a>(holders.Length)
        let skillResults = List<_>()

        for (hId, x) in holders do
            let fs =
                skills
                |> Seq.map snd
                |> Seq.filter (flip isCollided x)
                |> Seq.map(fun areaSkill h ->
                    let actor = Actor.get h
                    let (a, e) = apply gameSetting areaSkill actor
                    let nh = Actor.set a h
                    ( onApplyAreaSkill areaSkill h nh, e)
                )

            let mutable holder = x

            for f in fs do
                let h, es = f holder
                holder <- h
                skillResults.Add(es)

            resultHolders.Add(hId, holder)

        (Map.ofSeq resultHolders), skillResults.ToArray()


    let inline hitActorsFilter (areaSkill : AreaSkill) : AreaSkill option =
        if areaSkill.removeWhenHitActor && (areaSkill.collidedActors |> Set.isEmpty |> not) then
            None
        else
            Some areaSkill


module SkillEmit =
    let decrDelay (emit : SkillEmit) : SkillEmit =
        emit |> function
        | Area area ->
            Area { area with skillBase = { area.skillBase with delay = area.skillBase.delay - 1u } }



module SkillList =
    let inline private append (skills : seq<_>) skillList =
        let skillsCount = skills |> length
        {
            skillList with
                nextID = skillList.nextID + uint32 skillsCount
                waitings =
                    skillList.waitings
                    |> Map.toSeq
                    |> (<|>) (
                        skills
                        |> mapi (fun i x -> (skillList.nextID + uint32 i, x))
                    )
                    |> Map.ofSeq
        }

    let inline private mapArea f skillList =
        { skillList with
            areaPlayer = f <!> skillList.areaPlayer
            areaEnemy = f <!> skillList.areaEnemy
            areaAll = f <!> skillList.areaAll
        }

    let inline private filterMapArea f skillList =
        let g =
            Map.toSeq >> Seq.choose(fun (id, x) -> monad {
                let! x = f x
                yield (id, x)
            }) >> Map.ofSeq

        { skillList with
            areaPlayer = g skillList.areaPlayer
            areaEnemy = g skillList.areaEnemy
            areaAll = g  skillList.areaAll
        }

    let private popWaitingSkills (skillList : SkillList) : SkillList =
        let waitings = List<SkillID * SkillEmit>()
        let areaPlayer = List<SkillID * AreaSkill>()
        let areaEnemy = List<SkillID * AreaSkill>()
        let areaAll = List<SkillID * AreaSkill>()

        for (id, emit) in skillList.waitings |> Map.toSeq do
            if emit |> SkillEmit.delay = 0u then
                emit |> function
                | Area area ->
                    area.target |> function
                    | Players ->
                        areaPlayer.Add(id, area)
                    | Enemies ->
                        areaEnemy.Add(id, area)
                    | All ->
                        areaAll.Add(id, area)
            else
                waitings.Add(id, emit |> SkillEmit.decrDelay)

        let append a b =
            seq {
                for x in a -> x
                for x in b |> Map.toSeq -> x
            } |> Map.ofSeq

        {
            nextID = skillList.nextID
            waitings = waitings |> Map.ofSeq
            areaPlayer = append areaPlayer skillList.areaPlayer
            areaEnemy = append areaEnemy skillList.areaEnemy
            areaAll = append areaAll skillList.areaAll
        }

    let private checkCollision (players : Map<_, Player> ) (enemies : Map<_, Enemy>) (skillList : SkillList) =
        let playerActors =
            players
            |> Map.toSeq
            |>> (snd >> fun x -> x.actor)

        let enemiesActor =
            enemies
            |> Map.toSeq
            |>> (snd >> fun x -> x.actor)

        { skillList with
            areaPlayer =
                skillList.areaPlayer
                |>> AreaSkill.checkCollision playerActors
            areaEnemy =
                skillList.areaEnemy
                |>> AreaSkill.checkCollision enemiesActor
            areaAll =
                skillList.areaAll
                |>> AreaSkill.checkCollision(playerActors <|> enemiesActor)
        }

    let private appendGeneratedEmits skillList : SkillList =
        let f : Map<_, AreaSkill> -> _ =
            Map.toSeq
            >> map(snd >> fun areaSkill ->
                areaSkill.emits
                |>> (AreaSkillBuilder.build areaSkill.skillBase.invokerActor >> Area)
            )
            >> Seq.concat

        skillList
        |> append (f skillList.areaPlayer)
        |> append (f skillList.areaEnemy)
        |> append (f skillList.areaAll)

    let inline appendActorSkills (actor) (skills : ^a -> SkillEmitBuilder list) =
        skills actor
        |>> SkillEmitBuilder.build actor
        |> append

    let private applySkillsToModel (model : Model) : Model * _ =
        let skillList = model.skillList
                
        let inline f onApply x = AreaSkill.applyToActorHolders model.gameSetting onApply x

        let chain f (a, b) =
            let x, y = f a
            (x, Array.append y b)

        let players, emits =
            (model.players, empty)
            |> chain (f (fun _ _ a -> a) skillList.areaPlayer)
            |> chain (f (fun _ _ a -> a) skillList.areaAll)

        let enemies, results =
            (model.enemies, emits)
            |> chain (f (Enemy.onApplyAreaSkill) skillList.areaEnemy)
            |> chain (f (Enemy.onApplyAreaSkill) skillList.areaAll)

        let emits =
            seq {
                for result in results do
                    yield! result.emits
            }
            |> Seq.toArray

        let damages =
            seq {
                for result in results do
                    yield! result.damages
            }
            |> Seq.toArray

        { model with
            players = players
            enemies = enemies
        }
        |> SkillList.map(append emits)
        , damages
        

    let update (model : Model) =
        let m, ds =
            model
            |> SkillList.map(fun x ->
                x
                |> popWaitingSkills
                |> filterMapArea (AreaSkill.move model.gameSetting model.dungeonModel)
                |> checkCollision model.players model.enemies
            )
            |> applySkillsToModel

        m |> SkillList.map(fun x ->
            x
            |> appendGeneratedEmits
            |> filterMapArea (AreaSkill.hitActorsFilter)
        )
        , ds