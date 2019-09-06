namespace WhiteDungeon.Core.Game.Update.Skill

open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Collections

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Update
open WhiteDungeon.Core.Game.Model.Skill

open FSharpPlus
open wraikny.Tart.Helper.Extension

module Effect =
    let apply
        (gameSetting : GameSetting)
        (invoker : Actor.Actor)
        (effect : Effect)
        (actor : Actor.Actor)
        : (Actor.Actor * SkillEmit [])
        =
        effect |> function
        | Damage v ->
            let damage =
                gameSetting.damageCalculation
                    v
                    invoker
                    actor

            Actor.Actor.addHP -damage actor, empty

        | AddHP v ->
            Actor.Actor.addHP v actor, empty

        | DamageF f ->
            let damage = f invoker actor
            Actor.Actor.addHP -damage actor, empty

        | F f -> f invoker actor


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



    let inline isCollided (areaSkill : AreaSkill) (actor : Actor.Actor) : bool =
        Rect.isCollided
            (actor.objectBase |> ObjectBase.area)
            (areaSkill.objectBase |> ObjectBase.area)


    let inline checkCollision (actors : seq<Actor.Actor>) (areaSkill : AreaSkill) : AreaSkill =
        {
            areaSkill with
                collidedActors =
                    actors
                    |> filter (isCollided areaSkill)
                    |>> (fun a -> a.id)
                    |> Set.ofSeq
        }

    let private apply (gameSetting) (areaSkill : AreaSkill) (actor : Actor.Actor) : Actor.Actor * SkillEmit [] =
        // TODO
        areaSkill.collidedActors
        |> Set.contains actor.id
        |> function
        | true ->
            areaSkill.skillBase.effects
            |>> (Effect.apply gameSetting areaSkill.skillBase.invokerActor)
            |> fold (fun (a, s) f ->
                let a, s_ = f a
                (a, Array.append s s_)
            ) (actor, empty)
        | false ->
            actor, empty

    let getFoledSkills gameSetting skillsMap actor =
        skillsMap
        |> Map.toSeq
        |> map (snd >> apply gameSetting)
        |> fold (fun (a, s) f ->
            let a, s_ = f a
            (a, Array.append s s_)
        ) (actor, empty)

    let inline applyToActorHolders
        gameSetting
        (skills : Map<_, AreaSkill>)
        (holders : Map<'ID, ^a>) : Map<'ID, ^a> * SkillEmit [] =

        let foledSkills = getFoledSkills gameSetting skills

        let rec f targets actors emits =
            targets |> function
            | [] -> (Map.ofList actors, emits)
            | (id, x)::xs ->
                let a, e = foledSkills (Actor.Actor.get x)
                f xs ((id, Actor.Actor.set a x)::actors) (Array.append e emits)
        f (Map.toList holders) empty empty

    let inline hitActorsFilter (areaSkill : AreaSkill) : AreaSkill option =
        if areaSkill.removeWhenHitActor && (areaSkill.emits |> Array.isEmpty |> not) then
            None
        else
            Some areaSkill


module SkillEmit =
    let decrDelay (emit : SkillEmit) : SkillEmit =
        emit |> function
        | Area area ->
            Area { area with skillBase = { area.skillBase with delay = area.skillBase.delay - 1u } }



module SkillList =
    let append skills skillList =
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

    let inline mapArea f skillList =
        { skillList with
            areaPlayer = f <!> skillList.areaPlayer
            areaEnemy = f <!> skillList.areaEnemy
            areaAll = f <!> skillList.areaAll
        }

    let inline filterMapArea f skillList =
        let g =
            Map.toSeq >> filterMap(fun (id, x) -> monad{
                let! x = f x
                yield (id, x)
            }) >> Map.ofSeq

        { skillList with
            areaPlayer = g skillList.areaPlayer
            areaEnemy = g skillList.areaEnemy
            areaAll = g  skillList.areaAll
        }

    open System.Collections.Generic

    let private popWaitingSkills (skillList : SkillList) : SkillList =
        let waitings = new List<SkillID * SkillEmit>()
        let areaPlayer = new List<SkillID * AreaSkill>()
        let areaEnemy = new List<SkillID * AreaSkill>()
        let areaAll = new List<SkillID * AreaSkill>()

        for (id, emit) in skillList.waitings |> Map.toSeq do
            if emit |> SkillEmit.delay = 0u then
                emit |> function
                | Skill.Area area ->
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

    let private checkCollision (players : Map<_, Actor.Player> ) (enemies : Map<_, Actor.Enemy>) (skillList : SkillList) =
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
        let f : Map<_, Skill.AreaSkill> -> _ =
            Map.toSeq
            >> map(snd >> fun areaSkill ->
                areaSkill.emits
                |>> (AreaSkillBuilder.build areaSkill.skillBase.invokerActor >> Skill.Area)
            )
            >> Seq.concat

        skillList
        |> append (f skillList.areaPlayer)
        |> append (f skillList.areaEnemy)
        |> append (f skillList.areaAll)

    let update (model : Model) =
        SkillList.map(
            popWaitingSkills
            >> filterMapArea (AreaSkill.hitActorsFilter)
            >> filterMapArea (AreaSkill.move model.gameSetting model.dungeonModel)
            >> checkCollision model.players model.enemies
            >> appendGeneratedEmits
        ) model