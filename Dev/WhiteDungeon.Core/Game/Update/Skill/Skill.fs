namespace WhiteDungeon.Core.Game.Update.Skill

open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Monad

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Update
open WhiteDungeon.Core.Game.Model.Skill


module Effect =
    let apply gameSetting (invoker : Actor.Actor) (effect : Effect) (actor : Actor.Actor) =
        effect |> function
        | Damage calc ->
            let damage =
                calc
                    gameSetting
                    invoker.statusCurrent
                    actor.statusCurrent

            Actor.Actor.addHP damage actor


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
                let newObj, isCollided =
                    areaSkill.objectBase
                    |> ObjectBase.moveXYTogether gameSetting dungeonModel diff

                if areaSkill.removeWhenHitWall && isCollided then
                    None
                else
                    Some {
                        areaSkill with
                            objectBase = newObj
                    }

            | Scale diff ->
                Some {
                    areaSkill with
                        objectBase = ObjectBase.addSize diff areaSkill.objectBase
                }
            | Generate emits ->
                Some { areaSkill with emits = emits areaSkill }


    let isCollided (areaSkill : AreaSkill) (actor : Actor.Actor) : bool =
        Rect.isCollided
            (actor.objectBase |> ObjectBase.area)
            (areaSkill.objectBase |> ObjectBase.area)


    let checkCollision (actors : #seq<Actor.Actor>) (areaSkill : AreaSkill) : AreaSkill =
        {
            areaSkill with
                collidedActors =
                    actors
                    |> Seq.filter(isCollided areaSkill)
                    |> Seq.map(fun a -> a.id)
                    |> Set.ofSeq
        }

    let private apply (gameSetting : GameSetting) (areaSkill : AreaSkill) (actor : Actor.Actor) : Actor.Actor =
        // TODO
        areaSkill.collidedActors
        |> Set.contains actor.id
        |> function
        | true ->
            areaSkill.skillBase.effects
            |> Array.map(Effect.apply gameSetting areaSkill.skillBase.invokerActor)
            |> Array.fold (|>) actor
        | false ->
            actor

    let getFoledSkills gameSetting =
        List.map (snd >> apply gameSetting)
        >> List.fold (>>) id

    let applyToActorHolders
        (gameSetting)
        (updater : (Actor.Actor -> Actor.Actor) -> 'a -> 'a)
        (skills : (_ * AreaSkill) list)
        (holders : Map<'ID, 'a>) : Map<'ID, 'a> =

        let foledSkills = getFoledSkills gameSetting skills

        holders
        |> Map.map (fun _ h -> updater foledSkills h)


module SkillList =
    let append skills skillList =
        let skillsCount = skills |> Seq.length
        {
            skillList with
                nextID = skillList.nextID + uint32 skillsCount
                waitings =
                    skillList.waitings
                    |> Map.toList
                    |> List.append (
                        skills
                        |> List.indexed
                        |> List.map(fun (i, x) -> (skillList.nextID + uint32 i, x))
                    )
                    |> Map.ofSeq
        }

    let inline mapArea f skillList =
        { skillList with
            areaPlayer =
                skillList.areaPlayer
                |> Map.map(fun _ s -> f s)
            areaEnemy =
                skillList.areaEnemy
                |> Map.map(fun _ s -> f s)
            areaAll =
                skillList.areaAll
                |> Map.map(fun _ s -> f s)
        }



(*
module SkillEmit =
module SkillList =
    let append skills skillList =
        let skillsCount = skills |> List.length
        { skillList with
            nextID = skillList.nextID + uint32 skillsCount
            waitings =
                skillList.waitings
                |> List.append (
                    skills
                    |> List.indexed
                    |> List.map(fun (i, x) -> (skillList.nextID + uint32 i, x))
                )
        }

    //let inline skillEmitFuncToEffectsList f =
    //    List.map (fun (id, x) -> (id, f x))

    let inline private mapIDEffects f skillList =
        { skillList with
            playerIDEffects = f skillList.playerIDEffects
            enemyIDEffects = f skillList.enemyIDEffects
        }

    let inline private mapAreaEffects f skillList =
        { skillList with
            playerEffects = f skillList.playerEffects
            enemyEffects = f skillList.enemyEffects
            areaEffects = f skillList.areaEffects
        }

    let inline private map f skillList =
        skillList
        |> mapIDEffects f
        |> mapAreaEffects f

    let private popWaitingSkills (skillList : SkillList) : SkillList =
        let rec popWaitings ws (pis, eis, ps, es, ars) =
            function
            | [] -> ws, pis, eis, ps, es, ars
            | skill::xs ->
                let id, (skillEmit : SkillEmit) = skill
                let seBase = skillEmit.skillEmitBase
                xs |>
                if seBase.delay = 0u then
                    (seBase.invokerActor.id, seBase.target) |> function
                    | _, Players _ ->
                        (skill::pis), eis, ps, es, ars
                    | _, Enemies _ ->
                        pis, (skill::eis), ps, es, ars
                    | _, Area _ ->
                        pis, eis, ps, es, (skill::ars)
                    | Actor.Player _, Friends _
                    | Actor.Enemy _, Others _
                        ->
                        pis, eis, (skill::ps), es, ars
                    | _, Enemies _
                    | Actor.Enemy _, Friends _
                    | Actor.Player _, Others _
                        ->
                        pis, eis, ps, (skill::es), ars
                    |> popWaitings ws
                        
                else
                    popWaitings ((id, skillEmit |> SkillEmit.decrDelay)::ws) <| (pis, eis, ps, es, ars)

        let waitings, playerIDs, enemyIDs, players, enemies, areas =
            popWaitings [] ([], [], [], [], []) skillList.waitings

        {
            nextID = skillList.nextID
            waitings = waitings
            playerIDEffects = playerIDs |> List.append skillList.playerIDEffects
            enemyIDEffects = enemyIDs |> List.append skillList.enemyIDEffects
            playerEffects = players |> List.append skillList.playerEffects
            enemyEffects = enemies |> List.append skillList.enemyEffects
            areaEffects = areas |> List.append skillList.areaEffects
        }


    let private checkCollision (model : Model) (skillList : SkillList) =
        let playerActors =
            model.players
            |> Map.toSeq
            |> Seq.map (snd >> fun x -> x.actor)

        let enemiesActor =
            model.enemies
            |> Map.toSeq
            |> Seq.map (snd >> fun x -> x.actor)

        { skillList with
            playerEffects =
                skillList.playerEffects
                |> List.map(fun (id, e) -> id, e |> SkillEmit.checkCollision playerActors)
            enemyEffects =
                skillList.enemyEffects
                |> List.map(fun (id, e) -> id, e |> SkillEmit.checkCollision enemiesActor)
            areaEffects =
                skillList.areaEffects
                |> List.map(fun (id, e) -> id, e |> SkillEmit.checkCollision (Seq.append playerActors enemiesActor))
        }

    
    let private move gameSetting dungeonModel (skillList) : SkillList =
        skillList
        |> map(
            Seq.filterMap (fun (id, x) -> maybe {
                let! v =
                    x
                    |> SkillEmit.move gameSetting dungeonModel
                yield (id, v)
            })
            >> Seq.toList
        )

    let private appendGeneratedEmits skillList : SkillList =
        let f =
            Seq.map snd
            >> Seq.filterMap(fun emit -> maybe{
                let! area = emit |> SkillEmit.target |> Target.area
                yield (emit, area)
            })
            >> Seq.map(fun (emit, x) ->
                x.emits
                |> Seq.map(
                    emit.skillEmitBase.invokerActor
                    |> Skill.EmitCore.build
                    >> SkillEmit.build
                )
            )
            >> Seq.concat
            >> Seq.toList

        skillList
        |> append (f skillList.playerEffects)
        |> append (f skillList.enemyEffects)
        |> append (f skillList.areaEffects)


    let update (model : Model) skillList =
        skillList
        |> move model.gameSetting model.dungeonModel
        |> appendGeneratedEmits
        |> popWaitingSkills

*)