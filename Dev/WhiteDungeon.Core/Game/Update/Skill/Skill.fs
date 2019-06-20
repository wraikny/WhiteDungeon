namespace WhiteDungeon.Core.Game.Update.Skill

open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Extension
open wraikny.Tart.Helper.Monad

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Update
open WhiteDungeon.Core.Game.Model.Skill


// type Generator = Model -> SkillEmit list

module EmitMove =
    let apply gameSetting dungeonModel move (obj : ObjectBase) =
        move |> function
        | Stay -> obj
        | Move diff ->
            ObjectBase.moveXYTogether gameSetting dungeonModel diff obj
        | Scale diff ->
            obj
            |> ObjectBase.addSize diff


module AreaSkill =
    let move gameSetting dungeonModel areaSkill =
        areaSkill.move |> function
        | [] -> None
        | x::xs ->
            Some {
                area =
                    areaSkill.area
                    |> EmitMove.apply gameSetting dungeonModel x
                move = xs
            }


module SkillEmit =
    let move gameSetting dungeonModel (emit : SkillEmit) =
        let applyMove = AreaSkill.move gameSetting dungeonModel

        let emitBase = emit.skillEmitBase

        emitBase.target |> function
        | Friends area ->
            applyMove area
            |> Option.map(fun area -> { emitBase with target = Friends area })
        | Others area ->
            applyMove area
            |> Option.map(fun area -> { emitBase with target = Others area })
        | Area area ->
            applyMove area
            |> Option.map(fun area -> { emitBase with target = Area area })
        | Players (ids, frame) ->
            if frame = 0u then
                None
            else
                Some { emitBase with target = Players(ids, frame - 1u)}

        | Enemies (ids, frame) ->
            if frame = 0u then
                None
            else
                Some { emitBase with target = Enemies(ids, frame - 1u)}

        |> Option.map(fun emitBase -> {
            emit with
                skillEmitBase = emitBase
                frame = emit.frame - 1u
        })


    let private isCollided (emit : SkillEmit) (actor : Actor.Actor) : bool =
        emit.skillEmitBase.target |> function
        | Friends { area = o }
        | Others { area = o }
        | Area { area = o } ->
            Rect.isCollided
                (o |> ObjectBase.area)
                (actor.objectBase |> ObjectBase.area)
        | _ -> false
    
    let private apply (gameSetting : GameSetting) (emit : SkillEmit) (actor : Actor.Actor) : Actor.Actor =
        // TODO
        isCollided emit actor
        |> function
        | true ->
            emit.skillEmitBase.effects
            |> Array.map(function
            | Damage calc ->
                let damage =
                    calc
                        gameSetting
                        emit.skillEmitBase.invokerActor.statusCurrent
                        actor.statusCurrent

                Actor.Actor.addHP damage
            ) |> Array.fold (|>) actor
        | false ->
            actor

    let getFoledSkills gameSetting =
        List.map (snd >> apply gameSetting)
        >> List.fold (>>) id

    let applyToActorHolders
        (gameSetting)
        (updater : (Actor.Actor -> Actor.Actor) -> 'a -> 'a)
        (skills : (_ * SkillEmit) list)
        (holders : Map<'ID, 'a>) : Map<'ID, 'a> =

        let foledSkills =
            skills
            |> getFoledSkills gameSetting

        holders
        |> Map.map(fun _ h -> updater foledSkills h)






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

    let skillEmitFuncToEffectsList f = List.map (fun (id, x) -> (id, f x))

    let private mapIDEffects f skillList =
        { skillList with
            playerIDEffects = f skillList.playerIDEffects
            enemyIDEffects = f skillList.enemyIDEffects
        }

    let private mapAreaEffects f skillList =
        { skillList with
            playerEffects = f skillList.playerEffects
            enemyEffects = f skillList.enemyEffects
            areaEffects = f skillList.areaEffects
        }

    let private map f skillList =
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

    let update (model : Model) skillList =
        skillList
        |> move model.gameSetting model.dungeonModel
        |> popWaitingSkills