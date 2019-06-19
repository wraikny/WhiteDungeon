namespace WhiteDungeon.Core.Game.Update.Skill

open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Extension

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Update
open WhiteDungeon.Core.Game.Model.Skill


// type Generator = Model -> SkillEmit list

module SkillEmit =
    let move gameSetting dungeonModel (emit : SkillEmit) =
        let moveObj = ObjectBase.move gameSetting dungeonModel
        emit.target |> function
        | Friends o ->
            { emit with target = Friends (moveObj o.velocity o)}
        | Others o ->
            { emit with target = Others (moveObj o.velocity o)}
        | Area o ->
            { emit with target = Area (moveObj o.velocity o)}
        | _ ->
            emit

    let private isCollided (emit : SkillEmit) (actor : Actor.Actor) : bool =
        emit.target |> function
        | Friends o
        | Others o
        | Area o ->
            Rect.isCollided
                (o |> ObjectBase.area)
                (actor.objectBase |> ObjectBase.area)
        | _ -> false
    
    let private apply (gameSetting : GameSetting) (emit : SkillEmit) (actor : Actor.Actor) : Actor.Actor =
        // TODO
        isCollided emit actor |> function
        | true ->
            emit.kind |> function
            | Damage calc ->
                let damage =
                    calc
                        gameSetting
                        emit.invokerActor.statusCurrent
                        actor.statusCurrent

                actor
                |> Actor.Actor.addHP damage
        | false ->
            actor

    let getFoledSkills
        (gameSetting)
        (skills : (_ * SkillEmit) list) =
        let foledSkills =
            skills
            |> List.map (snd >> apply gameSetting)
            |> List.fold (>>) id

        foledSkills

    let applyToActorHolders
        (gameSetting)
        (updater : (Actor.Actor -> Actor.Actor) -> 'a -> 'a)
        (skills : (_ * SkillEmit) list)
        (holders : Map<'ID, 'a>) : Map<'ID, 'a> =

        let foledSkills =
            getFoledSkills gameSetting skills

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

    let private updateIDEffects f skillList =
        { skillList with
            playerIDEffects = f skillList.playerIDEffects
            enemyIDEffects = f skillList.enemyIDEffects
        }

    let private updateAreaEffects f skillList =
        { skillList with
            playerEffects = f skillList.playerEffects
            enemyEffects = f skillList.enemyEffects
            areaEffects = f skillList.areaEffects
        }

    let private updateUnWaitingEffects f skillList =
        skillList
        |> updateIDEffects f
        |> updateAreaEffects f

    let private decrFrame (skillList : SkillList) : SkillList =
        let decr emit =
            if emit.frame = 0u then
                None
            else
                Some {
                    emit with
                        frame = emit.frame - 1u
                }

        let f =
            Seq.filterMap(fun (id, x) ->
                decr x |> function
                | Some x -> Some (id, x)
                | None -> None
            ) >> Seq.toList

        updateUnWaitingEffects f skillList

    let private popWaitingSkills (skillList : SkillList) : SkillList =
        let rec popWaitings ws pis eis ps es ars =
            function
            | [] -> ws, pis, eis, ps, es, ars
            | skill::xs ->
                let id, x = skill
                xs |>
                if x.delay = 0u then
                    let pis, eis, ps, es, ars =
                        (x.invokerID, x.target) |> function
                        | _, Players _ ->
                            (skill::pis), eis, ps, es, ars
                        | _, Enemies _ ->
                            pis, (skill::eis), ps, es, ars
                        | _, Area _ ->
                            pis, eis, ps, es, (skill::ars)
                        | Player _, Friends _
                        | Enemy _, Others _
                            ->
                            pis, eis, (skill::ps), es, ars
                        | _, Enemies _
                        | Enemy _, Friends _
                        | Player _, Others _
                            ->
                            pis, eis, ps, (skill::es), ars
                    popWaitings ws pis eis ps es ars
                        
                else
                    popWaitings ((id, {x with delay = x.delay - 1u})::ws) pis eis ps es ars

        let waitings, playerIDs, enemyIDs, players, enemies, areas =
            popWaitings [] [] [] [] [] [] skillList.waitings

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
        |> updateAreaEffects (
            (SkillEmit.move gameSetting dungeonModel)
            |> skillEmitFuncToEffectsList
        )
        

    let update (model : Model) skillList =
        skillList
        |> move model.gameSetting model.dungeonModel
        |> decrFrame
        |> popWaitingSkills