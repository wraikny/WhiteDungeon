namespace WhiteDungeon.Core.Game.Update.Skill

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Skill


// type Generator = Model -> SkillEmit list

module SkillEmit =
    let applyToActor (gameSetting : GameSetting) (skill : SkillEmit) (actor : Actor.Actor) : Actor.Actor =
        // TODO
        skill.kind |> function
        | Damage damage ->
            actor

    let applyToActorHolder
        (gameSetting)
        (skill : SkillEmit)
        (updater : (Actor.Actor -> Actor.Actor) -> 'a -> 'a)
        (actor : 'a) : 'a =
        actor |> updater (applyToActor gameSetting skill)

module SkillList =
    let append skills skillList =
        { skillList with
            waitings =
                skillList.waitings
                |> List.append skills
        }

    let popWaitingSkills (skillList : SkillList) =
        let rec popWaitings ws pis eis ps es ars =
            function
            | [] -> ws, pis, eis, ps, es, ars
            | x::xs ->
                xs |>
                if x.delay = 0u then
                    let pis, eis, ps, es, ars =
                        (x.invoker, x.target) |> function
                        | _, Players _ ->
                            (x::pis), eis, ps, es, ars
                        | _, Enemies _ ->
                            pis, (x::eis), ps, es, ars
                        | _, Area _ ->
                            pis, eis, ps, es, (x::ars)
                        | Player _, Friends _
                        | Enemy, Others _
                            ->
                            pis, eis, (x::ps), es, ars
                        | _, Enemies
                        | Enemy, Friends _
                        | Player _, Others _
                            ->
                            pis, eis, ps, (x::es), ars
                    popWaitings ws pis eis ps es ars
                        
                else
                    popWaitings ({x with delay = x.delay - 1u}::ws) pis eis ps es ars

        let waitings, playerIDs, enemyIDs, players, enemies, areas = popWaitings [] [] [] [] [] [] skillList.waitings

        {
            waitings = waitings
            playerIDEffects = playerIDs
            enemyIDEffects = enemyIDs
            playerEffects = skillList.playerEffects |> List.append players
            enemyEffects = skillList.enemyEffects |> List.append enemies
            areaEffects = areas
        }