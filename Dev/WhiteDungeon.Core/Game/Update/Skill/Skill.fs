namespace WhiteDungeon.Core.Game.Update.Skill

open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Skill


type Generator = Model -> SkillEmit list


module SkillList =
    let append skills skillList =
        { skillList with
            waitings =
                skillList.waitings
                |> List.append skills
        }

    let popWaitingSkills (skillList : SkillList) =
        let rec popWaitings players enemies waitings =
            function
            | [] -> players, enemies, waitings
            | x::xs ->
                xs |>
                if x.delay = 0u then
                    (x.invoker, x.target) |> function
                    | _, Area _ ->
                            popWaitings (x::players) (x::enemies) waitings
                    | Player _, Self
                    | Player _, Friends _
                    | Enemy, Others _
                        ->
                            popWaitings (x::players) enemies waitings
                    | Enemy, Self
                    | Enemy, Friends _
                    | Player _, Others _
                        ->
                            popWaitings players (x::enemies) waitings
                else
                    popWaitings players enemies ({x with delay = x.delay - 1u}::waitings)

        let players, enemies, waitings = popWaitings [] [] [] skillList.waitings

        {
            waitings = waitings
            playersTarget = skillList.playersTarget |> List.append players
            enemiesTarget = skillList.enemiesTarget |> List.append enemies
        }