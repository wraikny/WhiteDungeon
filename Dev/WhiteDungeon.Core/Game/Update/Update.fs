﻿namespace WhiteDungeon.Core.Game.Update

open wraikny.Tart.Core

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model

open WhiteDungeon.Core.Game.Update


module Update =
    let incrCount (model : Model) : Model =
        { model with count = model.count + 1u }

    let setPlayers players model =
        { model with players = players }

    let updatePlayers f (model : Model) : Model =
        { model with players = f model.players }


    let updateEachPlayer f (model : Model) : Model =
        model
        |> setPlayers(
            model.players
            |> Map.map(fun _ x -> f x)
        )


    let updatePlayerOf id f (model : Model) =
        model.players
        |> Map.tryFind id
        |> Option.map (fun player ->
            model
            |> updatePlayers (Map.add id (f player))
        )
        |> Option.defaultValue model

    let updateEachEnemy f (model : Model) : Model = {
        model with
            enemies =
                model.enemies
                |> Map.map(fun _ x -> f x)
    }

    open wraikny.Tart.Helper.Math
    open WhiteDungeon.Core.Game.Msg

    let updateSkillList f (model : Model) : Model =
        { model with skillList = f model.skillList }


    let appendSkills (skills : Skill.SkillEmit list) (model : Model) : Model =
        model
        |> updateSkillList (Skill.SkillList.append skills)


    let applySkills (model : Model) : Model =
        let gameSetting = model.gameSetting
        let skillList = model.skillList

        { model with
            players =
                model.players
                |> Skill.SkillEmit.applyToActorHolders
                    gameSetting
                    Actor.Player.updateActor
                    (Skill.SkillList.toPlayers skillList)
                |> Map.map(fun id ->
                    skillList.playerIDEffects
                    |> List.filter(
                        snd
                        >> Skill.SkillEmit.getTarget
                        >> function
                        | Skill.Players ids ->
                            ids |> Set.contains id
                        | _ -> false
                    )
                    |> Skill.SkillEmit.getFoledSkills gameSetting
                    |> Actor.Player.updateActor
                )

            enemies =
                model.enemies
                |> Skill.SkillEmit.applyToActorHolders
                    model.gameSetting
                    Actor.Enemy.updateActor
                    (Skill.SkillList.toEnemies model.skillList)
                |> Map.map(fun id ->
                    skillList.enemyIDEffects
                    |> List.filter(
                        snd
                        >> Skill.SkillEmit.getTarget
                        >> function
                        | Skill.Enemies ids ->
                            ids |> Set.contains id
                        | _ -> false
                    )
                    |> Skill.SkillEmit.getFoledSkills gameSetting
                    |> Actor.Enemy.updateActor
                )
        }



    let update (msg : Msg.Msg) (model : Model) : Model * Cmd<Msg.Msg, ViewMsg.ViewMsg> =
        msg |> function
        | TimePasses ->
            let model =
                model
                |> updateEachPlayer Actor.Player.update
                |> updateEachEnemy Actor.Enemy.update
                |> updateSkillList Skill.SkillList.update
                |> applySkills

            model, Cmd.none

        | PlayerInputs (id, inputSet) ->
            let move, direction = Msg.PlayerInput.getPlayerMoveFromInputs inputSet
            
            let model =
                if direction <> Vec2.zero() then
                    model
                    |> updatePlayerOf id (
                        Update.Actor.Player.updateActor <|
                            Update.Actor.Actor.move
                                model.gameSetting
                                model.dungeonModel
                                move
                                direction
                    )
                else
                    model


            model, Cmd.none

        | AppendSkillEmits ->
            let id = PlayerID 0u
            let player0 = model.players |> Map.find id
            let pos =
                player0.actor.objectBase.position
                |> (+) (Vec2.init1(100.0f) * Model.MoveDirection.toVector player0.actor.objectBase.direction)
            let emit : Skill.SkillEmit = {
                invokerActor = player0.actor
                invokerID = Skill.InvokerID.Player id
                target =
                    Skill.Target.Area
                        (ObjectBase.init (Vec2.init(100.0f, 100.0f)) pos)
                
                
                delay = 0u
                frame = 60u
                frameFirst = 60u
                kind = Skill.Damage(fun gs atk def ->
                    0.0f
                )
            }
            let emits = [emit]

            model |> appendSkills emits, Cmd.none