namespace WhiteDungeon.Core.Game.Update

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


    let appendSkills (skills : Skill.SkillEmitBuilder list) (model : Model) : Model =
        model
        |> updateSkillList (
            skills
            |> List.map(Skill.SkillEmitBuilder.build)
            |> Skill.SkillList.append
        )


    let applySkills (model : Model) : Model =
        let gameSetting = model.gameSetting
        let skillList = model.skillList

        { model with
            players =
                model.players
                |> Skill.SkillEmit.applyToActorHolders
                    gameSetting
                    Actor.Player.updateActor
                    skillList.playerEffects
                |> Skill.SkillEmit.applyToActorHolders
                    gameSetting
                    Actor.Player.updateActor
                    skillList.areaEffects
                |> Map.map(fun id ->
                    skillList.playerIDEffects
                    |> List.filter(
                        snd
                        >> Skill.SkillEmit.getTarget
                        >> function
                        | Skill.Players (ids, _) ->
                            ids |> Set.contains id
                        | _ -> false
                    )
                    |> Skill.SkillEmit.getFoledSkills gameSetting
                    |> Actor.Player.updateActor
                )

            enemies =
                model.enemies
                |> Skill.SkillEmit.applyToActorHolders
                    gameSetting
                    Actor.Enemy.updateActor
                    skillList.enemyEffects
                |> Skill.SkillEmit.applyToActorHolders
                    gameSetting
                    Actor.Enemy.updateActor
                    skillList.areaEffects
                |> Map.map(fun id ->
                    skillList.enemyIDEffects
                    |> List.filter(
                        snd
                        >> Skill.SkillEmit.getTarget
                        >> function
                        | Skill.Enemies (ids, _) ->
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
            let model =
                model
                |> updateSkillList (Skill.SkillList.update model)
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

        #if DEBUG

        | AppendSkillEmits ->
            let id = PlayerID 0u
            let player0 = model.players |> Map.find id
            let dir = 
                player0.actor.objectBase.direction
                |> Model.MoveDirection.toVector

            let pos =
                player0.actor.objectBase.position
                + (100.0f .* dir)

            let emit : Skill.SkillEmitBuilder = {
                invokerActor = player0.actor
                invokerID = Skill.InvokerID.Player id
                target = Skill.Target.Area {
                        area = ObjectBase.init (Vec2.init(100.0f, 100.0f)) pos
                        move = seq {
                            for _ in 1..30 -> Skill.Stay
                            for _ in 1..120 -> Skill.Move(dir *. 5.0f)
                        } |> Seq.toList
                    }
                
                
                delay = 0u
                kind = Skill.Damage(fun gs atk def ->
                    0.0f
                )
            }
            let emits = [emit]

            model |> appendSkills emits, Cmd.none

        #endif