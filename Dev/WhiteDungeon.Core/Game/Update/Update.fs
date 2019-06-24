namespace WhiteDungeon.Core.Game.Update

open wraikny.Tart.Core

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Skill

open WhiteDungeon.Core.Game.Update


module Update =
    let inline incrCount (model : Model) : Model =
        { model with count = model.count + 1u }

    let inline setPlayers players model =
        { model with players = players }

    let inline updatePlayers f (model : Model) : Model =
        { model with players = f model.players }


    let inline updateEachPlayer f (model : Model) : Model =
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

    let inline updateEachEnemy f (model : Model) : Model = {
        model with
            enemies =
                model.enemies
                |> Map.map(fun _ x -> f x)
    }

    open wraikny.Tart.Helper.Math
    open WhiteDungeon.Core.Game.Msg

    let inline updateSkillList f (model : Model) : Model =
        { model with skillList = f model.skillList }


    let appendSkills (skills : Skill.EmitBase list) (model : Model) : Model =
        model
        |> updateSkillList (
            skills
            |> List.filter(
                (fun b -> b.target)
                >> Skill.Target.area
                >> Option.map(
                    (fun a -> a.area)
                    >> ObjectBase.insideDungeon model.gameSetting model.dungeonModel
                )
                >> Option.defaultValue true
            )
            |> List.map(Skill.SkillEmit.build)
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
                        >> Skill.SkillEmit.target
                        >> function
                        | Skill.Skill.Players (ids, _) ->
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
                        >> Skill.SkillEmit.target
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
                if direction <> Vector.zero() then
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

            let emit : Skill.Skill.EmitBase = {
                invokerActor = player0.actor
                target = Skill.Skill.Target.Area {
                        area = ObjectBase.init (Vec2.init(100.0f, 100.0f)) pos
                        move = seq {
                            for _ in 1..10 -> Skill.Skill.Stay
                            for _ in 1..60 -> Skill.Skill.Move(dir *. 5.0f)
                            for _ in 1..60 -> Skill.Skill.Scale(Vec2.init(3.0f, 3.0f))
                        } |> Seq.toList
                        emits = [||]
                    }
                
                
                delay = 0u
                effects = [|
                    Skill.Damage(fun gs atk def ->
                        0.0f
                    )
                |]
            }
            let emits = [emit]

            model |> appendSkills emits, Cmd.none

        #endif