namespace WhiteDungeon.Core.Game.Update

open wraikny.Tart.Core

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model

open WhiteDungeon.Core.Game.Update


module Update =
    let incrCount (model : Model) : Model =
        { model with count = model.count + 1u }


    let updatePlayers f (model : Model) : Model = {
        model with
            players =
                model.players
                |> List.map(fun (id, player) ->
                    (id, f player)
                )
    }

    let updatePlayerOf id f (model : Model) : Model = {
        model with
            players = 
                model.players
                |> List.map(fun (pID, player) ->
                    pID, if pID = id then f player else player
                )
    }

    open wraikny.Tart.Helper.Math
    open WhiteDungeon.Core.Game.Msg

    let getPlayerMoveFromInputs (inputSet : PlayerInput Set) : ActorMove * float32 Vec2 =
        let actorMove =
            if inputSet |> Set.contains PlayerInput.DashKey then Dash else Walk

        let moveDirs = [
            UpKey, Vec2.init(0.0f, -1.0f)
            DownKey, Vec2.init(0.0f, 1.0f)
            RightKey, Vec2.init(1.0f, 0.0f)
            LeftKey, Vec2.init(-1.0f, 0.0f)
        ]

        let direction =
            moveDirs
            |> List.filter(fun (key, _) ->
                inputSet |> Set.contains key
            )
            |> List.map snd
            |> List.fold (+) (Vec2.zero())
            |> VectorClass.normalize
        
        actorMove, direction

    let updateSkillList f (model : Model) : Model =
        { model with skillList = f model.skillList }


    let appendSkills (skills : Skill.SkillEmit list) (model : Model) : Model * Cmd<_, _> =
        model |> updateSkillList (Skill.SkillList.append skills), Cmd.viewMsg[ViewMsg.AppendSkills skills]


    let applySkills (model : Model) : Model =
        // TODO
        model


    let update (msg : Msg.Msg) (model : Model) : Model * Cmd<Msg.Msg, ViewMsg.ViewMsg> =
        msg |> function
        | TimePasses ->
            let model =
                model
                |> updatePlayers (Actor.Player.update)

            model, Cmd.none
        | PlayerInput (id, inputSet) ->
            let move, direction = getPlayerMoveFromInputs inputSet
            
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