namespace WhiteDungeon.Core.Game.Update

open wraikny.Tart.Core

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model


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

    let getPlayerMoveFromInput (inputSet : PlayerInput Set) : ActorMove * float32 Vec2 =
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
            |> List.fold (+) ( Vec2.zero() )
            |> VectorClass.normalize
        
        actorMove, direction


    let update (msg : Msg.Msg) (model : Model) : Model * Cmd<Msg.Msg, ViewMsg.ViewMsg> =
        msg |> function
        | TimePasses ->
            model, Cmd.none
        | PlayerInput (id, inputSet) ->
            let move, direction = getPlayerMoveFromInput inputSet
            
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