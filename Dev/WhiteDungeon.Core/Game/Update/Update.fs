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

    open WhiteDungeon.Core.Game.Msg

    let update (msg : Msg.Msg) (model : Model) : Model * Cmd<Msg.Msg, ViewMsg.ViewMsg> =
        msg |> function
        | TimePasses ->
            model, Cmd.none
        | PlayerMove (id, move, direction) ->
            model
            |> updatePlayerOf id (
                Update.Actor.Player.updateActor <|
                    Update.Actor.Actor.move move direction
            )
            , Cmd.none