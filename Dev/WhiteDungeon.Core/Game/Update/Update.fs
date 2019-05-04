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


    let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
        model, Cmd.none