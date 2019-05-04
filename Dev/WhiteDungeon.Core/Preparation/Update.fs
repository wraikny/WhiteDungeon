module WhiteDungeon.Core.Preparation.Update

open wraikny.Tart.Core

open WhiteDungeon.Core


let incrPlayer (model : Model) : Model =
    let playerCount =
        (model.playerCount + 1)
        |> min model.gameSetting.maxPlayerCount

    { model with
        playerCount = playerCount
        players =
            let id = Game.Model.Actor.PlayerID (uint32 playerCount)
            if model.players |> Map.containsKey id then
                model.players
            else
                model.players
                |> Map.add id ()
    }


let update (msg : Msg) (model : Model) : Model * Cmd<Msg, _> =
    msg |> function
    | IncrPlayer ->
        incrPlayer model, Cmd.none
    | DecrPlayer ->
        { model with
            playerCount =
                (model.playerCount - 1)
                |> min model.gameSetting.minPlayerCount
        }, Cmd.none