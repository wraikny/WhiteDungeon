namespace WhiteDungeon.Core.Preparation

open wraikny.Tart.Core

module Update =
    let update (msg : Msg) (model : Model) : Model * Cmd<Msg, _> =
        model, Cmd.none