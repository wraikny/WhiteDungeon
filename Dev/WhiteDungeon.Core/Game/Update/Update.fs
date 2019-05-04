namespace WhiteDungeon.Core.Game.Update

open wraikny.Tart.Core

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model


module Update =
    let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
        model, Cmd.none