module WhiteDungeon.Core.Update.Building

open wraikny.Tart.Core
open WhiteDungeon.Core
open WhiteDungeon.Core.Model

open FSharpPlus

let whenInside (kind : BuildingKind) (model : Model) : Model * Cmd<Msg, ViewMsg> =
    let frame = model.inBuildingFrame
    kind |> function
    | Gate when frame = 60u ->
        { model with mode = GateMode }, Cmd.none
    | Gate -> model, Cmd.none
