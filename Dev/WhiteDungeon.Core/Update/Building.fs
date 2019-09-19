module WhiteDungeon.Core.Update.Building

open wraikny.Tart.Helper
open wraikny.Tart.Math
open wraikny.Tart.Math.Utils
open wraikny.Tart.Advanced
open WhiteDungeon.Core
open WhiteDungeon.Core.Model

open FSharpPlus

let whenInside (kind : BuildingKind) (model : Model) : Model =
    let frame = model.inBuildingFrame
    kind |> function
    | Gate when frame = 60u ->
        { model with mode = GateMode }
    | Gate -> model