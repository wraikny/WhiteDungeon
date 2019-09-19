module WhiteDungeon.Core.Update.Building

open wraikny.Tart.Helper
open wraikny.Tart.Math
open wraikny.Tart.Math.Utils
open wraikny.Tart.Advanced
open WhiteDungeon.Core
open WhiteDungeon.Core.Model

open FSharpPlus

let onEntered (kind : BuildingKind) (model : Model) : Model =
    kind |> function
    | Gate -> { model with mode = GateMode }
