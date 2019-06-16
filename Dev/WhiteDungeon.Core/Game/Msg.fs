namespace WhiteDungeon.Core.Game.Msg

open wraikny.Tart.Helper.Math

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game


[<Struct>]
type ActorMove =
    | Walk
    | Dash


[<Struct>]
type InputDirection =
    | Right
    | Left
    | Up
    | Down


[<Struct>]
type PlayerInput =
    | RightKey
    | LeftKey
    | UpKey
    | DownKey
    | DashKey



type Msg =
    | TimePasses
    | PlayerInput of PlayerID * PlayerInput Set