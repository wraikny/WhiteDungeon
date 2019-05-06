namespace WhiteDungeon.Core.Game.Msg

open wraikny.Tart.Helper.Math

open WhiteDungeon.Core.Game


[<Struct>]
type ActorMove =
    | Walk
    | Dash


[<Struct>]
type MoveDirection =
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
    | PlayerInput of Model.Actor.PlayerID * PlayerInput Set