namespace WhiteDungeon.Core.Game.Msg

open wraikny.Tart.Helper.Math

open WhiteDungeon.Core.Game


[<Struct>]
type ActorMove =
    | Walk
    | Dash


type Msg =
    | TimePasses
    | PlayerMove of Model.Actor.PlayerID * ActorMove * (float32 Vec2)