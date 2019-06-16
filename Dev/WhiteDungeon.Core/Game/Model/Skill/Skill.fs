namespace WhiteDungeon.Core.Game.Model.Skill

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


type Invoker =
    | Player of Actor.PlayerID


type Target =
    | Self
    | Friend of float32 Vec2 Rect
    | Opponent of float32 Vec2 Rect
    | Area of float32 Vec2 Rect



type Effect =
    | Damage of float32



type Skill =
    {
        delay : uint32
        invoker : Invoker
        target : Target
        kind : Effect
    }

type Generator = GameSetting -> Skill list