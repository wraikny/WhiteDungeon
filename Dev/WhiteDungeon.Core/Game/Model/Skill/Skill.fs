namespace WhiteDungeon.Core.Game.Model.Skill

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


type Invoker =
    | Player of Actor.PlayerID
    | Enemy //of Actor.EnemyID


type Target =
    | Self
    | Friends of float32 Vec2 Rect
    | Others of float32 Vec2 Rect
    | Area of float32 Vec2 Rect



type Effect =
    | Damage of float32



type SkillEmit =
    {
        delay : uint32
        frame : uint32
        invoker : Invoker
        target : Target
        kind : Effect
    }


type SkillList =
    {
        waitings : SkillEmit list
        playersTarget : SkillEmit list
        enemiesTarget : SkillEmit list
    }

module SkillList =
    let init() =
        {
            waitings = []
            playersTarget = []
            enemiesTarget = []
        }