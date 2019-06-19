namespace WhiteDungeon.Core.Game.Model.Skill

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

//type ConditionKind =
//    | StatusAdd of ActorStatus
//    | StatusMul of ActorStatus


//type Condition =
//    {
//        priority : int
//        frame : uint32
//        kind : ConditionKind
//    }

//module Condition =
//    let priority c = c.priority


type Invoker =
    | Player of PlayerID
    | Enemy //of EnemyID


type Target =
    | Players of PlayerID list
    | Enemies //of EnemyID list
    | Friends of ObjectBase
    | Others of ObjectBase
    | Area of ObjectBase



type Effect =
    | AddSkillEmits of SkillEmit list
    // | AddConditions of Condition list
    | Damage of float32
    | Move of float32 Vec2

and SkillEmit =
    {
        invoker : Invoker
        target : Target
        delay : uint32
        frame : uint32
        removedWhenHit : bool
        kind : Effect
    }


type SkillList =
    {
        waitings : SkillEmit list
        playerIDEffects : SkillEmit list
        enemyIDEffects : SkillEmit list
        playerEffects : SkillEmit list
        enemyEffects : SkillEmit list
        areaEffects : SkillEmit list
    }

module SkillList =
    let init() =
        {
            waitings = []
            playerIDEffects = []
            enemyIDEffects = []
            playerEffects = []
            enemyEffects = []
            areaEffects = []
        }