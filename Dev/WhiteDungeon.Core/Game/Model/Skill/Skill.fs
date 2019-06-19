﻿namespace WhiteDungeon.Core.Game.Model.Skill

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

//type ConditionKind =
//    | StatusAdd of ActorStatus
//    | StatusMul of ActorStatus
//    | Move of (int * float32 Vec2)

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
    // | AddSkillEmits of SkillEmit list
    // | AddConditions of Condition list
    | Damage of (ActorStatus -> ActorStatus -> float32)

type SkillEmit =
    {
        invoker : Actor.Actor
        invokerKind : Invoker
        target : Target
        delay : uint32
        frame : uint32
        removedWhenHit : bool
        kind : Effect
    }


type SkillID = uint64


type SkillList =
    {
        nextID : SkillID
        waitings : (SkillID * SkillEmit) list
        playerIDEffects : (SkillID * SkillEmit) list
        enemyIDEffects : (SkillID * SkillEmit) list
        playerEffects : (SkillID * SkillEmit) list
        enemyEffects : (SkillID * SkillEmit) list
        areaEffects : (SkillID * SkillEmit) list
    }

module SkillList =
    let init() =
        {
            nextID = 0uL
            waitings = []
            playerIDEffects = []
            enemyIDEffects = []
            playerEffects = []
            enemyEffects = []
            areaEffects = []
        }