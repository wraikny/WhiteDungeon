namespace WhiteDungeon.Core.Game.Model.Skill

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


type InvokerID =
    | Player of PlayerID
    | Enemy of EnemyID


type Target =
    | Players of PlayerID Set
    | Enemies of EnemyID Set
    | Friends of ObjectBase
    | Others of ObjectBase
    | Area of ObjectBase



type Effect =
    // | AddSkillEmits of SkillEmit list
    // | AddConditions of Condition list
    | Damage of (GameSetting -> ActorStatus -> ActorStatus -> float32)

type SkillEmit =
    {
        invokerActor : Actor.Actor
        invokerID : InvokerID
        target : Target
        delay : uint32
        frame : uint32
        frameFirst : uint32
        // removedWhenHit : bool
        kind : Effect
    }

module SkillEmit =
    let getTarget s = s.target


type SkillID = uint32


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
            nextID = 0u
            waitings = []
            playerIDEffects = []
            enemyIDEffects = []
            playerEffects = []
            enemyEffects = []
            areaEffects = []
        }

    let private withArea skillList = List.append skillList.areaEffects

    let toPlayers (skillList) =
        withArea skillList skillList.playerEffects

    let toEnemies (skillList) =
        withArea skillList skillList.enemyEffects

    let allAreaEffects skillList =
        skillList.playerEffects
        |> List.append skillList.enemyEffects
        |> List.append skillList.areaEffects