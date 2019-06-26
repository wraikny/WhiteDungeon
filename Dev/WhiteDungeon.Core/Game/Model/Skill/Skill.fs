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


type Effect =
    // | AddConditions of Condition list
    | Damage of (GameSetting -> ActorStatus -> ActorStatus -> float32)


type SkillBase = {
    invokerActor : Actor.Actor

    delay : uint32
    effects : Effect []
}


type 'ID IDSkill when 'ID : comparison = {
    skillBase : SkillBase

    targetIDs : 'ID Set

    frame : uint32
}


type SkillBaseBuilder = {
    delay : uint32
    effects : Effect []
}


type IDSkillBuilder = {
    skillBase : SkillBaseBuilder
    
    targetIDs : Actor.ActorID Set
    frame : uint32
}


type AreaTarget =
    | Players
    | Enemies
    | All

type EmitMove =
    | Stay
    | Move of float32 Vec2
    | Scale of float32 Vec2
    | Generate of (AreaSkill -> AreaSkillBuilder [])


and AreaSkill = {
    skillBase : SkillBase
    objectBase : ObjectBase

    target : AreaTarget
    removeWhenHitWall : bool
    removeWhenHitActor : bool

    move : EmitMove list

    emits : AreaSkillBuilder []
    collidedActors : Set<Actor.ActorID>

    frame : uint32
    frameFirst : uint32
}

and AreaSkillBuilder = {
    skillBase : SkillBaseBuilder

    removeWhenHitWall : bool
    removeWhenHitActor : bool
    
    area : ObjectBase
    move : EmitMove list
}


type SkillEmit =
    // | IDPlayer of PlayerID IDSkill
    // | IDEnemy of EnemyID IDSkill
    | Area of AreaSkill


type SkillID = uint32


type SkillList =
    {
        nextID : SkillID
        waitings : Map<SkillID, SkillEmit>
        //idPlayer : Map<SkillID,PlayerID IDSkill>
        //idEnemy : Map<SkillID, EnemyID IDSkill>
        areaPlayer : Map<SkillID, AreaSkill>
        areaEnemy : Map<SkillID, AreaSkill>
        areaAll : Map<SkillID, AreaSkill>
    }

module SkillList =
    let inline init() =
        let m = Map.empty
        {
            nextID = 0u
            waitings = Map.empty
            //idPlayer = Map.empty
            //idEnemy = Map.empty
            areaPlayer = m
            areaEnemy = m
            areaAll = m
        }