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


[<AutoOpen>]
module rec Skill =
    type EmitCore =
        {
            target : Target
            delay : uint32
            effects : Effect []
        }

    type EmitMove =
        | Stay
        | Move of float32 Vec2
        | Scale of float32 Vec2
        | Generate of (ObjectBase -> EmitCore [])


    type Area =
        {
            removeWhenHitWall : bool
            removeWhenHitActor : bool
            area : ObjectBase
            move : EmitMove list
            emits : EmitCore []
        }

    type Target =
        | Players of PlayerID Set * uint32
        | Enemies of EnemyID Set * uint32
        | Friends of Area
        | Others of Area
        | Area of Area


    type EmitBase =
        {
            invokerActor : Actor.Actor
            target : Target
            delay : uint32
            effects : Effect []
        }

    module EmitCore =
        let inline build invoker (x : EmitCore) : EmitBase = {
            invokerActor = invoker
            target = x.target
            delay = x.delay
            effects = x.effects
        }
        

type SkillEmit =
    internal {
        skillEmitBase : EmitBase
        frame : uint32
        frameFirst : uint32
    }


module Area =
    let inline area a = a.area


module Target =
    let area = function
        | Friends area
        | Others area
        | Area area -> Some area
        | Players _
        | Enemies _ -> None

module SkillEmit =
    let target s = s.skillEmitBase.target

    let decrDelay s =
        { s with
            skillEmitBase = {
                s.skillEmitBase with
                    delay = s.skillEmitBase.delay - 1u
        }}

    let build (skillEmitBase : EmitBase) : SkillEmit =
        let frame = skillEmitBase.target |> function
            | Friends { move = move }
            | Others { move = move }
            | Area { move = move } ->
                move |> List.length |> uint32
            | Players (_, frame)
            | Enemies (_, frame) ->
                frame

        {
            skillEmitBase = skillEmitBase
            frame = frame
            frameFirst = frame
        }


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
    let inline init() =
        {
            nextID = 0u
            waitings = []
            playerIDEffects = []
            enemyIDEffects = []
            playerEffects = []
            enemyEffects = []
            areaEffects = []
        }