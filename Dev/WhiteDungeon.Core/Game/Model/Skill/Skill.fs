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


type EmitMove =
    | Stay
    | Move of float32 Vec2
    | Scale of float32 Vec2


type InvokerID =
    | Player of PlayerID
    | Enemy of EnemyID


type AreaSkill =
    {
        area : ObjectBase
        move : EmitMove list
    }

module AreaSkill =
    let area a = a.area

type Target =
    | Players of PlayerID Set * uint32
    | Enemies of EnemyID Set * uint32
    | Friends of AreaSkill
    | Others of AreaSkill
    | Area of AreaSkill


module Target =
    let areaSkill = function
        | Friends area
        | Others area
        | Area area -> Some area
        | Players _
        | Enemies _ -> None


type Effect =
    // | AddSkillEmits of SkillEmit list
    // | AddConditions of Condition list
    | Damage of (GameSetting -> ActorStatus -> ActorStatus -> float32)
        

type SkillEmit =
    internal {
        invokerActor : Actor.Actor
        invokerID : InvokerID
        target : Target
        delay : uint32
        frame : uint32
        frameFirst : uint32
        kind : Effect
    }

module SkillEmit =
    let getTarget s = s.target


type SkillEmitBuilder =
    {
        invokerActor : Actor.Actor
        invokerID : InvokerID
        target : Target
        delay : uint32
        kind : Effect
    }

module SkillEmitBuilder =
    let build (builder : SkillEmitBuilder) : SkillEmit =
        let frame = builder.target |> function
            | Friends { move = move }
            | Others { move = move }
            | Area { move = move } ->
                move |> List.length |> uint32
            | Players (_, frame)
            | Enemies (_, frame) ->
                frame

        {
            invokerActor = builder.invokerActor
            invokerID = builder.invokerID
            target = builder.target
            delay = builder.delay
            frame = frame
            frameFirst = frame
            kind = builder.kind
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