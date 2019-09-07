namespace WhiteDungeon.Core.Game.Model.Actor.Skill

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Actor

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


type AreaTarget =
    | Players
    | Enemies
    | All


type Effect =
    // | AddConditions of Condition list
    | Damage of float32
    | AddHP of float32
    | DamageF of (Actor -> Actor -> float32)
    | F of (Actor -> Actor -> (Actor * SkillEmit []))



and SkillBase = {
    invokerActor : Actor.Actor

    delay : uint32
    effects : Effect []
}


and 'ID IDSkill when 'ID : comparison = {
    skillBase : SkillBase

    targetIDs : 'ID Set

    frame : uint32
}

and AreaSkill =
    {
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
with
    static member inline SetObjectBase (x : AreaSkill, y : ObjectBase) =
        { x with objectBase = y }



and SkillEmit =
    // | IDPlayer of PlayerID IDSkill
    // | IDEnemy of EnemyID IDSkill
    | Area of AreaSkill


and SkillBaseBuilder = {
    delay : uint32
    effects : Effect []
}


and IDSkillBuilder = {
    skillBase : SkillBaseBuilder
    
    targetIDs : Actor.ActorID Set
    frame : uint32
}

and AreaSkillBuilder = {
    skillBase : SkillBaseBuilder
    objectBase : ObjectBase

    target : AreaTarget
    removeWhenHitWall : bool
    removeWhenHitActor : bool
    
    move : EmitMove list
}


and EmitMove =
    | Stay
    | Move of float32 Vec2
    | Scale of float32 Vec2
    | Generate of (AreaSkill -> AreaSkillBuilder [])

module SkillBaseBuilder =
    let build (invoker) (builder : SkillBaseBuilder) : SkillBase =
        {
            invokerActor = invoker
            delay = builder.delay
            effects = builder.effects
        }

module AreaSkillBuilder =
    let build (invoker) (builder : AreaSkillBuilder) : AreaSkill =
        let frame = builder.move |> List.length |> uint32
        {
            skillBase =
                builder.skillBase
                |> SkillBaseBuilder.build invoker
            objectBase = builder.objectBase

            target = builder.target

            removeWhenHitActor = builder.removeWhenHitActor
            removeWhenHitWall = builder.removeWhenHitWall

            move = builder.move

            emits = Array.empty
            collidedActors = Set.empty

            frame = frame
            frameFirst = frame
        }

module SkillEmit =
    let inline skillBase s =
        s |> function
        | Area a -> a.skillBase

    let inline delay s =
        s |> skillBase |> fun s -> s.delay


type SkillEmitBuilder =
    | AreaBuilder of AreaSkillBuilder


module SkillEmitBuilder =
    let build invoker (builder) : SkillEmit =
        builder |> function
        | AreaBuilder area ->
            AreaSkillBuilder.build invoker area
            |> Area


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
    let init =
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

    let inline get (x : ^a) : SkillList =
        (^a : (member skillList : _) x)

    let inline set skillList (x : ^a) : ^a =
        (^a : (static member SetSkillList : _*_->_) (x, skillList))

    let inline map f x =
        set (f (get x)) x