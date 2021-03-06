﻿namespace WhiteDungeon.Core.Model

open Affogato
open WhiteDungeon.Core.Model

open FSharpPlus

type EnemyMsg =
    | RotateMsg of float32


type MoveValueContainer = {
    rotateFrame : uint16
} with
    static member Zero : MoveValueContainer = {
        rotateFrame = zero
    }

type EnemyMode =
    | FreeMoving
    | Chasing of PlayerID * float32 Vector2 * angle:float32
    | AfterChasing of pos:float32 Vector2 * angle:float32
    //| LookingAround of uint16


type Enemy = {
    actor : Actor
    id : EnemyID

    kind : EnemyKind

    visionDistance : float32
    visionAngle : float32

    lookingRadian : float32

    mode : EnemyMode

    //target : PlayerID option

    skillCoolTime : uint16

    hateMap : Map<PlayerID, float32>

    moveValues : MoveValueContainer
} with
    member inline x.objectBase =
        x.actor.objectBase

    static member inline SetActor (x : Enemy, y) =
        { x with actor = y }

    static member inline SetObjectBase (x : Enemy, y) =
        Actor.map (ObjectBase.set y) x

    static member inline SetMoveValues (x : Enemy, y) =
        { x with moveValues = y }


module Enemy =
    let init size position id level actorStatus kind angle visionDistance visionAngleRate =
        {
            actor = Actor.init size position (OfEnemyID id) level actorStatus
            id = id
            kind = kind

            visionDistance = visionDistance
            visionAngle = visionAngleRate * 2.0f * pi

            lookingRadian = angle

            mode = FreeMoving

            skillCoolTime = zero

            hateMap = Map.empty

            moveValues = zero
        }


    let inline lookingDirection (enemy : Enemy) =
        enemy.lookingRadian
        |> Vector2.ofAngle

    let addHate targetId hate (enemy : Enemy) =
        { enemy with
            hateMap =
                enemy.hateMap
                |> Map.tryFind targetId
                |>> (+) hate
                |> Option.defaultValue hate
                |> flip (Map.add targetId) enemy.hateMap
        }

    let setTarget (target : Player) (enemy : Enemy) : Enemy =
        { enemy with
            mode = Chasing(target.id, ObjectBase.position target, ObjectBase.movingDirection target)
            lookingRadian = ObjectBase.calcAngle enemy target
        }

