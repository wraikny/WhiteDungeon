namespace WhiteDungeon.Core.Game.Model.Actor


open wraikny.Tart.Helper.Math
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

open FSharpPlus

type EnemyMode =
    | FreeMoving
    | Chasing of PlayerID
    | AfterChasing of float32 Vec2


type Enemy =
    {
        actor : Actor
        id : EnemyID

        kind : EnemyKind

        visionDistance : float32
        visionAngle : float32

        lookingRadian : float32

        mode : EnemyMode

        target : PlayerID option

        hateMap : Map<PlayerID, float32>
    }

with
    member inline x.objectBase =
        x.actor.objectBase

    static member inline SetActor (x : Enemy, y) =
        { x with actor = y }

    static member inline SetObjectBase (x : Enemy, y) =
        Actor.map (ObjectBase.set y) x


module Enemy =
    let init size position id actorStatus kind angle visionDistance visionAngleRate = {
        actor = Actor.Actor.init size position (Actor.OfEnemyID id) actorStatus
        id = id
        kind = kind

        visionDistance = visionDistance
        visionAngle = visionAngleRate * 2.0f * Angle.pi

        lookingRadian = angle

        mode = FreeMoving

        target = None

        hateMap = Map.empty
    }

    let addHate targetId hate (enemy : Enemy) =
        { enemy with
            hateMap =
                enemy.hateMap
                |> Map.tryFind targetId
                |>> (+) hate
                |> Option.defaultValue hate
                |> flip (Map.add targetId) enemy.hateMap
        }
        

    let insideVision (enemy : Enemy) (point : float32 Vec2) : bool =
        let pos = enemy |> ObjectBase.position

        (
            let dist = pos - point |> Vector.squaredLength
            dist < enemy.visionDistance * enemy.visionDistance
        )
        &&
        (
            let angle = (point - pos) |> Vec2.angle
            let d = angle - enemy.visionAngle
            (d * d < enemy.visionAngle * enemy.visionAngle / 4.0f)
        )