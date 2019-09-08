namespace WhiteDungeon.Core.Game.Model.Actor

open wraikny.Tart.Helper.Extension
open wraikny.Tart.Helper.Math
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

open FSharpPlus

type EnemyMode =
    | FreeMoving
    | Chasing of PlayerID * float32 Vec2
    | AfterChasing of float32 Vec2
    //| LookingAround of uint16


type Enemy =
    {
        actor : Actor
        id : EnemyID

        kind : EnemyKind

        visionDistance : float32
        visionAngle : float32

        lookingRadian : float32

        mode : EnemyMode

        //target : PlayerID option

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

        //target = None

        hateMap = Map.empty
    }

    let inline lookingDirection (enemy : Enemy) =
        enemy.lookingRadian
        |> Vec2.fromAngle

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
            mode = Chasing(target.id, ObjectBase.position target)
            lookingRadian = ObjectBase.calcAngle enemy target
        }

