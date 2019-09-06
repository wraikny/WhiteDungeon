namespace WhiteDungeon.Core.Game.Model.Actor


open wraikny.Tart.Helper.Math
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

type EnemyMode =
    | FreeMoving
    | Chasing of PlayerID
    | AfterChasing of float32 Vec2


type Enemy =
    {
        actor : Actor
        id : EnemyID

        kind : EnemyKind

        lookAngleRadian : float32

        mode : EnemyMode
    }

with
    member inline x.objectBase =
        x.actor.objectBase

    static member inline SetActor (x : Enemy, y) =
        { x with actor = y }

    static member inline SetObjectBase (x : Enemy, y) =
        Actor.map (ObjectBase.set y) x


module Enemy =
    let init size position id actorStatus kind angle = {
        actor = Actor.Actor.init size position (Actor.OfEnemyID id) actorStatus
        id = id
        kind = kind

        lookAngleRadian = angle

        mode = FreeMoving
    }