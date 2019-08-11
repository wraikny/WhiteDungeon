namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


type Enemy =
    {
        actor : Actor

        id : EnemyID
    }

with
    static member inline objectBase (x : Enemy) =
        x.actor.objectBase

    static member inline MapActor (x : Enemy, f) =
        { x with actor = f x.actor }

    static member inline MapObjectBase (x : Enemy, f) =
        { x with actor = ObjectBase.map f x.actor }