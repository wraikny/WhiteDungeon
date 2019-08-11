namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


type Enemy =
    {
        actor : Actor

        id : EnemyID
    }

with
    member inline x.objectBase =
        x.actor.objectBase

    static member inline SetActor (x : Enemy, y) =
        { x with actor = y }

    static member inline SetObjectBase (x : Enemy, y) =
        Actor.map (ObjectBase.set y) x
