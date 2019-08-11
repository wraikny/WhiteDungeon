namespace WhiteDungeon.Core.Game.Model.Actor

open wraikny.Tart.Helper.Math
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

type ActorID =
    | OfPlayerID of PlayerID
    | OfEnemyID of EnemyID


[<Struct>]
type ActorMove =
    | Walk
    | Dash

     
type Actor =
    {
        id : ActorID
        objectBase : ObjectBase
        statusCurrent : ActorStatus
        statusDefault : ActorStatus
        // skillEmits : Skill.SkillEmit list
        // conditions : Skill.Condition list
        currentMove : ActorMove
    }
with
    static member inline MapObjectBase (x, f) =
        { x with objectBase = f x.objectBase }


module Actor =
    let inline statusCurrent (actor : Actor) = actor.statusCurrent

    let inline statusDefault (actor : Actor) = actor.statusDefault

    //let inline objectBase (actor : Actor) = actor.objectBase

    let inline stateRate (f : ActorStatus -> ^a) (actor : Actor) =
        let currentStatus = actor.statusCurrent
        let maxStatus = actor.statusDefault
        f currentStatus / f maxStatus

    let inline init size position id actorStatus = {
        id = id
        statusCurrent = actorStatus
        statusDefault = actorStatus
        objectBase = ObjectBase.init size position
        // skillEmits = []
        // conditions = []
        currentMove = Walk
    }

    let inline get (x : ^a) =
        (^a : (static member actor : ^a -> Actor) x)

    let inline map f (x : ^a) =
        (^a : (static member MapActor : ^a * (Actor -> Actor) -> ^b) (x, f))