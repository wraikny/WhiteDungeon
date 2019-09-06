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
        currentMove : ActorMove
    }
with
    member inline x.actor = x

    static member inline SetActor (_ : Actor, y : Actor) = y

    static member inline SetObjectBase (x, y) =
        { x with objectBase = y }


module Actor =
    let inline init size position id actorStatus = {
        id = id
        statusCurrent = actorStatus
        statusDefault = actorStatus
        objectBase = ObjectBase.init size position
        currentMove = Walk
    }

    let inline get (x : ^a) : Actor =
        (^a : (member actor : _) x)

    let inline set (a : Actor) (x : ^a) : ^a =
        (^a : (static member SetActor : _*_->_) (x, a))

    let inline map f x = set (f (get x)) x

    let inline statusCurrent x = (get x).statusCurrent

    let inline statusDefault x = (get x).statusDefault

    let inline statusRate f x =
        let c = statusCurrent x
        let d = statusDefault x
        f c / f d

    let inline mapStatus f x =
        map (fun a -> { a with statusCurrent = f (statusCurrent x)}) x