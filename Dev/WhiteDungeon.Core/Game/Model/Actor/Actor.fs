namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

     
type Actor = {
    objectBase : ObjectBase

    currentStatus : ActorStatus
    maxStatus : ActorStatus
}

module Actor =
    let currentStatus (actor : Actor) = actor.currentStatus

    let maxStatus (actor : Actor) = actor.maxStatus

    let objectBase (actor : Actor) = actor.objectBase

    let inline stateRate (f : ActorStatus -> ^a) (actor : Actor) =
        let currentStatus = actor.currentStatus
        let maxStatus = actor.maxStatus
        f currentStatus / f maxStatus

    let init size position status = {
        currentStatus = status
        maxStatus = status
        objectBase = ObjectBase.init size position
    }