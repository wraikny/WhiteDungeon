namespace WhiteDungeon.Core.Game.Model.Actor

open wraikny.Tart.Helper.Math
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

     
type Actor = {
    gameObject : GameObject
    statusCurrent : ActorStatus
    statusDefault : ActorStatus
}

module Actor =
    let statusCurrent (actor : Actor) = actor.statusCurrent

    let statusDefault (actor : Actor) = actor.statusDefault

    let gameObject (actor : Actor) = actor.gameObject

    let inline stateRate (f : ActorStatus -> ^a) (actor : Actor) =
        let currentStatus = actor.statusCurrent
        let maxStatus = actor.statusDefault
        f currentStatus / f maxStatus

    let init size position objectStatus actorStatus = {
        statusCurrent = actorStatus
        statusDefault = actorStatus
        gameObject = GameObject.init size position objectStatus
    }