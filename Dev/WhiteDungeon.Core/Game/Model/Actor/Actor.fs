namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Game.Model

type ActorStatus = {
    hp : int
    walkSpeed : float32
    dashSpeed : float32
}

module ActorStatus =
    let hp a = a.hp

    let walkSpeed a = a.walkSpeed

    let dashSpeed a = a.dashSpeed


type Actor = {
    objectBase : ObjectBase

    currentStatus : ActorStatus
    maxStatus : ActorStatus
}

module Actor =
    let currentStatus a = a.currentStatus

    let maxStatus a = a.maxStatus

    let objectBase a = a.objectBase

    let init status objectBase =
        {
            currentStatus = status
            maxStatus = status
            objectBase = objectBase
        }