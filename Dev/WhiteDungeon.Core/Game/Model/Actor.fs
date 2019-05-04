namespace WhiteDungeon.Core.Game.Model


type ActorStatus = {
    hp : int
}

module ActorStatus =
    let hp a = a.hp


type Actor = {
    currentStatus : ActorStatus
    maxStatus : ActorStatus

    objectBase : ObjectBase
}

module Actor =
    let currentStatus a = a.currentStatus

    let maxStatus a = a.maxStatus

    let objectBase a = a.objectBase