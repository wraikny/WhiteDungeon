namespace WhiteDungeon.Core.Game.Model.Actor

open wraikny.Tart.Helper.Math
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

type ActorID =
    | Player of PlayerID
    | Enemy of EnemyID

     
type Actor = {
    id : ActorID
    objectBase : ObjectBase
    statusCurrent : ActorStatus
    statusDefault : ActorStatus
    // skillEmits : Skill.SkillEmit list
    // conditions : Skill.Condition list
}

module Actor =
    let inline statusCurrent (actor : Actor) = actor.statusCurrent

    let inline statusDefault (actor : Actor) = actor.statusDefault

    let inline objectBase (actor : Actor) = actor.objectBase

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
    }