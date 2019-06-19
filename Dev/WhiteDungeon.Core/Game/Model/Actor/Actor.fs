namespace WhiteDungeon.Core.Game.Model.Actor

open wraikny.Tart.Helper.Math
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

     
type Actor = {
    objectBase : ObjectBase
    statusCurrent : ActorStatus
    statusDefault : ActorStatus
    // skillEmits : Skill.SkillEmit list
    // conditions : Skill.Condition list
}

module Actor =
    let statusCurrent (actor : Actor) = actor.statusCurrent

    let statusDefault (actor : Actor) = actor.statusDefault

    let objectBase (actor : Actor) = actor.objectBase

    let inline stateRate (f : ActorStatus -> ^a) (actor : Actor) =
        let currentStatus = actor.statusCurrent
        let maxStatus = actor.statusDefault
        f currentStatus / f maxStatus

    let init size position actorStatus = {
        statusCurrent = actorStatus
        statusDefault = actorStatus
        objectBase = ObjectBase.init size position
        // skillEmits = []
        // conditions = []
    }