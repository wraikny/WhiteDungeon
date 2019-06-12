namespace WhiteDungeon.Core.Game.Model.Actor

open wraikny.Tart.Helper.Math
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


[<Struct>]
type ActorDirection =
    | Front
    | Back
    | Right
    | Left
    | FrontRight
    | FrontLeft
    | BackRight
    | BackLeft

module ActorDirection =
    let fromVector v =
        let pi2 = 2.0f * Angle.pi
        let angle = (pi2 + Vec2.angle v) % pi2
        let a = angle * 8.0f / Angle.pi
        let bw s t = s <= a && a < t
        let result =
            if bw 1.0f 3.0f then
                FrontRight
            elif bw 3.0f 5.0f then
                Front
            elif bw 5.0f 7.0f then
                FrontLeft
            elif bw 7.0f 9.0f then
                Left
            elif bw 9.0f 11.0f then
                BackLeft
            elif bw 11.0f 13.0f then
                Back
            elif bw 13.0f 15.0f then
                BackRight
            else
                Right
        
        // printfn "%A %A %A" v angle result

        result

     
type Actor = {
    objectBase : ObjectBase
    direction : ActorDirection
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
        direction = Front
        maxStatus = status
        objectBase = ObjectBase.init size position
    }