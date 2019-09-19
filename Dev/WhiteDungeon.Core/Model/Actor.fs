namespace WhiteDungeon.Core.Model

open wraikny.Tart.Math
open WhiteDungeon.Core.Model

     
type Actor = {
    id : ActorID
    objectBase : ObjectBase
    level : uint16
    statusCurrent : ActorStatus
    statusDefault : ActorStatus
    currentMove : ActorMove
} with
    member inline x.actor = x

    static member inline SetActor (_ : Actor, y : Actor) = y

    static member inline SetObjectBase (x, y) =
        { x with objectBase = y }

    member inline x.HPRate() = x.statusCurrent.hp / x.statusDefault.hp


module Actor =
    let inline calcStatusOf easing growthRateOverMax (maxLevel : uint16) currentLevel status =
        let maxSt = 0.5f * float32 maxLevel
        
        let growthRate =
            if currentLevel > maxLevel then
                maxSt + (growthRateOverMax * float32 (currentLevel - maxLevel))
            else
                maxSt * (Easing.calculate easing (int maxLevel) (int currentLevel) )

        { status with
            hp = status.hp * growthRate
            atk = status.atk * growthRate
            def = status.def * growthRate
        }

    let inline init size position id level actorStatus = {
            id = id
            level = level
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

    let inline level x = (get x).level