module WhiteDungeon.Core.Game.Update.Actor.Actor

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model.Actor

let setObjectBase (objectBase : Model.ObjectBase) (actor : Actor) =
    { actor with objectBase = objectBase }


let updateObjectBase f (actor : Actor) =
    actor
    |> setObjectBase (f actor.objectBase)


open WhiteDungeon.Core.Game.Msg

open wraikny.Tart.Helper.Math


let move (gameSetting) (dungeonModel) (move : ActorMove) (direction : float32 Vec2) (actor : Actor) : Actor =
    let speed = move |> function
        | Walk -> actor.statusCurrent.walkSpeed
        | Dash -> actor.statusCurrent.dashSpeed

    let direction = direction |> VectorClass.normalize

    actor
    |> updateObjectBase(
        Update.ObjectBase.move
            gameSetting
            dungeonModel
            (Vec2.init1 speed * direction)
    )