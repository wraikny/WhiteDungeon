module WhiteDungeon.Core.Game.Update.Actor.Actor

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model.Actor

let setObjectBase (gameObject : Model.GameObject) (actor : Actor) =
    { actor with gameObject = gameObject }


let updateObjectBase f (actor : Actor) =
    actor
    |> setObjectBase (f actor.gameObject)


open WhiteDungeon.Core.Game.Msg

open wraikny.Tart.Helper.Math


let move (gameSetting) (dungeonModel) (move : ActorMove) (direction : float32 Vec2) (actor : Actor) : Actor =
    let speed = move |> function
        | Walk -> actor.statusCurrent.walkSpeed
        | Dash -> actor.statusCurrent.dashSpeed

    let direction = direction |> VectorClass.normalize

    actor
    |> updateObjectBase(
        Update.GameObject.move
            gameSetting
            dungeonModel
            (Vec2.init1 speed * direction)
    )
    |> updateObjectBase (Update.GameObject.setDirection (Model.MoveDirection.fromVector direction))