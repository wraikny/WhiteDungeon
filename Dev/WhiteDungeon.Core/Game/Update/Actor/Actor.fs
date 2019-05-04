module WhiteDungeon.Core.Game.Update.Actor.Actor

open WhiteDungeon.Core.Game

open WhiteDungeon.Core.Game.Update


let setObjectBase (objectBase : Model.ObjectBase) (actor : Model.Actor.Actor) =
    { actor with objectBase = objectBase }


let updateObjectBase (actor : Model.Actor.Actor) =
    let updatedObjectBase =
        actor.objectBase
        |> ObjectBase.update

    actor
    |> setObjectBase updatedObjectBase