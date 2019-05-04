module WhiteDungeon.Core.Game.Update.Actor.Actor

open WhiteDungeon.Core.Game


let setObjectBase (objectBase : Model.ObjectBase) (actor : Model.Actor.Actor) =
    { actor with objectBase = objectBase }


let updateObjectBase f (actor : Model.Actor.Actor) =
    actor
    |> setObjectBase (f actor.objectBase)