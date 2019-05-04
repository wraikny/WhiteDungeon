module WhiteDungeon.Core.Game.Update.ObjectBase


open WhiteDungeon.Core.Game.Model

let setPosition position (obj : ObjectBase) = {
    obj with
        position = position
        lastPosition = obj.position
}


let addPosition diff (obj : ObjectBase) =
    obj |> setPosition (diff + obj.position)


let setVelocity velocity (obj : ObjectBase) = {
    obj with
        velocity = velocity
}

let update (obj : ObjectBase) =
    obj
    |> addPosition obj.velocity