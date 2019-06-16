module WhiteDungeon.Core.Game.Update.Actor.Enemy

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model.Actor
open WhiteDungeon.Core.Game.Update.Actor

let setActor actor (enemy : Enemy) =
    { enemy with actor = actor }


let updateActor f (enemy : Enemy) =
    enemy
    |> setActor (f enemy.actor)


let updateObjectBase f (enemy : Enemy) =
    enemy
    |> setActor (enemy.actor |> Actor.updateObjectBase f)


let update enemy : Enemy =
    enemy
    |> updateActor Actor.update