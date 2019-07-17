module WhiteDungeon.Core.Game.Update.Actor.Enemy

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model.Actor
open WhiteDungeon.Core.Game.Update.Actor

let inline setActor actor (enemy : Enemy) =
    { enemy with actor = actor }


let inline updateActor f (enemy : Enemy) =
    enemy
    |> setActor (f enemy.actor)


let inline updateObjectBase f (enemy : Enemy) =
    enemy
    |> setActor (enemy.actor |> Actor.updateObjectBase f)


let inline update enemy : Enemy =
    enemy
    |> updateActor Actor.update