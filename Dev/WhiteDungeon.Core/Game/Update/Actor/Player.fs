module WhiteDungeon.Core.Game.Update.Actor.Player

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model.Actor
open WhiteDungeon.Core.Game.Update.Actor

let setActor actor (player : Player) =
    { player with actor = actor }


let updateActor f (player : Player) =
    player
    |> setActor (f player.actor)


let updateObjectBase f (player : Player) =
    player
    |> setActor (player.actor |> Actor.updateObjectBase f)


let update player : Player =
    player
    |> updateActor Actor.update