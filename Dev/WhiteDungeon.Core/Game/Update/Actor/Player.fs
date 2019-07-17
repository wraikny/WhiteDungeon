module WhiteDungeon.Core.Game.Update.Actor.Player

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model.Actor
open WhiteDungeon.Core.Game.Update.Actor

let inline setActor actor (player : Player) =
    { player with actor = actor }


let inline updateActor f (player : Player) =
    player
    |> setActor (f player.actor)


let inline updateObjectBase f (player : Player) =
    player
    |> setActor (player.actor |> Actor.updateObjectBase f)


let inline update player : Player =
    player
    |> updateActor Actor.update