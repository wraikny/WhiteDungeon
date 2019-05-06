module WhiteDungeon.Core.Game.Update.Actor.Player

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Update.Actor

let setActor actor (player : Model.Actor.Player) =
    { player with actor = actor }


let updateActor f (player : Model.Actor.Player) =
    player
    |> setActor (f player.actor)


let updateObjectBase f (player : Model.Actor.Player) =
    player
    |> setActor (player.actor |> Actor.updateObjectBase f)