module WhiteDungeon.Core.Game.Update.Actor.Player

open WhiteDungeon.Core.Game

let setActor actor (player : Model.Actor.Player) =
    { player with actor = actor }


let updateActor f (player : Model.Actor.Player) =
    player
    |> setActor (f player.actor)