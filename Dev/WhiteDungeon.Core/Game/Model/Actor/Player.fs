namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


type Player = {
    actor : Actor

    id : PlayerID
    character : Character
}

module Player =
    let inline actor (player : Player) = player.actor

    let inline id (player : Player) = player.id

    let inline character (player : Player) = player.character

    let inline objectBase (player : Player) = player.actor.objectBase

    let inline init size position actorStatus id character = {
        actor = Actor.Actor.init size position (Actor.Player id) actorStatus
        id = id
        character = character
    }


open wraikny.Tart.Helper.Math


//module PlayerBuilder =
//    let inline build size position id actorStatus (character : Character) =
//        Player.init
//            size
//            position
//            actorStatus
//            id
//            character