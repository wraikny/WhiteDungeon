namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


type Player = {
    actor : Actor

    id : PlayerID
    character : Character
}

module Player =
    let actor (player : Player) = player.actor

    let id (player : Player) = player.id

    let character (player : Player) = player.character

    let objectBase (player : Player) = player.actor.objectBase

    let init size position actorStatus id character = {
        actor = Actor.Actor.init size position actorStatus
        id = id
        character = character
    }


open wraikny.Tart.Helper.Math


module PlayerBuilder =
    let build size position id actorStatus (character : Character) =
        Player.init
            size
            position
            actorStatus
            id
            character