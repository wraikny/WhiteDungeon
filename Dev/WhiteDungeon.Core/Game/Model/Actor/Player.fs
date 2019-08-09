namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


type Player = {
    actor : Actor

    id : PlayerID
    character : Character

    skill1CoolTime : uint16
    skill2CoolTime : uint16
}

module Player =
    let inline actor (player : Player) = player.actor

    let inline id (player : Player) = player.id

    let inline character (player : Player) = player.character

    let inline objectBase (player : Player) = player.actor.objectBase

    let inline init size position actorStatus id character = {
        actor = Actor.Actor.init size position (Actor.OfPlayerID id) actorStatus
        id = id
        character = character

        skill1CoolTime = 0us
        skill2CoolTime = 0us
    }