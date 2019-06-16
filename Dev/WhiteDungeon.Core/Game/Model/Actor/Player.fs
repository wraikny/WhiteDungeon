namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


[<Struct>]
type PlayerID = PlayerID of id : uint32 with
    member this.Value = this |> function | PlayerID x -> x


type Player = {
    actor : Actor

    id : PlayerID
    character : Character
}

module Player =
    let actor (player : Player) = player.actor

    let id (player : Player) = player.id

    let character (player : Player) = player.character

    let gameObject (player : Player) = player.actor.gameObject

    let init size position objectStatus actorStatus id character = {
        actor = Actor.Actor.init size position objectStatus actorStatus
        id = id
        character = character
    }


open wraikny.Tart.Helper.Math


module PlayerBuilder =
    let build size position id objectStatus actorStatus (character : Character) =
        Player.init
            size
            position
            objectStatus
            actorStatus
            id
            character