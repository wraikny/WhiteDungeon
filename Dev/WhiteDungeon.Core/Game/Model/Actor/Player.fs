namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Game.Model


[<Struct>]
type PlayerID = PlayerID of id : uint32 with
    member this.Value = this |> function | PlayerID x -> x


[<Struct>]
type Occupation =
    | Sword
    | Magic


type Player = {
    actor : Actor

    id : PlayerID
    occupation : Occupation
}

module Player =
    let actor (player : Player) = player.actor

    let id (player : Player) = player.id

    let occupation (player : Player) = player.occupation

    let objectBase (player : Player) = player.actor.objectBase