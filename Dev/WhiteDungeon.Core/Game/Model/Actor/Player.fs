namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Game.Model


[<Struct>]
type PlayerID = PlayerID of id : int

module PlayerID =
    let id = function | PlayerID x -> x


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
    let actor p = p.actor

    let id p = p.id

    let occupation p = p.occupation