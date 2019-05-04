namespace WhiteDungeon.Core.Game.Model



[<Struct>]
type PlayerID = PlayerID of id : int

module PlayerID =
    let id = function | PlayerID x -> x


type Player = {
    id : PlayerID
    actor : Actor
}

module Player =
    let id p = p.id

    let actor p = p.actor