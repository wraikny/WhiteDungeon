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

    let init size position status id occupation = {
        actor = Actor.Actor.init size position status
        id = id
        occupation = occupation
    }


open wraikny.Tart.Helper.Math


type PlayerBuilder = {
    size : float32 Vec2
    status : Actor.ActorStatus
    id : PlayerID
    occupation : Occupation
}

module PlayerBuilder =
    let build (position) (builder : PlayerBuilder) =
        Player.init
            builder.size
            position
            builder.status
            builder.id
            builder.occupation