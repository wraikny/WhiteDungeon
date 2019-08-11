namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


type SkillKind = Skill1 | Skill2


type Player =
    {
        actor : Actor

        id : PlayerID
        character : Character

        skill1CoolTime : uint16
        skill2CoolTime : uint16
    }

with
    static member inline objectBase (x : Player) =
        x.actor.objectBase

    static member inline MapActor (x : Player, f) =
        { x with actor = f x.actor }

    static member inline MapObjectBase (x : Player, f) =
        x |> Actor.map (ObjectBase.map f)



module Player =
    let inline actor (player : Player) = player.actor

    let inline id (player : Player) = player.id

    let inline character (player : Player) = player.character

    let inline init size position actorStatus id character = {
        actor = Actor.Actor.init size position (Actor.OfPlayerID id) actorStatus
        id = id
        character = character

        skill1CoolTime = 0us
        skill2CoolTime = 0us
    }

    let inline coolTime kind player =
        kind |> function
        | Skill1 -> player.skill1CoolTime
        | Skill2 -> player.skill2CoolTime

    let inline mapCoolTime kind f player =
        kind |> function
        | Skill1 ->
            { player with skill1CoolTime = f player.skill1CoolTime }
        | Skill2 ->
            { player with skill2CoolTime = f player.skill2CoolTime }