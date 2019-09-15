namespace WhiteDungeon.Core.Game.Model

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

        expoint : uint16
        expointSum : uint16
    }

with
    member inline x.objectBase =
        x.actor.objectBase

    static member inline SetActor (x : Player, y) =
        { x with actor = y }

    static member inline SetObjectBase (x : Player, y) =
        Actor.map (ObjectBase.set y) x


module Player =
    let inline id (player : Player) = player.id

    let inline character (player : Player) = player.character

    let inline init size position level actorStatus id character = {
        actor = Actor.init size position (OfPlayerID id) level actorStatus
        id = id
        character = character

        skill1CoolTime = 0us
        skill2CoolTime = 0us
        expoint = 0us
        expointSum = 0us
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