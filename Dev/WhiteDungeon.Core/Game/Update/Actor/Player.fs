module WhiteDungeon.Core.Game.Update.Actor.Player

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model.Actor
open WhiteDungeon.Core.Game.Update.Actor

let inline setSkill1CoolTime x player =
    { player with skill1CoolTime = x }

let inline setSkill2CoolTime x player =
    { player with skill2CoolTime = x }

let inline decrCoolTimes player =
    let inline decr x = if x = 0us then 0us else x - 1us
    player
    |> setSkill1CoolTime (decr player.skill1CoolTime)
    |> setSkill2CoolTime (decr player.skill2CoolTime)

let inline setActor actor (player : Player) =
    { player with actor = actor }


let inline updateActor f (player : Player) =
    player
    |> setActor (f player.actor)


let inline updateObjectBase f (player : Player) =
    player
    |> setActor (player.actor |> Actor.updateObjectBase f)


let inline update player : Player =
    player
    |> updateActor Actor.update
    |> decrCoolTimes