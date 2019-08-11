module WhiteDungeon.Core.Game.Update.Actor.Player

//open wraikny.Tart.Helper.Extension

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model.Actor
open WhiteDungeon.Core.Game.Update.Actor


let inline decrCoolTimes player =
    let inline decr x = if x = 0us then 0us else x - 1us
    player
    |> Player.mapCoolTime Skill1 decr
    |> Player.mapCoolTime Skill2 decr

let inline update player : Player =
    player
    |> Actor.map Actor.update
    |> decrCoolTimes

