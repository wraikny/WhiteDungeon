module WhiteDungeon.Core.Game.Update.Actor.Enemy

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Actor
open WhiteDungeon.Core.Game.Update.Actor

let inline update enemy : Enemy =
    enemy
    |> Actor.update