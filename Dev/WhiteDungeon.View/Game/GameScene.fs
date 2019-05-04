namespace WhiteDungeon.View.Game

open wraikny
open wraikny.MilleFeuille.Core.Object

open WhiteDungeon.Core

[<Class>]
type GameScene() =
    inherit Scene()

    let messenger =
        Game.Tart.createMessage