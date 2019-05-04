namespace WhiteDungeon.Core.Game.ViewModel

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model

type ViewModel = Model


module ViewModel =
    let view (model : Model) = model