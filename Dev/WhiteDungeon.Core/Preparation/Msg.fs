namespace WhiteDungeon.Core.Preparation

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


type PlayerEdit =
    | Occupation of Occupation


type Msg =
    | IncrPlayer
    | DecrPlayer
    | EditPlayer of Actor.PlayerID * PlayerEdit