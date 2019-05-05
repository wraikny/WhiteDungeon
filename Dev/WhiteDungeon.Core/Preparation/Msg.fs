namespace WhiteDungeon.Core.Preparation

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

[<Struct>]
type PlayerEdit =
    | Occupation of Occupation


[<Struct>]
type Msg =
    | IncrPlayer
    | DecrPlayer
    | SelectCharacter of uint32 * CharacterID option
    | SetRandomRoomIndex of int


[<Struct>]
type ViewMsg =
    | ChangeToGame
