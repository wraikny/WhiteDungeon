namespace WhiteDungeon.Core.QuickPlay

open WhiteDungeon.Core.Model
open wraikny.Tart.Advanced
open WhiteDungeon.Core

type PlayerEdit =
    | Occupation of Occupation


type Msg =
    | IncrPlayer
    | DecrPlayer
    | SelectCharacter of uint32 * CharacterID option
    | GenerateDungeon
    | SetRandomRoomIndex of int
    | GeneratedDungeonModel of Dungeon.DungeonModel



type ViewMsg =
    | ChangeToGame of Game.Model.Model
