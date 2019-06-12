namespace WhiteDungeon.Core.QuickPlay

open WhiteDungeon.Core.Model
open wraikny.Tart.Advanced
open WhiteDungeon.Core

type PlayerEdit =
    | Occupation of Occupation


type Msg =
    | IncrPlayer
    | DecrPlayer
    | SelectOccupation of uint32 * Occupation
    | GenerateDungeon
    | SetRandomRoomIndex of int
    | GeneratedDungeonModel of Dungeon.DungeonModel



type ViewMsg =
    // | StartLoading
    | ChangeToGame of Game.Model.Model
