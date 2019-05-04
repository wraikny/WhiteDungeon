namespace WhiteDungeon.Core.Preparation

open WhiteDungeon.Core.Model
open WhiteDungeon.Core

open wraikny.Tart.Advanced

type Model = {
    playerCount : int
    players : Map<uint32, CharacterID option>

    dungeonBuilder : Dungeon.DungeonBuilder

    gameSetting : GameSetting
    savedData : SavedData
}