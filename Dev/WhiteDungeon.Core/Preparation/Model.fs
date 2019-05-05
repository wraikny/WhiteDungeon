namespace WhiteDungeon.Core.Preparation

open WhiteDungeon.Core.Model
open WhiteDungeon.Core

open wraikny.Tart.Advanced

type SceneMode =
    | Default
    | WaitingDungeonGenerating

type Model = {
    mode : SceneMode

    playerCount : int
    players : Map<uint32, CharacterID option>

    dungeonBuilder : Dungeon.DungeonBuilder
    randomRoomIndex : int

    gameSetting : GameSetting
    savedData : SavedData
}

module Model =
    let init dungeonBuilder gameSetting savedData = {
        mode = Default

        playerCount = 1
        players = [(0u, None)] |> Map.ofList

        dungeonBuilder = dungeonBuilder
        randomRoomIndex = 0

        gameSetting = gameSetting
        savedData = savedData
    }