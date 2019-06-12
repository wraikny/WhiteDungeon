namespace WhiteDungeon.Core.QuickPlay

open WhiteDungeon.Core.Model
open WhiteDungeon.Core

open wraikny.Tart.Advanced

type SceneMode =
    | Default
    | WaitingDungeonGenerating

type User = string * Occupation option

type Model = {
    mode : SceneMode

    playerCount : int
    players : Map<uint32, User>

    dungeonBuilder : Dungeon.DungeonBuilder
    randomRoomIndex : int

    gameSetting : GameSetting
}

module Model =
    let init dungeonBuilder gameSetting = {
        mode = Default

        playerCount = 1
        players = [(0u, ("Player1", None) )] |> Map.ofList

        dungeonBuilder = dungeonBuilder
        randomRoomIndex = 0

        gameSetting = gameSetting
    }