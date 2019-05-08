namespace WhiteDungeon.Core.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

type GameSetting = {
    dungeonCellSize : float32 Vec2
    minPlayerCount : int
    maxPlayerCount : int
    binarySearchCountMovingOnWall : int
    characterSize : float32 Vec2
}

module GameSetting =
    let fromDungeonCell (cellSize : float32 Vec2) (cell : int Vec2) : float32 Vec2 =
        let cellf = cell |> Vec2.map float32
        cellf * cellSize

    let toDungeonCell (cellSize : float32 Vec2) (coordinate : float32 Vec2) : int Vec2 =
        coordinate / cellSize
        |> Vec2.map (floor >> int)