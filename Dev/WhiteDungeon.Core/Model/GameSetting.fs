namespace WhiteDungeon.Core.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

type GameSetting = {
    dungeonCellSize : float32 Vec2
    maxPlayerCount : int
}

module GameSetting =
    let fromDungeonCell (cellSize : float32 Vec2) (cell : int Vec2) : float32 Vec2 =
        let cellf = cell |> Vec2.map float32
        cellf * cellSize

    let toDungeonCell (cellSize : float32 Vec2) (coordinate : float32 Vec2) : int Vec2 =
        Vec2.init(coordinate.x / cellSize.x, coordinate.y / cellSize.y)
        |> Vec2.map int