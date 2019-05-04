namespace WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math

type GameSetting = {
    dungeonCellSize : float32 Vec2
}

module GameSetting =
    let cellToCoordinate (cell : int Vec2) (gameSetting : GameSetting) : float32 Vec2 =
        let cellf = cell |> Vec2.map float32
        let cellSize = gameSetting.dungeonCellSize
        Vec2.init(cellf.x * cellSize.x, cellf.y * cellSize.y)

    let coordinateToCell (coordinate : float32 Vec2) (gameSetting : GameSetting) : int Vec2 =
        let cellSize = gameSetting.dungeonCellSize
        Vec2.init(coordinate.x / cellSize.x, coordinate.y / cellSize.y)
        |> Vec2.map int