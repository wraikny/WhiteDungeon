namespace WhiteDungeon.Core.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
// open WhiteDungeon.Core.Model

//type Calcs =
//    {
//        damage : Atk -> Def -> HP
//    }



type GameSetting = {
    dungeonCellSize : float32 Vec2
    minPlayerCount : int
    maxPlayerCount : int
    binarySearchCountMovingOnWall : int
    characterSize : float32 Vec2
    occupationDefaultStatus : Map<Occupation, ActorStatus>
}


module GameSetting =
    let fromDungeonCell (cellSize : float32 Vec2) (cell : int Vec2) : float32 Vec2 =
        let cellf = cell |> Vec2.map float32
        cellf * cellSize

    let toDungeonCell (cellSize : float32 Vec2) (coordinate : float32 Vec2) : int Vec2 =
        coordinate / cellSize
        |> Vec2.map (floor >> int)


    open wraikny.Tart.Advanced
    open wraikny.Tart.Helper.Collections

    let insideDungeon 
        (gameSetting : GameSetting)
        (dungeonModel : Dungeon.DungeonModel) =
        Array.map(fun point ->
            let cell =
                toDungeonCell
                    gameSetting.dungeonCellSize
                    point
    
            dungeonModel.cells
            |> HashMap.containsKey cell
        )
        >> Array.fold (&&) true