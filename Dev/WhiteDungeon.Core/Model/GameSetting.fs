namespace WhiteDungeon.Core.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open FSharpPlus
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
    open wraikny.Tart.Advanced
    open wraikny.Tart.Helper.Collections

    let collidedWithCell (gameSetting) cell =
        Seq.exists(fun point ->
            Dungeon.DungeonModel.coordinateToCell
                gameSetting.dungeonCellSize
                point
            |> (=) cell
        )

    let collidedWiithCells
        (gameSetting : GameSetting)
        (cells : _) =
        Seq.exists(fun point ->
            let cell =
                Dungeon.DungeonModel.coordinateToCell
                    gameSetting.dungeonCellSize
                    point
            cells
            |> Set.contains cell
        )

    let insideCells
        (gameSetting : GameSetting)
        (cells : _) =
        Seq.forall(fun point ->
            let cell =
                Dungeon.DungeonModel.coordinateToCell
                    gameSetting.dungeonCellSize
                    point
            cells
            |> HashMap.containsKey cell

        )

    let inline insideDungeon 
        (gameSetting : GameSetting)
        (dungeonModel : Dungeon.DungeonModel) =
        insideCells gameSetting dungeonModel.cells