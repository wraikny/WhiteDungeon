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

    let insideDungeon 
        (gameSetting : GameSetting)
        (dungeonModel : Dungeon.DungeonModel) =
        Array.map(fun point ->
            let cell =
                Dungeon.DungeonModel.coordinateToCell
                    gameSetting.dungeonCellSize
                    point
    
            dungeonModel.cells
            |> HashMap.containsKey cell
        )
        >> fold (&&) true