namespace WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open FSharpPlus

open WhiteDungeon.Core.Model
// open WhiteDungeon.Core.Model

//type Calcs =
//    {
//        damage : Atk -> Def -> HP
//    }


type OccupationSetting =
    {
        status : ActorStatus
        skill1 : Actor.Actor -> Skill.SkillEmitBuilder list
        skill2 : Actor.Actor -> Skill.SkillEmitBuilder list

        skill1CoolTime : uint16
        skill2CoolTime : uint16
    }


type GameSetting = {
    dungeonCellSize : float32 Vec2
    minPlayerCount : int
    maxPlayerCount : int
    binarySearchCountMovingOnWall : int
    characterSize : float32 Vec2
    occupationSettings : Map<Occupation, OccupationSetting>
}


module GameSetting =
    open wraikny.Tart.Advanced
    open wraikny.Tart.Helper.Collections

    let inline collidedWithCell (gameSetting) cell =
        Dungeon.DungeonModel.coordinateToCell
            gameSetting.dungeonCellSize
        >> (=) cell
        |> Seq.exists

    let collidedWiithCells (gameSetting : GameSetting) cells =
        Dungeon.DungeonModel.coordinateToCell
            gameSetting.dungeonCellSize
        >> flip Set.contains cells
        |> Seq.exists

    let insideCells (gameSetting : GameSetting) cells =
        Dungeon.DungeonModel.coordinateToCell
            gameSetting.dungeonCellSize
        >> flip HashMap.containsKey cells
        |> Seq.forall

    let inline insideDungeon 
        (gameSetting : GameSetting)
        (dungeonModel : Dungeon.DungeonModel) =
        insideCells gameSetting dungeonModel.cells