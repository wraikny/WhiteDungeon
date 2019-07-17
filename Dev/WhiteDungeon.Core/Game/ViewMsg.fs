namespace WhiteDungeon.Core.Game.ViewMsg

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Advanced
open wraikny.Tart.Helper.Collections

type DungeonView = {
    largeRooms : float32 Vec2 Rect list
    smallRooms : float32 Vec2 Rect list
    corridors : float32 Vec2 Rect list
}

module DungeonView =
    let inline private roomsToList cellSize =
        HashMap.toList
        >> List.map (snd >> fun (space : Dungeon.Space) ->
            space.rect
            |> Rect.map (Dungeon.DungeonModel.cellToCoordinate cellSize)
        )

    let inline fromModel (model : Model.Model) = {
        largeRooms =
            model.dungeonModel.largeRooms
            |> roomsToList model.gameSetting.dungeonCellSize
            
        smallRooms =
            model.dungeonModel.smallRooms
            |> roomsToList model.gameSetting.dungeonCellSize

        corridors =
            model.dungeonModel.corridors
            |> roomsToList model.gameSetting.dungeonCellSize
    }

open WhiteDungeon.Core.Game.Model


type ViewMsg =
    | GenerateDungeonView of DungeonView
    | AppendSkills of Skill.SkillEmit list