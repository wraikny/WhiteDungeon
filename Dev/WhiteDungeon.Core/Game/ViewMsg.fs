namespace WhiteDungeon.Core.Game.ViewMsg

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Advanced

type DungeonView = {
    largeRooms : float32 Vec2 Rect list
    smallRooms : float32 Vec2 Rect list
    corridors : float32 Vec2 Rect list
}

module DungeonView =
    let private roomsToList cellSize =
        Map.toList
        >> List.map (snd >> fun (space : Dungeon.Space) ->
            space.rect
            |> Rect.map (GameSetting.fromDungeonCell cellSize)
        )

    let fromModel (model : Model.Model) = {
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


[<Struct>]
type ViewMsg =
    | GenerateDungeonView of DungeonView