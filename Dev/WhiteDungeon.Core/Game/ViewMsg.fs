﻿namespace WhiteDungeon.Core.Game.ViewMsg

open WhiteDungeon.Core.Game

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Advanced

type DungeonView = {
    largeRooms : float32 Rect list
    smallRooms : float32 Rect list
    corridors : float32 Rect list
}

module DungeonView =
    let private roomsToList cellSize =
        Map.toList
        >> List.map (snd >> fun (space : Dungeon.Space) ->
            space.rect
            |> Rect.map (Model.GameSetting.fromDungeonCell cellSize)
        )

    let fromModel (gameSetting : Model.GameSetting) (dungeonModel : Dungeon.DungeonModel) = {
        largeRooms =
            dungeonModel.largeRooms
            |> roomsToList gameSetting.dungeonCellSize
            
        smallRooms =
            dungeonModel.smallRooms
            |> roomsToList gameSetting.dungeonCellSize

        corridors =
            dungeonModel.corridors
            |> roomsToList gameSetting.dungeonCellSize
    }


type ViewMsg =
    | GenerateDungeonView of DungeonView