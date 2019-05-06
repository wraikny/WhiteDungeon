module WhiteDungeon.Core.Game.Update.ObjectBase

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Advanced
open WhiteDungeon.Core
open WhiteDungeon.Core.Game.Model

let setPosition position (obj : ObjectBase) = {
    obj with
        position = position
        lastPosition = obj.position
}


let addPosition diff (obj : ObjectBase) =
    obj |> setPosition (diff + obj.position)


let move
    (gameSetting : Model.GameSetting)
    (dungeonModel : Dungeon.DungeonModel)
    (diff) (obj : ObjectBase)
    =
    let area = obj |> ObjectBase.area
    let lu, rd = area |> Rect.get_LU_RD
    let ld, ru = lu + area.size * Vec2.init(0.0f, 1.0f), lu + area.size * Vec2.init(1.0f, 0.0f)

    let existsNextCell =
        // TODO: bug(ずれる)
        [(lu - gameSetting.dungeonCellSize); rd; ld; ru]
        |> List.map(fun point ->
            let nextPosition = point + diff
            let nextCell =
                Model.GameSetting.toDungeonCell
                    gameSetting.dungeonCellSize
                    nextPosition
        
            dungeonModel.cells
            |> Map.containsKey nextCell
        )
        |> List.fold (&&) true
    
    if existsNextCell then
        obj |> addPosition diff
    else
        obj


let setVelocity velocity (obj : ObjectBase) = {
    obj with
        velocity = velocity
}

let update (obj : ObjectBase) =
    obj
    |> addPosition obj.velocity