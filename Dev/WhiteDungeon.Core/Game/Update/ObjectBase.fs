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
    let existsNextCell =
        [lu; rd]
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