module WhiteDungeon.Core.Game.Update.GameObject

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Advanced
open WhiteDungeon.Core
open WhiteDungeon.Core.Game.Model

let setPosition position (obj : GameObject) = {
    obj with
        position = position
        lastPosition = obj.position
}


let addPosition diff (obj : GameObject) =
    obj |> setPosition (diff + obj.position)


let move
    (gameSetting : Model.GameSetting)
    (dungeonModel : Dungeon.DungeonModel)
    (diff) (obj : GameObject)
    =

    let area = obj |> GameObject.area
    let lu, rd = area |> Rect.get_LU_RD
    let ld, ru = lu + area.size * Vec2.init(0.0f, 1.0f), lu + area.size * Vec2.init(1.0f, 0.0f)

    let objectAreaPoints = [lu; rd; ld; ru]

    let rec serchMaxDiff count diffSum current target =
        if count <= 0 then diffSum
        else
            let middle = (current + target) / Vec2.init1(2.0f)
            let newDiffSum = diffSum + (middle - current)

            let existsNextCell =
                objectAreaPoints
                |> List.map(fun point ->
                    let nextPosition = point + newDiffSum
                    let nextCell =
                        Model.GameSetting.toDungeonCell
                            gameSetting.dungeonCellSize
                            nextPosition
                
                    dungeonModel.cells
                    |> Map.containsKey nextCell
                )
                |> List.fold (&&) true

            if existsNextCell then
                serchMaxDiff (count - 1) newDiffSum middle target
            else
                serchMaxDiff (count - 1) diffSum current middle

    let searchDiff =
        ((+) obj.position)
        >>
        serchMaxDiff
            gameSetting.binarySearchCountMovingOnWall
            (Vec2.init1 0.0f)
            obj.position

    let diffX =
        searchDiff ({ diff with y = 0.0f } : _ Vec2)
        |> Vec2.x

    let diffY =
        searchDiff ({ diff with x = 0.0f } : _ Vec2)
        |> Vec2.y
    
    obj |> addPosition (Vec2.init(diffX, diffY))


let setVelocity velocity (obj : GameObject) = {
    obj with
        velocity = velocity
}

let setDirection (direction) (obj : GameObject) =
    { obj with direction = direction }

let update (obj : GameObject) =
    obj
    |> addPosition obj.velocity