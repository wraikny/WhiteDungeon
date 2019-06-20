module WhiteDungeon.Core.Game.Update.ObjectBase

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Advanced
open WhiteDungeon.Core
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

let setPosition position (obj : ObjectBase) = {
    obj with
        position = position
        lastPosition = obj.position
}

let addPosition diff (obj : ObjectBase) =
    obj |> setPosition (diff + obj.position)

let setSize size (obj : ObjectBase) =
    { obj with size = size }

let addSize diff obj =
    obj |> setSize (obj.size + diff)

let setDirection (direction) (obj : ObjectBase) =
    { obj with direction = direction }

let insideDungeon
    (gameSetting : GameSetting)
    (dungeonModel : Dungeon.DungeonModel) obj =
    let area = obj |> ObjectBase.area
    let lu, rd = area |> Rect.get_LU_RD
    let ld, ru = lu + area.size * Vec2.init(0.0f, 1.0f), lu + area.size * Vec2.init(1.0f, 0.0f)
    
    GameSetting.insideDungeon
        gameSetting
        dungeonModel
        [|lu; rd; ld; ru|]

open WhiteDungeon.Core.Utils

let getMovingDiffWithBinarySearch bsCount isInside (diff : _ Vec2) currentPosition =
    let searchDiff =
        (+) currentPosition
        >>
        Math.binarySearchVec2
            bsCount
            isInside
            currentPosition

    let diffX =
        searchDiff { diff with y = 0.0f }
        |> Vec2.x

    let diffY =
        searchDiff { diff with x = 0.0f }
        |> Vec2.y

    Vec2.init(diffX, diffY)


let move
    (gameSetting : Model.GameSetting)
    (dungeonModel : Dungeon.DungeonModel)
    (diff) (obj : ObjectBase)
    =
    let area = obj |> ObjectBase.area
    let lu, rd = area |> Rect.get_LU_RD
    let ld, ru = lu + area.size * Vec2.init(0.0f, 1.0f), lu + area.size * Vec2.init(1.0f, 0.0f)

    let objectAreaPoints = [|lu; rd; ld; ru|]

    let isInside = GameSetting.insideDungeon gameSetting dungeonModel

    let diff =
        objectAreaPoints
        |> Array.map ((+) diff)
        |> isInside
        |> function
        | true -> diff
        | false ->
            getMovingDiffWithBinarySearch
                gameSetting.binarySearchCountMovingOnWall
                (fun newDiff ->
                    objectAreaPoints
                    |> Array.map ((+) newDiff)
                    |> isInside
                )
                diff
                obj.position
    
    obj
    |> addPosition (diff)
    |> setDirection (MoveDirection.fromVector diff)


let setVelocity velocity (obj : ObjectBase) =
    { obj with velocity = velocity }

let update (obj : ObjectBase) =
    obj
    |> addPosition obj.velocity