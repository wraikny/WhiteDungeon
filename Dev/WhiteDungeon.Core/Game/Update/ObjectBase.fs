module WhiteDungeon.Core.Game.Update.ObjectBase

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Math.Utils
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Advanced
open WhiteDungeon.Core
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

open FSharpPlus

let inline setPosition position (obj : ObjectBase) = {
    obj with
        position = position
        lastPosition = obj.position
}

let inline addPosition diff (obj : ObjectBase) =
    obj |> setPosition (diff + obj.position)

let inline setSize size (obj : ObjectBase) =
    { obj with size = size }

let inline addSize diff obj =
    obj |> setSize (obj.size + diff)

let inline setDirection (direction) (obj : ObjectBase) =
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

let inline private bsDiffXYTogether bsCount isInside (diff : _ Vec2) currentPosition =
    BinarySearch.vector
        bsCount
        isInside
        currentPosition
        (currentPosition + diff)

let private bsDiffXYAnother bsCount isInside (diff : _ Vec2) currentPosition =
    let searchDiff =
        (+) currentPosition
        >>
        BinarySearch.vector
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

let private moveWithBS
    f
    (gameSetting : Model.GameSetting)
    (dungeonModel : Dungeon.DungeonModel)
    (diff) (obj : ObjectBase)
    =
    let area = obj |> ObjectBase.area
    let lu, rd = area |> Rect.get_LU_RD
    let ld, ru = lu + area.size * Vec2.init(0.0f, 1.0f), lu + area.size * Vec2.init(1.0f, 0.0f)

    let objectAreaPoints = [|lu; rd; ld; ru|]

    let isInside = GameSetting.insideDungeon gameSetting dungeonModel

    let isCollided =
        objectAreaPoints
        |>> ((+) diff)
        |> isInside
        |> not

    let diff =
        if isCollided then
            f
                gameSetting.binarySearchCountMovingOnWall
                (fun newDiff ->
                    objectAreaPoints
                    |>> ((+) newDiff)
                    |> isInside
                )
                diff
                obj.position
        else
            diff
    
    obj
    |> addPosition (diff)
    |> setDirection (MoveDirection.fromVector diff), isCollided

let moveXYTogether = moveWithBS bsDiffXYTogether

let moveXYAnother = moveWithBS bsDiffXYAnother


let inline setVelocity velocity (obj : ObjectBase) =
    { obj with velocity = velocity }

let inline update (obj : ObjectBase) =
    obj
    |> addPosition obj.velocity