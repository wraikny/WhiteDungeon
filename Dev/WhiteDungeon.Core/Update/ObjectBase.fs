module WhiteDungeon.Core.Update.ObjectBase

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Math.Utils
open wraikny.Tart.Advanced
open WhiteDungeon.Core
open WhiteDungeon.Core.Model

open FSharpPlus
//open FSharpPlus.Math.Applicative


let inline setDirection (direction) (obj : ObjectBase) =
    { obj with direction = direction }


let inline getCorners (obj : ObjectBase) =
    let area = obj |> ObjectBase.area
    let lu, rd = area |> Rect.get_LU_RD
    let ld, ru = lu + area.size * (Vec2.init 0.0f 1.0f), lu + area.size * (Vec2.init 1.0f 0.0f)
    [|lu; rd; ld; ru|]


let collidedCell (gameSetting : GameSetting) (cell) obj =
    GameSetting.collidedWithCell
        gameSetting
        cell
        (getCorners obj)


let collidedCells (gameSetting : GameSetting) (cells) obj =
    GameSetting.collidedWiithCells
        gameSetting
        cells
        (getCorners obj)

let inline insideDungeon
    (gameSetting : GameSetting)
    (dungeonModel : Dungeon.DungeonModel) x =
    GameSetting.insideDungeon
        gameSetting
        dungeonModel
        (getCorners (ObjectBase.get x))


let inline private bsDiffXYTogether bsCount isInside (diff : _ Vec2) currentPosition : float32 Vec2 =
    BinarySearch.binarySearch
        bsCount
        isInside
        currentPosition
        (currentPosition + diff)

let inline private bsDiffXYAnother bsCount isInside (diff : _ Vec2) currentPosition : float32 Vec2 =
    let searchDiff =
        (+) currentPosition
        >>
        BinarySearch.binarySearch
            bsCount
            isInside
            currentPosition

    let diffX =
        searchDiff { diff with y = 0.0f }
        |> Vector.x

    let diffY =
        searchDiff { diff with x = 0.0f }
        |> Vector.y

    Vec2.init diffX diffY

let inline private moveWithBS
    f
    (gameSetting : GameSetting)
    (dungeonModel : Dungeon.DungeonModel)
    (diff0) (x : ^a)
    =
    let obj = ObjectBase.get x
    let area = obj |> ObjectBase.area
    let lu, rd = area |> Rect.get_LU_RD
    let ld, ru = lu + area.size * (Vec2.init 0.0f 1.0f), lu + area.size * (Vec2.init 1.0f 0.0f)

    let objectAreaPoints = [|lu; rd; ld; ru|]

    let isInside = GameSetting.insideDungeon gameSetting dungeonModel

    let isCollided =
        objectAreaPoints
        |>> ((+) diff0)
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
                diff0
                obj.position
        else
            diff0
    
    x
    |> ObjectBase.map (
        ObjectBase.mapPosition ((+) diff)
        >> setDirection (MoveDirection.fromVector diff0)
        >> fun x -> { x with isMoved = true }
    ), isCollided

let inline moveXYTogether a = moveWithBS bsDiffXYTogether a

let inline moveXYAnother a = moveWithBS bsDiffXYAnother a


let inline moveReflectable
    (gameSetting : GameSetting)
    (dungeonModel : Dungeon.DungeonModel)
    (diff0) (x : ^a)
    =
    let obj = ObjectBase.get x
    let area = obj |> ObjectBase.area
    let lu, rd = area |> Rect.get_LU_RD
    let ld, ru = lu + area.size * (Vec2.init 0.0f 1.0f), lu + area.size * (Vec2.init 1.0f 0.0f)

    let objectAreaPoints = [|lu; rd; ld; ru|]

    let isInside = GameSetting.insideDungeon gameSetting dungeonModel

    let isCollided =
        objectAreaPoints
        |>> ((+) diff0)
        |> isInside
        |> not

    let calcDiff f =
        f
            gameSetting.binarySearchCountMovingOnWall
            (fun newDiff ->
                objectAreaPoints
                |>> ((+) newDiff)
                |> isInside
            )
            diff0
            obj.position

    let diff, reflectedDir =
        if isCollided then
            let diffSlide = calcDiff bsDiffXYAnother

            let diff = calcDiff bsDiffXYTogether

            let tangent = Vector.normalize(diffSlide - diff)

            let reflectedDir =
                let dir = Vector.normalize diff
                (2.0f * (Vector.dot dir tangent) *. tangent) - dir

            let restLength = Vector.length diff - Vector.length diff0

            diff + ( reflectedDir .* (max restLength 10.0f) )
            , Some reflectedDir
        else
            diff0, None

    
    x
    |> ObjectBase.map (
        ObjectBase.mapPosition ((+) diff)
        >> setDirection (MoveDirection.fromVector diff0)
        >> fun x -> { x with isMoved = true }
    ), reflectedDir


let inline setVelocity velocity (obj : ObjectBase) =
    { obj with velocity = velocity }

let inline update (o : ^a) =
    o
    |> ObjectBase.mapPosition ( (+) (ObjectBase.velocity o))
    |> ObjectBase.map( fun x -> { x with isMoved = false } )