namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper
open wraikny.Tart.Helper.Utils
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core.View
open wraikny.MilleFeuille.Core
open wraikny.MilleFeuille.Fs
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.Geometry
open WhiteDungeon.View.Utils.Color
open WhiteDungeon.Core.Game.Model

open FSharpPlus

type TexParam = string * int Rect2 * float32


type AnimationTextures =
    {
        front : TexParam list
        back : TexParam list
        right : TexParam list
        left : TexParam list
        frontRight : TexParam list
        frontLeft : TexParam list
        backRight : TexParam list
        backLeft : TexParam list
    }

module AnimationTextures =
    let empty =
        {
            front = []
            back = []
            right = []
            left = []
            frontRight = []
            frontLeft = []
            backRight = []
            backLeft = []
        }


type MoveAnimation(owner : asd.TextureObject2D) =
    let load =
        Seq.map(fun (path, area, angle) ->
            asd.Engine.Graphics.CreateTexture2D(path),
            (Rect.toRectI area).ToF(),
            angle
        )
        >> Seq.toList

    let mutable texturesMap = Map.empty

    let mutable current = Front

    let mutable sleepFrame = 0u

    let anim =
        seq {
            match texturesMap |> Map.tryFind current with
            | None -> ()
            | Some textures ->
                while true do
                    for (tex, area, angle) in textures do
                        owner.Texture <- tex
                        owner.Src <- area
                        owner.Angle <- angle
                        yield()

                        yield! Coroutine.sleep(int sleepFrame)
        }

    let mutable coroutine = anim.GetEnumerator()

    member __.SleepFrame
        with get() = sleepFrame
        and set(x) = sleepFrame <- x

    member __.SetAnimationTextures(textures : AnimationTextures) =
        texturesMap <-
            [
                Front, load textures.front
                Back, load textures.back
                Right, load textures.right
                Left, load textures.left
                FrontRight, load textures.frontRight
                FrontLeft, load textures.frontLeft
                BackRight, load textures.backRight
                BackLeft, load textures.backLeft

            ] |> Map.ofList
        

    member __.SetDirection(dir) =
        if current <> dir then
            current <- dir
            coroutine <- anim.GetEnumerator()

    member __.Next() =
        if coroutine <> null then
            coroutine.MoveNext() |> ignore