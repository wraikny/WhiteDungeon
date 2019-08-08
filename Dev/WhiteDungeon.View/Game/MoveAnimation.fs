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


type MoveAnimation(owner : asd.TextureObject2D) =
    let load =
        Seq.map(fun (path, area, angle) ->
            asd.Engine.Graphics.CreateTexture2D(path),
            (Rect.toRectI area).ToF(),
            angle
        )
        >> Seq.toList

    let mutable images = ActorImages.empty

    let mutable current = Front

    let mutable sleepFrame = 0u

    let anim =
        seq {
            let textures =
                images
                |> ActorImages.fromDirection current

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

    member __.SetAnimationTextures(images' : ActorImages<string, int Rect2>) =
        images <-
            images'
            |> ActorImages.map(fun (path, area, angle) ->
                asd.Engine.Graphics.CreateTexture2D(path),
                (Rect.toRectI area).ToF(),
                angle
            )
        

    member __.SetDirection(dir) =
        if current <> dir then
            current <- dir
            coroutine <- anim.GetEnumerator()

    member __.Next() =
        if coroutine <> null then
            coroutine.MoveNext() |> ignore