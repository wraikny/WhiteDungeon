﻿namespace WhiteDungeon.View.Game

open wraikny.Tart.Helper
open Affogato
open WhiteDungeon.View
open WhiteDungeon.Core.Model
open wraikny.MilleFeuille
open FSharpPlus


type MoveAnimation(owner : asd.TextureObject2D) =
    //let load =
    //    Seq.map(fun (path, area, angle) ->
    //        asd.Engine.Graphics.CreateTexture2D(path),
    //        (Rect.toRectI area).ToF(),
    //        angle
    //    )
    //    >> Seq.toList

    let mutable images = ActorImages.empty

    let mutable current = Back

    let mutable currentMove = ActorMove.Walk

    let anim =
        seq {
            let textures =
                images
                |> ActorImages.fromDirection current
            match textures with
            | [] ->
                ()
            | (tex, area, angle)::[] ->
                owner.Texture <- tex
                owner.Src <- area
                owner.Angle <- angle
                yield()
            | _ ->
                while true do
                    for (tex, area, angle) in textures do
                        owner.Texture <- tex
                        owner.Src <- area
                        owner.Angle <- angle
                        yield()

                        yield!(
                            match currentMove with
                            | Walk -> images.sleepWalk
                            | Dash -> images.sleepDash
                            |> int |> Coroutine.sleep
                        )
        }

    let mutable coroutine = anim.GetEnumerator()

    member __.Move with get() = currentMove and set(x) = currentMove <- x

    member __.SetAnimationTextures(images' : ActorImages<string, int Rectangle2>) =
        images <-
            images'
            |> ActorImages.map(fun (path, area, angle) ->
                asd.Engine.Graphics.CreateTexture2D(path),
                (Rectangle.toRectI area).ToF(),
                angle
            )
        

    member __.SetDirection(dir) =
        if current <> dir then
            current <- dir
            coroutine <- anim.GetEnumerator()
            true
        else
            false

    member __.Next() =
        if coroutine <> null then
            coroutine.MoveNext() |> ignore