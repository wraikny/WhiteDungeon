﻿namespace WhiteDungeon.View.Utils

open wraikny.MilleFeuille.Core

open FSharpPlus

type BGMPlayer<'T when 'T :> asd.Scene>(name, bgmList : string list) =
    inherit SceneComponent<'T>(name)

    do
        if length bgmList = 0 then
            invalidArg "bgmList" "is empty"

    let sources =
        bgmList
        |>> fun path -> asd.Engine.Sound.CreateSoundSource(path, false)

    let count = length sources
    do
        for source in sources do
            source.IsLoopingMode <- true

    let mutable volume = 1.0f
    let mutable fadeSeconds = ValueNone

    let mutable bgmId = ValueNone
    let mutable index = 0

    
    let play() =
        let id = asd.Engine.Sound.Play sources.[index]

        fadeSeconds
        |> ValueOption.iter (curry asd.Engine.Sound.FadeIn id)

        asd.Engine.Sound.SetVolume(id, volume)

        bgmId <- ValueSome id
        index <- (index + 1) % count

    let update() =
        bgmId
        |> function
        | ValueSome id ->
            if not <| asd.Engine.Sound.GetIsPlaying(id) then
                play()
        | ValueNone -> ()

    do
        base.add_OnUpdatedEvent(fun _ -> update())
        base.add_OnDisposedEvent(fun _ ->
            
            (bgmId, fadeSeconds) |> function
            | ValueSome id, ValueSome fade ->
                asd.Engine.Sound.FadeOut(id, fade)
            | ValueSome id, _ ->
                asd.Engine.Sound.Stop(id)
            | _ -> ()
            bgmId <- ValueNone
        )

    member __.Start() =
        bgmId |> function
        | ValueNone -> play()
        | _ -> invalidOp "BGM has already been played!"

    member __.Pause() =
        bgmId |> ValueOption.iter (asd.Engine.Sound.Pause)

    member __.Resume() =
        bgmId |> ValueOption.iter (asd.Engine.Sound.Resume)

    member __.FadeOut(sec) =
        bgmId
        |> ValueOption.iter(fun id ->
            asd.Engine.Sound.FadeOut(id, sec))
        bgmId <- ValueNone
        

    member __.FadeSeconds
        with get() = ValueOption.defaultValue 0.0f fadeSeconds
        and set(x) = fadeSeconds <- ValueSome x

    member __.Volume
        with get() = volume
        and set(x) =
            volume <- x
            bgmId |> ValueOption.iter(fun id ->
                asd.Engine.Sound.SetVolume(id, x))