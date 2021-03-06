﻿namespace WhiteDungeon.View.Game

open wraikny.Tart.Helper
open Affogato
open wraikny.Tart.Core
open wraikny.MilleFeuille
open wraikny.MilleFeuille.Objects
open WhiteDungeon.Core
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color


open FSharpPlus
open FSharpPlus.Math.Applicative

open System.Collections.Generic

type DamagesView(gameViewSetting : GameViewSetting) =
    inherit asd.Layer2DComponent()

    let mutable objectsCount = 0u
    let objects = Stack<asd.TextObject2D>()

    let remove (o : asd.TextObject2D) =
        objectsCount <- objectsCount + 1u
        o.IsDrawn <- false
        objects.Push(o)

    let createFont (v : FontSetting) =
        asd.Engine.Graphics.CreateDynamicFont(
            v.font,
            v.size, Vector3.toColor v.color,
            v.sizeOutline, Vector3.toColor v.colorOutline
        )

    let damageFont = createFont gameViewSetting.damageView
    let healFont = createFont gameViewSetting.healView

    let rand = System.Random()

    let mutable cameraPosition = asd.Vector2DF()

    member __.OnNext(pos) =
        cameraPosition <- Vector2.toVector2DF pos

    member this.Add(damages : (float32 Vector2 * float32) []) =
        for (pos, damage) in damages do
            let obj =
                if objectsCount = 0u then
                    let obj = new asd.TextObject2D()
                    this.Owner.AddObject(obj)
                    obj
                else
                    objectsCount <- objectsCount - 1u
                    let obj = objects.Pop()
                    obj.IsDrawn <- true
                    obj

            obj.Font <- if damage >= 0.0f then damageFont else healFont
            obj.Text <- sprintf "%d" (int damage)

            let rnd =
                let angle = (float32 <| rand.NextDouble()) * 260.0f
                asd.Vector2DF(1.0f, 0.0f, Degree = angle)
                * float32 gameViewSetting.damageView.size * 2.0f
            let position = Vector2.toVector2DF pos + rnd

            obj.Position <- position - cameraPosition

            obj.AddCoroutineAsParallel(seq {

                let frame = gameViewSetting.damageTextFrame - 1
                for i in 0..frame ->
                    let alpha = 1.0f - Easing.calculate Easing.InSine frame i
                    let alpha = int(alpha * 255.0f)
                    obj.Color <- asd.Color(255, 255, 255, alpha)

                    let y =
                        -(Easing.calculate Easing.Linear frame i)
                        * gameViewSetting.damageTextMove

                    obj.Position <- position + asd.Vector2DF(0.0f, y) - cameraPosition
                remove(obj)
                yield()
            })
