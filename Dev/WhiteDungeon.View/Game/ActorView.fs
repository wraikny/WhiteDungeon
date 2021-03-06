﻿namespace WhiteDungeon.View.Game

open wraikny
open Affogato

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core
open wraikny.MilleFeuille.Objects
open WhiteDungeon.Core
open WhiteDungeon.View
open wraikny.MilleFeuille
open WhiteDungeon.View.Utils.Color

open FSharpPlus
open FSharpPlus.Math.Applicative

type ActorView< 'a
    when 'a : equality
    and 'a : comparison
    >(gameViewSetting : GameViewSetting, imagesMap, hpLayer : asd.Layer2D) as this =
    inherit ObjectBaseView<'a>(imagesMap)

    let font =
        let v = gameViewSetting.actorGameText
        asd.Engine.Graphics.CreateDynamicFont(
            v.font,
            v.size, Vector3.toColor v.color,
            v.sizeOutline, Vector3.toColor v.colorOutline
        )

    let levelText =
        let size = font.HorizontalSize("Lv.1234567890")
        new asd.TextObject2D(Font = font, Position = asd.Vector2DF(0.0f, float32 -size.Y - 5.0f))

    let hpWidth = 10.0f
    let hpRect = new asd.RectangleShape()
    let hpObj =
        new asd.GeometryObject2D(
            Shape = hpRect,
            Color = asd.Color(0uy, 255uy, 0uy),
            IsUpdated = false
        )

    do
        this.AddDrawnChild(hpObj,
            enum<asd.ChildManagementMode> 0b1110,
            asd.ChildTransformingMode.All,
            asd.ChildDrawingMode.Nothing
        )

        hpObj.AddDrawnChildWithoutColor(levelText)

    let mutable lastWidth = 0.0f
    let mutable lastRate = 0.0f
    let mutable lastLevel = 0us

    let updateHPBarRate(rate) =
        hpRect.DrawingArea <- asd.RectF(asd.Vector2DF(0.0f, 0.0f), asd.Vector2DF(rate * lastWidth, -hpWidth))
    
    do
        base.OnSetViewSize.Add(fun size ->
            hpObj.Position <- asd.Vector2DF(-size.X * 0.5f, -size.Y)
            updateHPBarRate(lastRate)
            lastWidth <- size.X
        )

    override __.OnAdded() =
        base.OnAdded()
        hpLayer.AddObject(hpObj)

    member this.UpdateActorView(actorView : ViewModel.ActorView) =
        this.UpdateObjectBaseView(actorView.objectBase)
        this.UpdateHPBar(actorView)

    member __.UpdateHPBar(actor : ViewModel.ActorView) =
        let rate = actor.HPRate()
        if rate <> lastRate then
            lastRate <- rate
            updateHPBarRate(lastRate)

        if lastLevel <> actor.level then
            lastLevel <- actor.level
            levelText.Text <- sprintf "Lv.%d" actor.level

