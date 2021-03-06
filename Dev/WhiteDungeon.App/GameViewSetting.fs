﻿module WhiteDungeon.App.GameViewSetting

open Affogato
open Affogato.Collections


open wraikny.MilleFeuille
open wraikny.MilleFeuille.UI
open WhiteDungeon
open WhiteDungeon.Core.Model

open WhiteDungeon.View
open FSharpPlus.Math.Applicative
open FSharpPlus



let black = Vector3.init 0uy 0uy 0uy
let white = Vector3.init 255uy 255uy 255uy

let red = Vector3 .init 255uy 0uy 0uy
let green = Vector3.init 0uy 255uy 0uy

let ume = Vector3.init 234uy 173uy 189uy
let sumire = Vector3.init 85uy 69uy 98uy
let sakura = Vector3.init 250uy 219uy 224uy

let windowSize = (Vector2.init 16 9) .* 75

let windowRSizeUnit = (map float32 windowSize) ./ float32 windowSize.x

let textFontPath = "Font/mplus-1c-light.ttf"

let damageView = {
    font = "Font/mplus-1p-bold.ttf"
    size = 60
    sizeOutline = 5
    color = red
    colorOutline = white
}

let gameViewSetting : View.GameViewSetting =
    let windowMargin = 0.02f /. windowRSizeUnit
    let floorSize = (Vector2.init 0.15f 0.03f) / windowRSizeUnit
    let playerStatusSize = (Vector2.init 0.2f 0.15f)

    {
        //occupationImages = [
        //    Model.Seeker, ActorImages.fromGraphicmaker 8u 4u "Image/Game/Occupation/hunter.png"
        //    Model.Bushi, Character.Bushi.images
        //] |> Map.ofList
        occupationSetting = [
            Character.Bushi.viewSetting
            Character.Onmyoji.viewSetting
        ]
        |> Seq.map(fun x -> (x.name, x))
        |> HashMap.ofSeq

        buildingTextuers = HashMap.ofList [
             Gate, ("Image/Game/building/torii.png", Rectangle.init (Vector2.init 0 0) (Vector2.init 512 512))
        ]

        bgms = [
            "bgm/gensei_no_rakuen.ogg"
            "bgm/buriki_no_coffee_maker.ogg"
        ]

        gameUIFrameColor = Vector4.init sumire.x sumire.y sumire.z 240uy
        gameUITextColor = sakura
        gameUITextFont = textFontPath
        gameUITextSize = 20
        gameUIDungeonFloor =
            Rectangle.init
                ( Vector2.init (1.0f - windowMargin.x - floorSize.x) windowMargin.y )
                floorSize
        gameUIPlayerArea =
            Rectangle.init
                ( Vector2.init windowMargin.x (1.0f - windowMargin.y) )
                playerStatusSize

        dungeonCellTexture = "Image/Game/cotton-c.png"

        damageTextFrame = 120
        damageTextMove = 50.0f
        damageView = damageView
        healView = {
            damageView with
                color = green
                colorOutline = white
        }

        actorGameText = {
            font = textFontPath
            size = 50
            sizeOutline = 3
            color = black
            colorOutline = black
        }
    }