module WhiteDungeon.App.Setting

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open wraikny.MilleFeuille
open wraikny.MilleFeuille.UI
open WhiteDungeon
open WhiteDungeon.Core.Model
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color

open WhiteDungeon.Core.Model

open WhiteDungeon.App.GameSetting
open WhiteDungeon.App.GameViewSetting

open FSharpPlus.Math.Applicative
open FSharpPlus

let appSetting : View.AppSetting = {
    windowSize = windowSize

    menuSceneSetting =
        let createButtonColor (col : byte Vec3) x y z =
            let c a = col |>> float32 |>> ( * ) a |> map byte |> Vec3.toColor
            {
                defaultColor = c x
                hoverColor = c y
                holdColor = c z
            }
            
        {
            backColor = white
            frameColor = Vec4.init ume.x ume.y ume.z 200uy
            buttonColor = createButtonColor sumire 1.0f 0.8f 0.6f
            inputColor = createButtonColor sakura 1.0f 0.8f 0.6f
            inputFocusColor = createButtonColor sakura 1.0f 1.0f 1.0f

            inputFont = textFontPath
            inputFontSize = 30
            textSize = 30
            inputFontColor = black

            windowWidthWRate = 0.8f
            window2MarginRate = 0.02f
            window2HeightRate = 0.9f

            bgm = "bgm/seirei_no_machi.ogg"

            #if !DEBUG
            titleFont = "Font/titleFont.aff"
            headerFont = "Font/headerFont.aff"
            #endif
        }

    gameViewSetting = gameViewSetting
    gameSetting = gameSetting
}
