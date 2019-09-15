namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core.View
open wraikny.MilleFeuille.Fs.Objects
open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.Geometry
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color

open FSharpPlus
open FSharpPlus.Math.Applicative

type SkillEmitView(_gameViewSetting : GameViewSetting) =
    inherit ObjectBaseView<unit>(HashMap.empty
        , EnabledSizeView = true
        , EnabledTextureView = false
    )

    let color = asd.Color(255, 0, 0, 200)
    do
        base.SizeView.Color <- color

    interface IUpdatee<Game.ViewModel.AreaSkillEmitView> with
        member this.Update(viewModel) =
            //this.SetFrame(viewModel.frameCurrent, viewModel.frameFirst)
            this.UpdateObjectBaseView(viewModel.baseView)

            this.DrawingPriority <-
                viewModel.baseView.Area
                |> Rect.centerPosition
                |> Vector.y
                |> int


    member private this.SetFrame(frameCurrent, frameFirst) =
        let alpha =
            (float32 frameCurrent) / (float32 frameFirst)
            |> (*) 255.0f
            |> byte

        this.SizeView.Color <- asd.Color(color.R, color.G, color.B, alpha)