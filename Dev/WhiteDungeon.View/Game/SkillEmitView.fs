namespace WhiteDungeon.View.Game

open Affogato
open Affogato.Collections
open wraikny.Tart.Helper
open wraikny.MilleFeuille
open WhiteDungeon.Core
open WhiteDungeon.View

type SkillEmitView(_gameViewSetting : GameViewSetting) =
    inherit ObjectBaseView<unit>(HashMap.empty
        , EnabledSizeView = true
        , EnabledTextureView = false
    )

    let color = asd.Color(255, 0, 0, 200)
    do
        base.SizeView.Color <- color

    interface IUpdatee<ViewModel.AreaSkillEmitView> with
        member this.Update(viewModel) =
            //this.SetFrame(viewModel.frameCurrent, viewModel.frameFirst)
            this.UpdateObjectBaseView(viewModel.objectBase)

            this.DrawingPriority <-
                viewModel.objectBase.Area
                |> Rectangle.centerPosition
                |> Vector.y
                |> int


    member private this.SetFrame(frameCurrent, frameFirst) =
        let alpha =
            (float32 frameCurrent) / (float32 frameFirst)
            |> (*) 255.0f
            |> byte

        this.SizeView.Color <- asd.Color(color.R, color.G, color.B, alpha)