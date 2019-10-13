namespace WhiteDungeon.View.Game

open Affogato
open wraikny.Tart.Helper

open WhiteDungeon.Core
open WhiteDungeon.View
open wraikny.MilleFeuille
open FSharpPlus


type PlayerView(gameViewSetting, imagesMap, hpLayer) =
    // inherit asd.GeometryObject2D(Color = ColorPalette.sumire)
    inherit ActorView<Model.Occupation>(
        gameViewSetting, imagesMap, hpLayer
        #if DEBUG
        , EnabledSizeView = true
        #endif
    )

    interface IUpdatee<ViewModel.PlayerView> with
        member this.Update(viewModel) =
            this.SetAnimationTextures(viewModel.character.currentOccupation)
            this.UpdateActorView(viewModel.actor)