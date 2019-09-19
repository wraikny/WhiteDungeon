namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Math

open wraikny.Tart.Helper
open wraikny.Tart.Core
open wraikny.MilleFeuille
open WhiteDungeon.Core
open WhiteDungeon.View
open wraikny.MilleFeuille
open WhiteDungeon.View.Utils.Color

open FSharpPlus
open FSharpPlus.Math.Applicative


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