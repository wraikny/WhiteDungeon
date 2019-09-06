namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core.View
open wraikny.MilleFeuille.Fs.Objects
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open wraikny.MilleFeuille.Core
open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.Geometry
open WhiteDungeon.View.Utils.Color

open FSharpPlus
open FSharpPlus.Math.Applicative


type PlayerView(imagesMap) =
    // inherit asd.GeometryObject2D(Color = ColorPalette.sumire)
    inherit ActorView<Model.Occupation>(
        imagesMap
        #if DEBUG
        , EnabledSizeView = true
        #endif
    )

    interface IUpdatee<Game.ViewModel.PlayerView> with
        member this.Update(viewModel) =
            this.SetAnimationTextures(viewModel.character.currentOccupation)
            this.UpdateActorView(viewModel.actorView)