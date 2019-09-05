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

type ActorView< 'a
    when 'a : equality
    and 'a : comparison
    >(imagesMap : Map<_, _>) =
    inherit ObjectBaseView<'a>(imagesMap)

    member this.UpdateActorView(actorView : ViewModel.ActorView) =
        this.UpdateObjectBaseView(actorView.objectBaseView)