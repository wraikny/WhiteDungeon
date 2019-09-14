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
    >(imagesMap, hpLayer : asd.Layer2D) =
    inherit ObjectBaseView<'a>(imagesMap)

    let hpWidth = 10.0f

    let hpRect = new asd.RectangleShape()
    let hpObj =
        new asd.GeometryObject2D(
            Shape = hpRect,
            Color = asd.Color(0uy, 255uy, 0uy),
            IsUpdated = false
        )
    do
        base.AddDrawnChild(hpObj,
            asd.ChildManagementMode.IsDrawn,
            asd.ChildTransformingMode.All,
            asd.ChildDrawingMode.Nothing
        )

    override __.OnAdded() =
        base.OnAdded()
        hpLayer.AddObject(hpObj)


    member this.UpdateActorView(actorView : ViewModel.ActorView) =
        this.UpdateObjectBaseView(actorView.objectBase)
        this.UpdateHPBar(actorView)

    member __.UpdateHPBar(actorView : ViewModel.ActorView) =
        let rate = actorView.HPRate()
        let size = base.ViewSize
        hpObj.Position <- asd.Vector2DF(-size.X, -size.Y)
        hpRect.DrawingArea <- asd.RectF(asd.Vector2DF(0.0f, 0.0f), asd.Vector2DF(rate * size.X * 2.0f, -hpWidth))
        ()