namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Core.View
open wraikny.MilleFeuille.Fs.Objects
open WhiteDungeon.Core
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Math
open WhiteDungeon.View.Utils.Color


type PlayerView() =
    inherit asd.GeometryObject2D(
        Color = ColorPalette.sumire
    )

    let mutable lastSize = Vec2.zero()
    let mutable lastPosition = Vec2.zero()

    let rect = new asd.RectangleShape()

    interface IObjectUpdatee<Game.ViewModel.PlayerView> with
        member this.Update(viewModel) =
            let objectBase = viewModel.actorView.objectBaseView

            let area = objectBase.area

            if area.position <> lastPosition then
                this.SetPosition(area.position)

            if area.size <> lastSize then
                this.SetSize(area.size)


    override this.OnAdded() =
        this.Shape <- rect
        

    member this.SetPosition(pos) =
        lastPosition <- pos
        this.Position <- Vec2.toVector2DF pos

    member this.SetSize(size) =
        let size = Vec2.toVector2DF size
        rect.DrawingArea <- new asd.RectF(new asd.Vector2DF(0.0f, 0.0f), size)