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

type EnemyView(gameViewSetting) =
    inherit asd.GeometryObject2D()

    let gameViewSetting : GameViewSetting = gameViewSetting
    
    let mutable lastPosition = zero
    let mutable lastSize = zero

    let rect = new asd.RectangleShape()
    do
        base.Shape <- rect

    interface IUpdatee<Game.ViewModel.EnemyView> with
        member this.Update(viewModel) =
            let objectBase = viewModel.actorView.objectBaseView
            
            // Position
            let area = objectBase.area
            let centerPos = Rect.centerPosition area
            lastPosition <- centerPos
            
            this.Position <- Vec2.toVector2DF (map floor centerPos .% GameViewSetting.modForCulling)

            // Size
            lastSize <- area.size
            let size = Vec2.toVector2DF area.size

            rect.DrawingArea <- asd.RectF(-size/2.0f, size)