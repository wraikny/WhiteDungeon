namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Core.View
open wraikny.MilleFeuille.Fs.Objects
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Math
open WhiteDungeon.View.Utils.Color

type SkillEmitView(gameViewSetting) =
    inherit asd.GeometryObject2D()

    let rect = new asd.RectangleShape()
    let mutable lastPosition = Vec2.zero()
    let mutable lastSize = Vec2.zero()

    let mutable lastFrameCurrent = 0u
    let mutable lastFrameFirst = 0u

    let mutable lastColor = Vec4.init(255uy, 0uy, 0uy, 255uy)

    override this.OnAdded() =
        this.Shape <- rect
        this.Color <- lastColor |> Vec4.toColor


    interface IUpdatee<Game.ViewModel.AreaSkillEmitView> with
        member this.Update(viewModel) =
            let objectBase = viewModel.baseView
            
            let area = objectBase.area
            
            this.SetPosition(area.position)
            
            this.SetSize(area.size)

            this.SetFrame(viewModel.frameCurrent, viewModel.frameFirst)

    member this.SetFrame(frameCurrent, frameFirst) =
        if lastFrameCurrent <> frameCurrent then
            lastFrameCurrent <- frameCurrent

        if lastFrameFirst <> frameFirst then
            lastFrameFirst <- frameFirst

        lastColor <- {
            lastColor with
                w =
                    (float32 frameCurrent) / (float32 frameFirst)
                    |> (*) 255.0f
                    |> byte
            }

        this.Color <- lastColor |> Vec4.toColor


    member this.SetPosition(pos) =
        if pos <> lastPosition then
            lastPosition <- pos
            this.Position <- Vec2.toVector2DF pos

    member this.SetSize(size) =
        if size <> lastSize then
            lastSize <- size
            rect.DrawingArea <-
                new asd.RectF(
                    new asd.Vector2DF(),
                    Vec2.toVector2DF size
                )