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


type PlayerView(gameViewSetting) =
    // inherit asd.GeometryObject2D(Color = ColorPalette.sumire)
    inherit asd.GeometryObject2D()

    let gameViewSetting : GameViewSetting = gameViewSetting

    let mutable lastPosition = zero
    let mutable lastSize = zero
    let mutable lastOccupation = ValueNone

    let textureOjb = new asd.TextureObject2D()

    let sizeViewRect = new asd.RectangleShape()
    let sizeView =
        let enabled = false
        new asd.GeometryObject2D(
            Shape = sizeViewRect,
            Color = asd.Color(0, 0, 255, 50),
            IsUpdated = enabled,
            IsDrawn = enabled
        )
    do
        base.AddDrawnChild(
            textureOjb,
            asd.ChildManagementMode.IsDrawn |||
            asd.ChildManagementMode.IsUpdated |||
            asd.ChildManagementMode.Disposal |||
            asd.ChildManagementMode.RegistrationToLayer,
            asd.ChildTransformingMode.All,
            asd.ChildDrawingMode.DrawingPriority
        )
        base.AddDrawnChildWithoutColor(sizeView)

    let moveAnimation = MoveAnimation(textureOjb)

#if DEBUG
    override this.OnUpdate() =
        if asd.Engine.Keyboard.GetKeyState asd.Keys.Num4 = asd.ButtonState.Push then
            printfn "lastPosition: %A" lastPosition
        ()
#endif


    member __.SizeView
        with get() = sizeView.IsDrawn
        and set(x) =
            sizeView.IsDrawn <- x
            sizeView.IsUpdated <- x

    interface IUpdatee<Game.ViewModel.PlayerView> with
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
            sizeViewRect.DrawingArea <- asd.RectF(-size/2.0f, size)

            this.SetOccupation(viewModel.character.currentOccupation)

            let changedDir = moveAnimation.SetDirection(objectBase.direction)

            if changedDir || objectBase.isMoved || textureOjb.Texture = null then
                moveAnimation.Next()

                let texSize = textureOjb.Src.Size
                let scale = size.X / texSize.X
                textureOjb.Scale <- scale * asd.Vector2DF(1.0f, 1.0f)

                textureOjb.Position <- -texSize * scale / 2.0f



    member __.SetOccupation(occupation) =
        if lastOccupation <> ValueSome occupation then
            lastOccupation <- ValueSome occupation

            gameViewSetting.occupationImages
            |> Map.find occupation
            |> moveAnimation.SetAnimationTextures