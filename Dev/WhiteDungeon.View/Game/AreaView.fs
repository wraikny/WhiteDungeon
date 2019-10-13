namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Math
open wraikny.Tart.Helper.Collections

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core
open wraikny.MilleFeuille.Objects
open WhiteDungeon.Core
open WhiteDungeon.View
open wraikny.MilleFeuille
open WhiteDungeon.View.Utils.Color

open FSharpPlus

type AreaView() =
    inherit asd.GeometryObject2D()

    let mutable lastPosition : float32 Vec2 = zero
    let mutable lastSize : float32 Vec2 = zero

    let sizeViewRect = new asd.RectangleShape()
    let sizeView =
        let enabled = false
        new asd.GeometryObject2D(
            Shape = sizeViewRect,
            Color = asd.Color(0, 0, 255, 50),
            IsUpdated = enabled,
            IsDrawn = enabled
        )

    let textureObj = new asd.TextureObject2D()

    do
        base.AddDrawnChild(
            textureObj,
            asd.ChildManagementMode.IsDrawn |||
            asd.ChildManagementMode.IsUpdated |||
            asd.ChildManagementMode.Disposal |||
            asd.ChildManagementMode.RegistrationToLayer,
            asd.ChildTransformingMode.All,
            asd.ChildDrawingMode.DrawingPriority
        )
        base.AddDrawnChildWithoutColor(sizeView)

    let mutable texViewSize = asd.Vector2DF()
    let onSetViewSize = Event<asd.Vector2DF>()

    member __.OnSetViewSize = onSetViewSize.Publish

    member this.SetTextureSize(texSize: asd.Vector2DF, areaHeight) =
        textureObj.CenterPosition <- texSize * asd.Vector2DF(0.5f, 1.0f)
        textureObj.Position <- asd.Vector2DF(0.0f, areaHeight * 0.5f)
        this.ViewSize <- texSize

    member private __.ViewSize
        with get() = texViewSize
        and set(x) =
            texViewSize <- x
            onSetViewSize.Trigger(x)

    member __.EnabledSizeView
        with get() = sizeView.IsDrawn
        and set(x) =
            sizeView.IsDrawn <- x
            sizeView.IsUpdated <- x

    member __.EnabledTextureView
        with get() = textureObj.IsDrawn
        and set(x) =
            textureObj.IsDrawn <- x
            textureObj.IsUpdated <- x

    member __.SizeView with get() = sizeView
    member __.TextureView with get() = textureObj

    //member __.LastSize with get() = lastSize

    member this.UpdateAreaView(area : float32 Rect2) =
        let lu, rd = Rect.lurd area
        let centerPos = (lu + rd) ./ 2.0f
        let bottom = Vec2.init centerPos.x rd.y
        lastPosition <- bottom
        
        // centerPos
        this.Position <- Vec2.toVector2DF (map floor centerPos) // .% GameViewSetting.modForCulling)

        this.DrawingPriority <- int bottom.y

        // Size
        lastSize <- area.size
        let size = Vec2.toVector2DF area.size
        sizeViewRect.DrawingArea <- asd.RectF(-size/2.0f, size)