﻿namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Collections

open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core
open wraikny.MilleFeuille.Objects
open WhiteDungeon.Core
open WhiteDungeon.View
open wraikny.MilleFeuille
open WhiteDungeon.View.Utils.Color

open FSharpPlus
open FSharpPlus.Math.Applicative

type ObjectBaseView< 'a
    when 'a : equality
    and 'a : comparison
    >(imagesMap : HashMap<_, ActorImages<_, _>>) =
    inherit asd.GeometryObject2D()

    let mutable lastPosition : float32 Vec2 = zero
    let mutable lastSize : float32 Vec2 = zero
    let mutable lastTextureKind : 'a option = None

    let textureObj = new asd.TextureObject2D()
    
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
            textureObj,
            asd.ChildManagementMode.IsDrawn |||
            asd.ChildManagementMode.IsUpdated |||
            asd.ChildManagementMode.Disposal |||
            asd.ChildManagementMode.RegistrationToLayer,
            asd.ChildTransformingMode.All,
            asd.ChildDrawingMode.DrawingPriority
        )
        base.AddDrawnChildWithoutColor(sizeView)
    
    let moveAnimation = MoveAnimation(textureObj)

    let mutable texViewSize = asd.Vector2DF()

    let onSetViewSize = Event<asd.Vector2DF>()

    member __.OnSetViewSize = onSetViewSize.Publish

    member __.SizeView with get() = sizeView
    member __.TextureView with get() = textureObj

    member __.LastSize with get() = lastSize
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

    member this.UpdateObjectBaseView(objectBaseView : ViewModel.ObjectBaseView) =
        // Position
        let area = objectBaseView |> Model.ObjectBase.area
        let lu, rd = Rect.get_LU_RD area
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

        if textureObj.IsDrawn then
            let changedDir = moveAnimation.SetDirection(objectBaseView.direction)
            if changedDir || objectBaseView.isMoved then
                moveAnimation.Next()
            
                let texSize = textureObj.Src.Size
                //let scale = 1.0f // size.X / texSize.X
                //textureObj.Scale <- scale * asd.Vector2DF(1.0f, 1.0f)
                textureObj.CenterPosition <- texSize * asd.Vector2DF(0.5f, 1.0f)
                textureObj.Position <- asd.Vector2DF(0.0f, size.Y * 0.5f)
                // TODO
                this.ViewSize <- texSize * asd.Vector2DF(0.5f, 1.0f) // * scale
        else
            this.ViewSize <- size

    member __.SetAnimationTextures(textureKind) =
        if lastTextureKind <> Some textureKind then
            lastTextureKind <- Some textureKind

            imagesMap
            |> HashMap.tryFind textureKind
            |> iter moveAnimation.SetAnimationTextures