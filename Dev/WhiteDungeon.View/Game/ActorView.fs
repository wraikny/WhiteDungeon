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
    >(gameViewSetting : GameViewSetting, imagesMap : Map<_, _>) =
    inherit asd.GeometryObject2D()

    let mutable lastPosition : float32 Vec2 = zero
    let mutable lastSize : float32 Vec2 = zero
    let mutable lastTextureKind : 'a option = None

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

    member __.SizeView
        with get() = sizeView.IsDrawn
        and set(x) =
            sizeView.IsDrawn <- x
            sizeView.IsUpdated <- x

    member this.UpdateActorView(actorView : ViewModel.ActorView) =
        let objectBase = actorView.objectBaseView
        // Position
        let area = objectBase.area
        let lu, rd = Rect.get_LU_RD area
        let centerPos = (lu + rd) ./ 2.0f
        let bottom = Vec2.init centerPos.x rd.y
        lastPosition <- bottom
        
        this.Position <- Vec2.toVector2DF (map floor centerPos .% GameViewSetting.modForCulling)


        // Size
        lastSize <- area.size
        let size = Vec2.toVector2DF area.size
        sizeViewRect.DrawingArea <- asd.RectF(-size/2.0f, size)


        let changedDir = moveAnimation.SetDirection(objectBase.direction)

        if changedDir || objectBase.isMoved || textureOjb.Texture = null then
            moveAnimation.Next()
            
            let texSize = textureOjb.Src.Size
            let scale = size.X / texSize.X
            textureOjb.Scale <- scale * asd.Vector2DF(1.0f, 1.0f)
            textureOjb.CenterPosition <- texSize * asd.Vector2DF(0.5f, 1.0f)
            textureOjb.Position <- asd.Vector2DF(0.0f, size.Y * 0.5f)

    member __.SetAnimationTextures(textureKind) =
        if lastTextureKind <> Some textureKind then
            lastTextureKind <- Some textureKind

            imagesMap
            |> Map.find textureKind
            |> moveAnimation.SetAnimationTextures