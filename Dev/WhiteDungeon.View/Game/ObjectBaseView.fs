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
open FSharpPlus.Math.Applicative

type ObjectBaseView< 'a
    when 'a : equality
    and 'a : comparison
    >(imagesMap : HashMap<_, ActorImages<_, _>>) =
    inherit AreaView()

    let mutable lastTextureKind : 'a option = None

    let moveAnimation = MoveAnimation(base.TextureView)

    let mutable texViewSize = asd.Vector2DF()

    let onSetViewSize = Event<asd.Vector2DF>()

    member __.OnSetViewSize = onSetViewSize.Publish

    member private __.ViewSize
        with get() = texViewSize
        and set(x) =
            texViewSize <- x
            onSetViewSize.Trigger(x)

    member this.UpdateObjectBaseView(objectBaseView : ViewModel.ObjectBaseView) =
        // Position
        let area = objectBaseView |> Model.ObjectBase.area
        base.UpdateAreaView(area)

        let size = area.size |> Vec2.toVector2DF

        if base.TextureView.IsDrawn then
            let changedDir = moveAnimation.SetDirection(objectBaseView.direction)
            if changedDir || objectBaseView.isMoved then
                moveAnimation.Next()
            
                let texSize = base.TextureView.Src.Size
                //let scale = 1.0f // size.X / texSize.X
                //textureObj.Scale <- scale * asd.Vector2DF(1.0f, 1.0f)
                base.TextureView.CenterPosition <- texSize * asd.Vector2DF(0.5f, 1.0f)
                base.TextureView.Position <- asd.Vector2DF(0.0f, size.Y * 0.5f)
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