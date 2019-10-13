namespace WhiteDungeon.View.Game

open wraikny
open Affogato
open Affogato.Collections
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

    member this.UpdateObjectBaseView(objectBaseView : ViewModel.ObjectBaseView) =
        // Position
        let area = objectBaseView |> Model.ObjectBase.area
        base.UpdateAreaView(area)

        let size = area.size |> Vector2.toVector2DF

        if base.TextureView.IsDrawn then
            let changedDir = moveAnimation.SetDirection(objectBaseView.direction)
            if changedDir || objectBaseView.isMoved then
                moveAnimation.Next()
            
                base.SetTextureSize(base.TextureView.Src.Size, size.Y)
        else
            base.SetTextureSize(size, size.Y)

    member __.SetAnimationTextures(textureKind) =
        if lastTextureKind <> Some textureKind then
            lastTextureKind <- Some textureKind

            imagesMap
            |> HashMap.tryFind textureKind
            |> iter moveAnimation.SetAnimationTextures