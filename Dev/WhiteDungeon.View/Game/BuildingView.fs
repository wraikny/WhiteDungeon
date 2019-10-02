namespace WhiteDungeon.View.Game

open wraikny.Tart.Math
open wraikny.Tart.Helper
open wraikny.Tart.Advanced
open WhiteDungeon.Core
open WhiteDungeon.View
open wraikny.MilleFeuille
open FSharpPlus

type BuildingView(gameViewSetting : GameViewSetting) =
    inherit AreaView(EnabledSizeView = true)

    let mutable lastKind = ValueNone

    interface IUpdatee<Model.BuildingKind * float32 Rect2> with
        member __.Update(x) =
            let kind, area = x
            let vk = ValueSome kind
            if vk <> lastKind then
                lastKind <- vk
                let texturePath, textureArea =
                    gameViewSetting.buildingTextuers |> HashMap.find kind
                base.TextureView.Texture <-
                    asd.Engine.Graphics.CreateTexture2D(texturePath)
                base.TextureView.Src <-
                    textureArea
                    |>> map float32
                    |> Rect.toRectF
                base.SetTextureSize(base.TextureView.Src.Size, area.size.y)
                
            base.UpdateAreaView(area)
