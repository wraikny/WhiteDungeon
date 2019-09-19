namespace WhiteDungeon.View.Game

open wraikny.Tart.Math
open wraikny.Tart.Helper
open wraikny.Tart.Advanced
open WhiteDungeon.Core
open WhiteDungeon.View
open wraikny.MilleFeuille
open FSharpPlus

type BuildingView(_gameViewSetting : GameViewSetting) =
    inherit AreaView(EnabledSizeView = true)

    interface IUpdatee<Model.BuildingKind * float32 Rect2> with
        member __.Update(x) =
            let kind, area = x
            base.UpdateAreaView(area)
