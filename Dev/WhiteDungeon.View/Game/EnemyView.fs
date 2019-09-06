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

type EnemyView(gameSetting : Model.GameSetting, gameViewSetting : GameViewSetting) =
    inherit ActorView<Model.EnemyKind>(Map.empty
        #if DEBUG
        , EnabledSizeView = true
        #endif
    )

    let mutable kind = ValueNone

    let mutable enemySetting = Unchecked.defaultof<_>

    interface IUpdatee<Game.ViewModel.EnemyView> with
        member this.Update(viewModel) =
            //this.SetAnimationTextures()
            this.UpdateActorView(viewModel.actorView)

            let currentKind = ValueSome viewModel.kind
            if kind <> currentKind then
                kind <- currentKind

                enemySetting <- gameSetting.enemySettings |> Map.find viewModel.kind