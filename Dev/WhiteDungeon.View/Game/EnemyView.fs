namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Collections
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

type EnemyView(gameSetting : Model.GameSetting, gameViewSetting : GameViewSetting, hpLayer) =
    inherit ActorView<Model.EnemyKind>(HashMap.empty, hpLayer
        #if DEBUG
        , EnabledSizeView = true
        #endif
    )

    let mutable kind = ValueNone
    let mutable enemySetting = Unchecked.defaultof<Model.EnemySetting>
    let mutable angle = 0.0f

    let mutable lastLookAngle = 0.0f

    let visionArc =
        new asd.ArcShape(
            InnerDiameter = 0.0f,
            NumberOfCorners = 36,
            StartingCorner = 0
        )

    let visionObj =
        let enabled =
            #if DEBUG
            true
            #else
            false
            #endif

        new asd.GeometryObject2D(
            Shape = visionArc,
            Color = base.SizeView.Color,
            IsDrawn = enabled,
            IsUpdated = enabled
        )
    do
        base.AddDrawnChildWithoutColor(visionObj)

    let mutable count = 0.0f

    member __.EnabledVisionView
        with get() = visionObj.IsDrawn
        and set(x) = visionObj.IsDrawn <- x; visionObj.IsUpdated <- x

    interface IUpdatee<Game.ViewModel.EnemyView> with
        member this.Update(viewModel) =
            let currentKind = ValueSome viewModel.enemy.kind

            if kind <> currentKind then
                kind <- currentKind
                enemySetting <- gameSetting.enemySettings |> HashMap.find viewModel.enemy.kind

                visionArc.EndingCorner <-
                    int (enemySetting.visionAngleRate * float32 visionArc.NumberOfCorners)
                visionArc.OuterDiameter <- enemySetting.visionDistance
                visionArc.Angle <- 90.0f - enemySetting.visionAngleRate * 180.0f

            
            this.SetAnimationTextures(viewModel.enemy.kind)
            this.UpdateActorView(viewModel.actorView)

            if this.EnabledVisionView then
                let lookAngle = viewModel.enemy.lookingRadian

                if abs(lookAngle - lastLookAngle) > 0.001f then
                    lastLookAngle <- lookAngle
                    visionObj.Angle <- (lookAngle |> Angle.radianToDegree)

