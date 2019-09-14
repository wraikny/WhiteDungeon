namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper
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
open wraikny.MilleFeuille.Fs.Component.Coroutine
open wraikny.MilleFeuille.Fs.Geometry
open WhiteDungeon.View.Utils.Color

open FSharpPlus
open FSharpPlus.Math.Applicative

type EnemyView(gameSetting : Model.GameSetting, gameViewSetting : GameViewSetting, hpLayer) = // as this =
    inherit ActorView<Model.EnemyKind>(gameViewSetting, HashMap.empty, hpLayer)

    let mutable kind = ValueNone
    let mutable enemySetting = Unchecked.defaultof<Model.EnemySetting>

    let visionArc = new asd.ArcShape(NumberOfCorners = 36)

    let visionObj =
        new asd.GeometryObject2D(
            Shape = visionArc,
            Color = asd.Color(255, 255, 0, int base.SizeView.Color.A),
            IsDrawn = false,
            IsUpdated = false
        )
    do
        base.AddDrawnChildWithoutColor(visionObj)

        #if DEBUG
        base.EnabledTextureView <- false
        base.EnabledSizeView <- true
        visionObj.IsDrawn <- true
        #endif

    //do
    //    this.StartCoroutine("position", seq {
    //        while true do
    //            yield! Coroutine.sleep 60
    //            let pos = this.Position
    //            printfn "%f, %f" pos.X pos.Y
    //            yield()
    //    })

    member __.EnabledVisionView
        with get() = visionObj.IsDrawn
        and set(x) = visionObj.IsDrawn <- x //; visionObj.IsUpdated <- x

    interface IUpdatee<Game.ViewModel.EnemyView> with
        member this.Update(viewModel) =
            let currentKind = ValueSome viewModel.kind

            if kind <> currentKind then
                kind <- currentKind
                enemySetting <- gameSetting.enemySettings |> HashMap.find viewModel.kind

                visionArc.EndingCorner <- int (
                    enemySetting.visionAngleRate * float32 visionArc.NumberOfCorners )

                visionArc.OuterDiameter <- enemySetting.visionDistance * 2.0f
                visionArc.Angle <- 90.0f - enemySetting.visionAngleRate * 180.0f

            
            this.SetAnimationTextures(viewModel.kind)
            this.UpdateActorView(viewModel.actor)

            if this.EnabledVisionView then
                let lookAngle = viewModel.lookingRadian

                visionObj.Angle <- (lookAngle |> Angle.radianToDegree)

