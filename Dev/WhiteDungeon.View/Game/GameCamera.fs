namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core
open wraikny.Tart.Core.View
open wraikny.MilleFeuille.Fs.Objects
open WhiteDungeon.Core
open WhiteDungeon.View.Utils.Math

type GameCamera() =
    inherit asd.CameraObject2D(
        Dst = new asd.RectI(
            new asd.Vector2DI(0, 0)
            , asd.Engine.WindowSize
        )
    )


    let mutable srcPosition = Vec2.zero()

    member val Zoom = 1.0f with get, set

    interface IObserver<Main.ViewModel> with
        member this.UpdateFromNotify(viewModel) =
            viewModel |> function
            | Main.ViewModel.GameViewModel viewModel ->
                let cameraView = viewModel.camera |> List.head
                this.SetSrc(cameraView.position |> Vec2.map int)

            | _ -> ()


    member this.SetSrc(srcPos) =
        srcPosition <- srcPos

        let size = asd.Engine.WindowSize.To2DF() / this.Zoom
        let size = size.To2DI()

        this.Src <- new asd.RectI(
            (Vec2.toVector2DI srcPos) - size / 2
            , size
        )