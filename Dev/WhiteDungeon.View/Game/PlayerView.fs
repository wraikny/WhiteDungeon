namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core.View
open wraikny.MilleFeuille.Fs.Objects
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.Geometry
open WhiteDungeon.View.Utils.Color

open FSharpPlus


type PlayerView(gameViewSetting) as this =
    // inherit asd.GeometryObject2D(Color = ColorPalette.sumire)
    inherit asd.TextureObject2D()

    let gameViewSetting : GameViewSetting = gameViewSetting


    let mutable lastPosition = zero
    let mutable lastSize = zero
    let mutable lastDirection = Model.MoveDirection.Front
    let mutable lastOccupation = None

    let moveAnimation = MoveAnimation(this)

    interface IUpdatee<Game.ViewModel.PlayerView> with
        member this.Update(viewModel) =
            let objectBase = viewModel.actorView.objectBaseView

            let area = objectBase.area

            this.SetPosition(area.position)

            this.SetSize(area.size)

            this.SetOccupation(viewModel.character.currentOccupation)


            moveAnimation.SetDirection(objectBase.direction)
            if this.Texture = null || objectBase.isMoved then
                moveAnimation.Next()

                this.SetScale()
        

    member this.SetPosition(pos) =
        if pos <> lastPosition then
            lastPosition <- pos
            this.Position <- Vec2.toVector2DF pos

    member this.SetSize(size) =
        if size <> lastSize then
            lastSize <- size
            this.SetScale()


    member this.SetOccupation(occupation) =
        if Some occupation <> lastOccupation then
            lastOccupation <- Some occupation

            gameViewSetting.occupationImages
            |> Map.find occupation
            |> moveAnimation.SetAnimationTextures

            this.SetScale()

    member this.SetScale() =
        if this.Texture <> null then
            let texSize = this.Texture.Size
            this.Scale <- (lastSize |> Vec2.toVector2DF) / texSize.To2DF()