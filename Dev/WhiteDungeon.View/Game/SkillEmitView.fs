namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Core.View
open wraikny.MilleFeuille.Fs.Objects
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Math
open WhiteDungeon.View.Utils.Color

type SkillEmitView(gameViewSetting) =
    inherit asd.GeometryObject2D()

    let mutable lastPosition = Vec2.zero()
    let mutable lastSize = Vec2.zero()

    interface IUpdatee<Game.ViewModel.AreaSkillEmitView> with
        member this.Update(viewModel) =
            let objectBase = viewModel.baseView
            
            let area = objectBase.area
            
            this.SetPosition(area.position)
            
            this.SetSize(area.size)


    member this.SetPosition(pos) =
        if pos <> lastPosition then
            lastPosition <- pos
            this.Position <- Vec2.toVector2DF pos

    member this.SetSize(size) =
        if size <> lastSize then
            lastSize <- size