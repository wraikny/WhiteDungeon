namespace WhiteDungeon.View.Game


open wraikny.Tart.Helper.Utils
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Core
open wraikny.Tart.Advanced
open wraikny.MilleFeuille.Fs.Objects
open wraikny.MilleFeuille.Fs.Input
open wraikny.MilleFeuille.Core
open wraikny.MilleFeuille.Core.Input

open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color

open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Extension
open wraikny.MilleFeuille.Fs
open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.Geometry

open FSharpPlus

open System
open System.Collections.Generic
open System.Linq
open System.Reactive
open System.Reactive.Linq

[<Struct>]
type DungeonCellKind =
    | Corridor
    | SmallRoom
    | LargeRoom

module DungeonCellKind =
    let fromSpaceID = function
        | Dungeon.Corridor _ -> Corridor
        | Dungeon.Small _ -> SmallRoom
        | Dungeon.Large _ -> LargeRoom


type CellAround = DungeonCellKind ValueOption

type DungeonCellViewModel =
    (CellAround * int Vec2) * (CellAround Vec3 * CellAround Vec2 * CellAround Vec3)


type DungeonCellView(cellSize : float32 Vec2) =
    inherit asd.Chip2D()

    do
        base.Texture <- asd.Engine.Graphics.CreateTexture2D("Image/Debug/empty200x200white.png")

    interface IUpdatee<int Vec2 * Dungeon.SpaceID> with
        member this.Update(viewModel) =
            //let ((kind, cell), (v1, v2, v3)) = viewModel
            ////let c00, c01, c02 = v1.x, v1.y, v1.z
            ////let c10,      c12 = v2.x,       v2.y
            ////let c20, c21, c22 = v3.x, v3.y, v3.z

            let cell, id = viewModel

            this.Color <-
                id
                |> DungeonCellKind.fromSpaceID
                |> function
                | Corridor ->
                    ColorPalette.sumire
                | SmallRoom ->
                    ColorPalette.ume
                | LargeRoom ->
                    ColorPalette.sakura

            let pos =
                Dungeon.DungeonModel.cellToCoordinate cellSize cell
            this.Position <- Vec2.toVector2DF pos

            this.Scale <- (Vec2.toVector2DF cellSize) / this.Texture.Size.To2DF()

