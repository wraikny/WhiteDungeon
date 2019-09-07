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
    | Gate

module DungeonCellKind =
    let fromSpaceID = function
        | Dungeon.Corridor _ -> Corridor
        | Dungeon.Small _ -> SmallRoom
        | Dungeon.Large _ -> LargeRoom


type DungeonCellView(cellSize : float32 Vec2, path : string) =
    inherit asd.Chip2D()

    do
        base.Texture <- asd.Engine.Graphics.CreateTexture2D(path)

    interface IUpdatee<int Vec2 * DungeonCellKind> with
        member this.Update(viewModel) =
            let cell, kind = viewModel

            this.Color <-
                kind
                |> function
                | Corridor ->
                    ColorPalette.sumire
                | SmallRoom ->
                    ColorPalette.ume
                | LargeRoom ->
                    ColorPalette.sakura
                | Gate ->
                    ColorPalette.black

            let pos =
                Dungeon.DungeonModel.cellToCoordinate cellSize cell
            this.Position <- Vec2.toVector2DF pos

            this.Scale <- (Vec2.toVector2DF cellSize) / this.Texture.Size.To2DF()