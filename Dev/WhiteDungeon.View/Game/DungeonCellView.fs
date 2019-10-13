namespace WhiteDungeon.View.Game


open Affogato
open Affogato.Advanced
open wraikny.Tart.Helper
open wraikny.MilleFeuille.Objects
open wraikny.MilleFeuille.Input
open wraikny.MilleFeuille

open WhiteDungeon.Core
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color

open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Extension

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
    //| Gate

module DungeonCellKind =
    let fromSpaceID = function
        | Dungeon.Corridor _ -> Corridor
        | Dungeon.Small _ -> SmallRoom
        | Dungeon.Large _ -> LargeRoom


type DungeonCellView(cellSize : float32 Vector2, path : string) =
    inherit asd.Chip2D()

    do
        base.Texture <- asd.Engine.Graphics.CreateTexture2D(path)

    interface IUpdatee<int Vector2 * DungeonCellKind> with
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
                //| Gate ->
                //    ColorPalette.black

            let pos =
                Dungeon.Model.cellToCoordinate cellSize cell
            this.Position <- Vector2.toVector2DF pos

            this.Scale <- (Vector2.toVector2DF cellSize) / this.Texture.Size.To2DF()