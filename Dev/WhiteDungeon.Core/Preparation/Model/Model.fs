namespace WhiteDungeon.Core.Preparation

open WhiteDungeon.Core.Model
open WhiteDungeon.Core

open wraikny.Tart.Advanced

type Model = {
    playerCount : int
    players : Game.Model.Actor.PlayerBuilder list

    dungeonBuilder : Dungeon.DungeonBuilder

    gameSetting : GameSetting
}