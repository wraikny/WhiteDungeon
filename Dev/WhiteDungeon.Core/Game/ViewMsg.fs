namespace WhiteDungeon.Core.Game.ViewMsg

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Advanced
open wraikny.Tart.Helper.Collections
open WhiteDungeon.Core.Game.Model


type ViewMsg =
    | UpdateDungeonView of Dungeon.DungeonModel
    | AppendSkills of Skill.SkillEmit list