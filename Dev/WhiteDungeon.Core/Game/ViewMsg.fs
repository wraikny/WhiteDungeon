namespace WhiteDungeon.Core.Game.ViewMsg

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Advanced
open wraikny.Tart.Helper.Collections
open WhiteDungeon.Core.Game.Model


type ViewMsg =
    | UpdateDungeonView of Dungeon.DungeonModel * int Vec2 Set
    | DamagesView of (float32 Vec2 * float32) []
    //| AppendSkills of Skill.SkillEmit list  