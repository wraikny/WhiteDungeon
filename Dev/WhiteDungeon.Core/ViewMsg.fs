namespace WhiteDungeon.Core

open WhiteDungeon.Core.Model

open wraikny.Tart.Math
open wraikny.Tart.Math

open wraikny.Tart.Advanced
open wraikny.Tart.Helper.Collections
open WhiteDungeon.Core.Model


type ViewMsg =
    | UpdateDungeonView of Dungeon.DungeonModel //* Building list
    | DamagesView of (float32 Vec2 * float32) []
    //| AppendSkills of Skill.SkillEmit list  