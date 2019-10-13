namespace WhiteDungeon.Core

open WhiteDungeon.Core.Model
open Affogato
open Affogato.Advanced

type ViewMsg =
    | UpdateDungeonView of Dungeon.Model //* Building list
    | DamagesView of (float32 Vector2 * float32) []
    //| AppendSkills of Skill.SkillEmit list  