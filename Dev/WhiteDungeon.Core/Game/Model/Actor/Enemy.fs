namespace WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model


type AfterLoseSight =
    | Stop
    | LookAround
    | ChaseLosePoint


type ChaseKind =
    | Losable of AfterLoseSight



type 'Model EnemySetting =
    {
        actorStatus : ActorStatus
        skill : 'Model -> Actor.Actor -> Skill.SkillEmitBuilder list

        visionAngle : float32
        visionDistance : float32
        lookAroundAfterLoseSight : bool
        chaseKind : ChaseKind
    }


type Enemy =
    {
        actor : Actor

        id : EnemyID
    }

with
    member inline x.objectBase =
        x.actor.objectBase

    static member inline SetActor (x : Enemy, y) =
        { x with actor = y }

    static member inline SetObjectBase (x : Enemy, y) =
        Actor.map (ObjectBase.set y) x


module Enemy =
    let init size position id actorStatus = {
        actor = Actor.Actor.init size position (Actor.OfEnemyID id) actorStatus
        id = id
    }