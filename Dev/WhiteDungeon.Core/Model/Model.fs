namespace WhiteDungeon.Core.Model

open wraikny.Tart.Math

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Model
open wraikny.Tart.Helper
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Advanced
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced.Dungeon

open FSharpPlus

type GameSceneMode =
    | HowToControl
    | GateMode
    | Pause
    | GameMode
    | WaitingGenerating
    | ErrorUI of exn
    //| GameOver
    | GameFinished of back:bool


type AfterLoseSight =
    | Stop
    | LookAround
    | ChaseLosePoint


type ChaseKind =
    | Losable of AfterLoseSight
    | ChaseTrace of time:float32


type FreeMove =
    | Forward
    | WithRotate of frame : uint16

//module FreeMove =
//    let toContainer = function
//        | Forward -> NoValue
//        | WithRotate x -> WithRotateContainer x


type OccupationSetting = {
    status : ActorStatus
    skill1 : Model -> Player -> SkillEmitBuilder list
    skill2 : Model -> Player -> SkillEmitBuilder list

    skill1CoolTime : uint16
    skill2CoolTime : uint16

    growthEasing : Easing

    size : float32 Vec2
}


and EnemySetting = {
    actorStatus : ActorStatus
    skillCoolTime : uint16
    skill : Model -> Enemy -> SkillEmitBuilder list

    visionAngleRate : float32
    visionDistance : float32
    //chaseKind : ChaseKind
    freeMove : FreeMove

    attackDistance : float32
    attackRange : float32

    hateDecrease : float32

    exPoint : uint16

    popFrequency : uint16
}


and GameSetting = {
    dungeonCellSize : float32 Vec2
    minPlayerCount : int
    maxPlayerCount : int
    binarySearchCountMovingOnWall : int

    visionWallCheckCount : uint32

    enemyUpdateDistance : float32
    damageCalculation : float32 -> Actor -> Actor -> float32
    occupationSettings : HashMap<Occupation, OccupationSetting>

    enemySettings : HashMap<EnemyKind, EnemySetting>
    enemyFrequencySum : uint16
    enemyFrequencyRanges : ( EnemyKind * (uint16 * uint16) ) []
    enemyGrowthEasing : Easing

    levelOffset : uint16
    lvUpExp : uint16 -> uint16

    maxLevel : uint16
    playerGrowthRateOverMax : float32

    levelSD : float32

    createDungeonBuilder : uint16 -> uint16 -> DungeonBuilder
    gateCount : uint16 -> uint16 -> int
}


and Model = {
    count : uint32

    nextPlayerID : uint32
    players : Map<PlayerID, Player>

    enemies : Map<EnemyID, Enemy>

    dungeonBuilder: Dungeon.DungeonBuilder
    dungeonModel : Dungeon.DungeonModel
    //dungeonGateCells : int Vec2 Set
    buildings : Building list

    skillList : SkillList

    timePassed : bool
    mode : GameSceneMode
    inBuildingFrame : uint32
    currentBuilding : Building option
    //haveEnteredBuilding : bool

    dungeonFloor : uint16

    initSize : uint16
    localPlayerId : PlayerID
    gameSetting : GameSetting
} with
    static member SetSkillList (x, s) =
        { x with skillList = s }


module OccupationSetting =
    let inline skillOf kind x =
        kind |> function
        | Skill1 -> x.skill1CoolTime, x.skill1
        | Skill2 -> x.skill2CoolTime, x.skill2


type EnemyInits = {
    kind : EnemyKind
    lookAngleRadian : float32
    levelDiff : int
}

type DungeonParams = {
    dungeonBuilder : DungeonBuilder
    dungeonModel : DungeonModel
    //gateCells : int Vec2 Set
    buildings : Building list
    enemyCells : (EnemyInits * int Vec2) []
    initPosition : float32 Vec2
}

module Model =
    let inline count (model : Model) = model.count

    let inline nextPlayerID (model : Model) = model.nextPlayerID
    let inline players (model : Model) = model.players
    
    let inline dungeonBuilder (model : Model) = model.dungeonBuilder
    let inline dungeonModel (model : Model) = model.dungeonModel

    let inline gameSetting (model : Model) = model.gameSetting


    let cellsToEnemies (gameSetting : GameSetting) (dungeonFloor) (enemyCells : (EnemyInits * int Vec2) []) cellSize : Map<_, Enemy> =
        enemyCells
        |> Seq.indexed
        |> Seq.map(fun (index, (ei, cell)) ->
            let enemyId = EnemyID <| uint32 index
            let setting = gameSetting.enemySettings |> HashMap.find ei.kind

            let level =
                (int gameSetting.levelOffset + ei.levelDiff + int dungeonFloor - 1)
                |> max 1
                |> uint16

            let status =
                Actor.calcStatusOf
                    gameSetting.enemyGrowthEasing
                    1.0f
                    gameSetting.maxLevel
                    level
                    setting.actorStatus

            enemyId
            , Enemy.init
                (Vec2.init 100.0f 100.0f)
                ( (DungeonModel.cellToCoordinate cellSize cell) + (cellSize .* 0.5f) )
                enemyId
                level
                status
                ei.kind
                ei.lookAngleRadian
                setting.visionDistance
                setting.visionAngleRate
        )
        |> Map.ofSeq

    let inline init players initSize (dungeonParams : DungeonParams) gameSetting = {
        gameSetting = gameSetting
        count = 0u

        nextPlayerID = players |> Map.count |> uint32
        players = players

        enemies =
            cellsToEnemies
                gameSetting
                1us
                dungeonParams.enemyCells
                gameSetting.dungeonCellSize

        skillList = SkillList.init

        dungeonBuilder = dungeonParams.dungeonBuilder
        dungeonModel = dungeonParams.dungeonModel
        //dungeonGateCells = Seq.toList dungeonGateCells |> Set.ofSeq
        buildings = dungeonParams.buildings

        timePassed = false

        mode = HowToControl
        inBuildingFrame = 0u
        currentBuilding = None

        dungeonFloor = 1us
        initSize = initSize

        localPlayerId = PlayerID 0u
    }

module GameSetting =
    open wraikny.Tart.Advanced
    open wraikny.Tart.Helper.Collections

    let inline collidedWithCell (gameSetting) cell =
        Dungeon.DungeonModel.coordinateToCell
            gameSetting.dungeonCellSize
        >> (=) cell
        |> Seq.exists

    let collidedWiithCells (gameSetting : GameSetting) cells =
        Dungeon.DungeonModel.coordinateToCell
            gameSetting.dungeonCellSize
        >> flip Set.contains cells
        |> Seq.exists

    let insideCells (gameSetting : GameSetting) cells =
        Dungeon.DungeonModel.coordinateToCell
            gameSetting.dungeonCellSize
        >> flip HashMap.containsKey cells

    let inline insideDungeon 
        (gameSetting : GameSetting)
        (dungeonModel : Dungeon.DungeonModel) =
        insideCells gameSetting dungeonModel.cells
        |> Seq.forall

    let inline insideDungeonOfLine
        (gameSetting : GameSetting)
        (dungeonModel : Dungeon.DungeonModel)
        count
        (p1) (p2) =

        let count = count - 1u

        [0u..count - 1u]
        |> Seq.map (fun x -> float32 x / float32 count)
        |> Seq.map(fun x -> p1 .* x + p2 .* (1.0f - x))
        |> insideDungeon gameSetting dungeonModel

module Dungeon =

    let inline generateDungeonModel (dungeonBuilder : DungeonBuilder) =
        (Random.int minValue<int> maxValue<int>)
        |> SideEffect.bind(fun seed ->
            let rec loop n = async {
                try
                    let builder = { dungeonBuilder with seed = seed + n }
                    let dungeon = DungeonBuilder.generate builder

                    if length dungeon.largeRooms > 3 then
                        return Ok( builder, dungeon )
                    else
                        return! loop(n + 1)
                with e -> return Error e
            }

            loop 0
        )

    let generateDungeonParams gameSetting gateCount dungeonBuilder (dungeonModel : DungeonModel) =
        let largeRooms = toList dungeonModel.largeRooms
        let smallRooms = toList dungeonModel.smallRooms
        
        let largeRoomsCount = length largeRooms
        let smallRoomsCount = length smallRooms

        let fromCell =
            gameSetting.dungeonCellSize
            |> DungeonModel.cellToCoordinate

        let rec loop() = monad {
            try
                //let gen = Random.int minValue<int> maxValue<int>
                let genNatural = Random.int 0 maxValue<int>
                let smallRoomGen = Random.int 0 (smallRoomsCount - 1)
                //let largeRoomGen = Random.int 0 (largeRoomsCount - 1)

                let! largeRoomRnds = Random.list largeRoomsCount genNatural
                let largeRoomValues =
                    largeRoomRnds
                    |> Seq.indexed
                    |> Seq.sortBy snd
                    |> Seq.toList

                let initRoomIndex = fst largeRoomValues.[0]

                let initRoom = snd largeRooms.[initRoomIndex]

                let initPosition =
                    initRoom.rect
                    |>> fromCell
                    |> Rect.centerPosition


                let toLargeRoomCells values =
                    seq {
                        for (i, v) in values ->
                            let room = snd largeRooms.[i]
                            let cells = room |> Space.cells
                            let cell = fst cells.[ v % length cells]
                            cell
                    }

                let gateBuildings =
                    toLargeRoomCells largeRoomValues.[1..gateCount]
                    |> Seq.indexed
                    |>> fun (i, c) -> Building.init one c (uint32 i) Gate
                    |> toList

                (*
                let itemCells =
                    (getCells largeRoomValues.[(gateCount + 1)..])
                    |> toList

                let! itemRnds = Random.list (length itemCells) genNatural

                let items =
                    Seq.zip itemCells itemRnds
                    |> Seq.toList
                *)


                let! enemyCellIndexs =
                    Random.list smallRoomsCount smallRoomGen

                let enemyKindsCount = gameSetting.enemySettings |> HashMap.count

                let searchKind ki =
                    let rec searchKind width index =
                        let index = index |> max 0 |> min (enemyKindsCount - 1)
                        let kind, (minV, maxV) = gameSetting.enemyFrequencyRanges.[index]
                        if ki < minV then
                            searchKind ( (width / 2) |> max 1 ) (index + width)
                        elif maxV < ki then
                            searchKind ( (width / 2) |> max 1 ) (index - width)
                        elif ki = minV then
                            fst gameSetting.enemyFrequencyRanges.[index - 1]
                        else
                            kind

                    searchKind (enemyKindsCount / 4) (enemyKindsCount / 2)

                let! enemyInints =
                    Random.list smallRoomsCount ( monad {
                        let! kindValue = Random.int 1 (int gameSetting.enemyFrequencySum)

                        let! angle = Random.double01
                        let! p1 = Random.double01
                        let! p2 = Random.double01
                        let p1, _ = Utils.boxMullersMethod (float32 p1) (float32 p2)
                        return {
                            EnemyInits.kind = searchKind (uint16 kindValue)
                            lookAngleRadian = 2.0f * pi * float32 angle
                            levelDiff = int <| (p1 * gameSetting.levelSD)
                        }
                    })

                let enemyCells =
                    seq {
                        for (index, (_, space)) in Seq.indexed smallRooms ->
                            let cells = space |> Space.cells
                            let cellIndex = (enemyCellIndexs : int list).[index]
                            let cell = fst cells.[ cellIndex % length cells]
                            let ei = (enemyInints : _ list).[index]
                            (ei, cell)
                    }
                    |> Seq.toArray

                return {
                    dungeonBuilder = dungeonBuilder
                    dungeonModel = dungeonModel
                    buildings = gateBuildings
                    enemyCells = enemyCells
                    initPosition = initPosition
                }
            with e ->
                printfn "%A" e
                return! loop()
        }

        flip SideEffect.performWith (loop())
