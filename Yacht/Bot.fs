module Yacht.Bot

open System
open Yacht.Scoring
open Yacht.GameState
open Yacht.Difficulty

type BotDecision =
  | KeepAndReroll of mask: bool list
  | StopRolling

type BotAction =
  | BotRolled of dice: int list * rollsUsed: int
  | BotKept of mask: bool list
  | BotStopped of dice: int list * rollsUsed: int
  | BotRecorded of category: Category * score: int

let private mostCommonFace (dice: int list) : int =
  dice |> List.countBy id |> List.maxBy (fun (face, count) -> count, -face) |> fst

let private keepMaskForFace (dice: int list) (face: int) : bool list = dice |> List.map (fun d -> d = face)

let private randomKeepMask (random: Random) (dice: int list) : bool list =
  dice |> List.map (fun _ -> random.Next(2) = 0)

let private categoryMaxScore (category: Category) : int =
  match category with
  | Aces -> 5
  | Deuces -> 10
  | Threes -> 15
  | Fours -> 20
  | Fives -> 25
  | Sixes -> 30
  | Choice -> 30
  | FourOfAKind -> 30
  | FullHouse -> 28
  | SmallStraight -> 15
  | LargeStraight -> 30
  | Yacht -> 50

let private lowestMaxScoreUnfilled (sc: Scorecard) : Category =
  Scorecard.unfilledCategories sc |> List.minBy categoryMaxScore

let private easyDecideRoll (random: Random) (dice: int list) (rollsUsed: int) : BotDecision =
  if rollsUsed >= 3 then
    StopRolling
  elif random.Next 2 = 0 then
    KeepAndReroll(randomKeepMask random dice)
  else
    StopRolling

let private intermediateDecideRoll (dice: int list) (rollsUsed: int) : BotDecision =
  if rollsUsed >= 3 then
    StopRolling
  else
    let face = mostCommonFace dice
    KeepAndReroll(keepMaskForFace dice face)

let private greedyDecideCategory (dice: int list) (sc: Scorecard) : Category =
  let applicable = Scorecard.applicableCategories dice sc

  match applicable with
  | [] -> lowestMaxScoreUnfilled sc
  | xs -> xs |> List.maxBy (fun c -> scoreDice c dice)

let private allMasks: bool list list =
  [ for i in 0..31 -> [ for bit in 0..4 -> (i >>> bit) &&& 1 = 1 ] ]

let private simulateOne (random: Random) (currentDice: int list) (mask: bool list) (rollsRemaining: int) : int list =
  let mutable dice = currentDice
  let mutable currentMask = mask

  for _ in 1..rollsRemaining do
    dice <-
      List.zip dice currentMask
      |> List.map (fun (d, k) -> if k then d else random.Next(1, 7))

    if List.exists not currentMask then
      let face = mostCommonFace dice
      currentMask <- keepMaskForFace dice face

  dice

let private hardSamples = 100

let private bestExpectedAfter
  (random: Random)
  (currentDice: int list)
  (mask: bool list)
  (rollsRemaining: int)
  (sc: Scorecard)
  : float =
  let unfilled = Scorecard.unfilledCategories sc

  let sampled =
    [| for _ in 1..hardSamples -> simulateOne random currentDice mask rollsRemaining |]

  let mutable best = 0.0

  for cat in unfilled do
    let mutable total = 0

    for sample in sampled do
      total <- total + scoreDice cat sample

    let ev = float total / float hardSamples

    if ev > best then
      best <- ev

  best

let private hardDecideRoll (random: Random) (dice: int list) (rollsUsed: int) (sc: Scorecard) : BotDecision =
  if rollsUsed >= 3 then
    StopRolling
  else
    let rollsRemaining = 3 - rollsUsed
    let unfilled = Scorecard.unfilledCategories sc

    let stopEv = unfilled |> List.map (fun c -> scoreDice c dice) |> List.max |> float

    let bestMask, bestEv =
      allMasks
      |> List.map (fun mask -> mask, bestExpectedAfter random dice mask rollsRemaining sc)
      |> List.maxBy snd

    if bestEv > stopEv then
      KeepAndReroll bestMask
    else
      StopRolling

let private hardDecideCategory (dice: int list) (sc: Scorecard) : Category =
  Scorecard.unfilledCategories sc |> List.maxBy (fun c -> scoreDice c dice)

let decideRoll
  (difficulty: Difficulty)
  (random: Random)
  (dice: int list)
  (rollsUsed: int)
  (sc: Scorecard)
  : BotDecision =
  match difficulty with
  | Easy -> easyDecideRoll random dice rollsUsed
  | Intermediate -> intermediateDecideRoll dice rollsUsed
  | Hard -> hardDecideRoll random dice rollsUsed sc

let decideCategory (difficulty: Difficulty) (random: Random) (dice: int list) (sc: Scorecard) : Category =
  match difficulty with
  | Easy -> greedyDecideCategory dice sc
  | Intermediate -> greedyDecideCategory dice sc
  | Hard -> hardDecideCategory dice sc

let takeTurn
  (difficulty: Difficulty)
  (roller: DiceRoller)
  (random: Random)
  (state: GameState)
  : GameState * BotAction list =
  let mutable currentState = state
  let actions = ResizeArray<BotAction>()
  let mutable finished = false

  while not finished do
    match currentState.Phase with
    | AwaitingFirstRoll ->
      match roll roller [] currentState with
      | Ok next ->
        currentState <- next

        match next.Phase with
        | Rolled(dice, n) -> actions.Add(BotRolled(dice, n))
        | _ -> ()
      | Error _ -> finished <- true
    | Rolled(dice, rollsUsed) ->
      match decideRoll difficulty random dice rollsUsed (currentScorecard currentState) with
      | KeepAndReroll mask ->
        actions.Add(BotKept mask)

        match roll roller mask currentState with
        | Ok next ->
          currentState <- next

          match next.Phase with
          | Rolled(newDice, n) -> actions.Add(BotRolled(newDice, n))
          | _ -> ()
        | Error _ -> finished <- true
      | StopRolling ->
        actions.Add(BotStopped(dice, rollsUsed))
        let cat = decideCategory difficulty random dice (currentScorecard currentState)
        let score = scoreDice cat dice

        match record cat currentState with
        | Ok next ->
          currentState <- next
          actions.Add(BotRecorded(cat, score))
        | Error _ -> ()

        finished <- true

  currentState, List.ofSeq actions

let simulateGame (p1: Difficulty) (p2: Difficulty) (seed: int) : GameOutcome =
  let dieRandom = Random(seed)
  let strategyRandom = Random(seed + 1)
  let roller () = dieRandom.Next(1, 7)

  let mutable state = initial

  while not (isGameOver state) do
    let difficulty = if state.Current = Player1 then p1 else p2

    let next, _ = takeTurn difficulty roller strategyRandom state
    state <- next

  outcome state
