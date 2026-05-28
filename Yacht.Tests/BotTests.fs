namespace Yacht.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Yacht.Scoring
open Yacht.GameState
open Yacht.Difficulty
open Yacht.Bot

[<TestClass>]
type BotEasyTests() =

  [<TestMethod>]
  member _.``Easy decideRoll can both reroll and stop before roll 3``() =
    let decisions =
      [ for seed in 0..49 -> decideRoll Easy (Random(seed)) [ 1; 2; 3; 4; 5 ] 1 Scorecard.empty ]

    Assert.IsTrue(
      decisions
      |> List.exists (function
        | KeepAndReroll _ -> true
        | _ -> false)
    )

    Assert.IsTrue(
      decisions
      |> List.exists (function
        | StopRolling -> true
        | _ -> false)
    )

  [<TestMethod>]
  member _.``Easy decideRoll stops at 3 rolls used``() =
    let r = Random 42

    match decideRoll Easy r [ 1; 2; 3; 4; 5 ] 3 Scorecard.empty with
    | StopRolling -> ()
    | KeepAndReroll _ -> Assert.Fail "Expected StopRolling at rollsUsed=3"

  [<TestMethod>]
  member _.``Easy decideCategory picks the best-scoring applicable category``() =
    let r = Random 42
    // [1;2;3;4;5]: Large Straight (30) beats every other applicable category.
    let cat = decideCategory Easy r [ 1; 2; 3; 4; 5 ] Scorecard.empty
    Assert.AreEqual<Category>(LargeStraight, cat)

  [<TestMethod>]
  member _.``Easy decideCategory never wastes a scoring roll``() =
    let r = Random 42
    // [6;6;6;1;2]: Choice (21) beats Sixes (18); Easy must not pick a 0-scoring slot.
    let cat = decideCategory Easy r [ 6; 6; 6; 1; 2 ] Scorecard.empty
    Assert.AreEqual<Category>(Choice, cat)
    Assert.IsTrue(scoreDice cat [ 6; 6; 6; 1; 2 ] > 0)

  [<TestMethod>]
  member _.``Easy decideCategory falls back to lowest-max when nothing applicable``() =
    let r = Random 42
    // Upper + Choice filled; [1;1;2;3;6] scores nowhere → lowest-max unfilled = SmallStraight (15).
    let filled = [ Aces; Deuces; Threes; Fours; Fives; Sixes; Choice ]

    let sc = { Scores = filled |> List.map (fun c -> c, 0) |> Map.ofList }

    let cat = decideCategory Easy r [ 1; 1; 2; 3; 6 ] sc
    Assert.AreEqual<Category>(SmallStraight, cat)

[<TestClass>]
type BotIntermediateTests() =

  [<TestMethod>]
  member _.``Intermediate decideRoll keeps most common face``() =
    let r = Random 42

    match decideRoll Intermediate r [ 1; 1; 2; 2; 3 ] 1 Scorecard.empty with
    | KeepAndReroll mask ->
      // 1s and 2s tie at count 2; smaller face wins → keep the 1s.
      Assert.AreEqual<bool list>([ true; true; false; false; false ], mask)
    | StopRolling -> Assert.Fail "Expected KeepAndReroll"

  [<TestMethod>]
  member _.``Intermediate decideRoll stops at 3 rolls used``() =
    let r = Random 42

    match decideRoll Intermediate r [ 1; 2; 3; 4; 5 ] 3 Scorecard.empty with
    | StopRolling -> ()
    | KeepAndReroll _ -> Assert.Fail "Expected StopRolling at rollsUsed=3"

  [<TestMethod>]
  member _.``Intermediate decideCategory picks Yacht when applicable``() =
    let r = Random 42
    let cat = decideCategory Intermediate r [ 5; 5; 5; 5; 5 ] Scorecard.empty
    Assert.AreEqual<Category>(Yacht, cat)

  [<TestMethod>]
  member _.``Intermediate decideCategory falls back to lowest-max when nothing applicable``() =
    let r = Random 42
    // Fill all upper categories and Choice; leave FourOfAKind, FullHouse, SmallStraight,
    // LargeStraight, Yacht open. With dice [1;1;2;3;6] none score → fallback to lowest-max
    // unfilled, which is SmallStraight (max 15).
    let filled = [ Aces; Deuces; Threes; Fours; Fives; Sixes; Choice ]

    let sc = { Scores = filled |> List.map (fun c -> c, 0) |> Map.ofList }

    let cat = decideCategory Intermediate r [ 1; 1; 2; 3; 6 ] sc
    Assert.AreEqual<Category>(SmallStraight, cat)

[<TestClass>]
type BotHardTests() =

  [<TestMethod>]
  member _.``Hard decideCategory picks Yacht when applicable``() =
    let r = Random 42
    let cat = decideCategory Hard r [ 5; 5; 5; 5; 5 ] Scorecard.empty
    Assert.AreEqual<Category>(Yacht, cat)

  [<TestMethod>]
  member _.``Hard decideRoll stops on a Yacht``() =
    let r = Random 42

    match decideRoll Hard r [ 5; 5; 5; 5; 5 ] 1 Scorecard.empty with
    | StopRolling -> ()
    | KeepAndReroll _ -> Assert.Fail "Expected StopRolling on a Yacht"

  [<TestMethod>]
  member _.``Hard decideRoll stops at 3 rolls used``() =
    let r = Random 42

    match decideRoll Hard r [ 1; 2; 3; 4; 5 ] 3 Scorecard.empty with
    | StopRolling -> ()
    | KeepAndReroll _ -> Assert.Fail "Expected StopRolling at rollsUsed=3"

[<TestClass>]
type BotDeterminismTests() =

  [<TestMethod>]
  member _.``simulateGame is deterministic given the same seed``() =
    let a = simulateGame Hard Easy 12345
    let b = simulateGame Hard Easy 12345
    Assert.AreEqual<GameOutcome>(a, b)

  [<TestMethod>]
  member _.``simulateGame terminates``() =
    let outcome = simulateGame Easy Easy 7
    Assert.AreNotEqual<GameOutcome>(InProgress, outcome)

[<TestClass>]
type HardBeatsEasyTests() =

  [<TestMethod>]
  member _.``Hard wins at least 60 of 100 games vs Easy``() =
    let mutable hardWins = 0

    for i in 0..99 do
      // Alternate Hard's slot to average out first-mover bias.
      let p1, p2 = if i % 2 = 0 then Hard, Easy else Easy, Hard
      let outcome = simulateGame p1 p2 (20200206 + i)

      let hardWon =
        match outcome with
        | Player1Wins -> p1 = Hard
        | Player2Wins -> p2 = Hard
        | Tie
        | InProgress -> false

      if hardWon then
        hardWins <- hardWins + 1

    Assert.IsTrue(hardWins >= 60, sprintf "Hard won %d/100; expected >= 60" hardWins)

[<TestClass>]
type IntermediateBeatsEasyTests() =

  [<TestMethod>]
  member _.``Intermediate wins at least 60 of 100 games vs Easy``() =
    let mutable intWins = 0

    for i in 0..99 do
      // Alternate Intermediate's slot to average out first-mover bias.
      let p1, p2 = if i % 2 = 0 then Intermediate, Easy else Easy, Intermediate
      let outcome = simulateGame p1 p2 (20200206 + i)

      let intWon =
        match outcome with
        | Player1Wins -> p1 = Intermediate
        | Player2Wins -> p2 = Intermediate
        | Tie
        | InProgress -> false

      if intWon then
        intWins <- intWins + 1

    Assert.IsTrue(intWins >= 60, sprintf "Intermediate won %d/100; expected >= 60" intWins)
