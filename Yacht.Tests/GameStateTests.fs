namespace yacht.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Yacht.Scoring
open Yacht.GameState

module private Helpers =
  let sequenceRoller (values: int list) : DiceRoller =
    let mutable remaining = values

    fun () ->
      match remaining with
      | [] -> failwith "sequenceRoller exhausted"
      | x :: rest ->
        remaining <- rest
        x

  let mustOk (r: Result<'a, GameError>) : 'a =
    match r with
    | Ok x -> x
    | Error e -> failwithf "expected Ok, got Error %A" e

  let mustErr (r: Result<'a, GameError>) : GameError =
    match r with
    | Ok _ -> failwith "expected Error, got Ok"
    | Error e -> e

  // Drives a full game: 12 turns per player, alternating, single roll per turn,
  // each player records categories in `allCategories` order using whatever dice the
  // roller emits. Returns the final state.
  let playFullGame (roller: DiceRoller) : GameState =
    let mutable state = initial

    for cat in allCategories do
      state <- state |> roll roller [] |> mustOk |> record cat |> mustOk // P1
      state <- state |> roll roller [] |> mustOk |> record cat |> mustOk // P2

    state

open Helpers

[<TestClass>]
type InitialStateTests() =

  [<TestMethod>]
  member _.``Initial state has empty scorecards``() =
    Assert.AreEqual<Scorecard>(Scorecard.empty, initial.Player1)
    Assert.AreEqual<Scorecard>(Scorecard.empty, initial.Player2)

  [<TestMethod>]
  member _.``Player 1 takes the first turn``() =
    Assert.AreEqual<PlayerSlot>(Player1, initial.Current)

  [<TestMethod>]
  member _.``Initial phase is AwaitingFirstRoll``() =
    Assert.AreEqual<TurnPhase>(AwaitingFirstRoll, initial.Phase)

  [<TestMethod>]
  member _.``Initial state is not game over``() =
    initial |> isGameOver |> Assert.IsFalse

  [<TestMethod>]
  member _.``Initial outcome is InProgress``() =
    Assert.AreEqual<GameOutcome>(InProgress, outcome initial)

  [<TestMethod>]
  member _.``currentScorecard returns Player1 scorecard initially``() =
    Assert.AreEqual<Scorecard>(Scorecard.empty, currentScorecard initial)

[<TestClass>]
type RollTests() =

  [<TestMethod>]
  member _.``First roll produces 5 dice and rollsUsed=1``() =
    let roller = sequenceRoller [ 1; 2; 3; 4; 5 ]
    let state = initial |> roll roller [] |> mustOk

    match state.Phase with
    | Rolled(dice, n) ->
      Assert.AreEqual<int list>([ 1; 2; 3; 4; 5 ], dice)
      Assert.AreEqual<int>(1, n)
    | _ -> Assert.Fail "expected Rolled phase"

  [<TestMethod>]
  member _.``First roll ignores keepMask``() =
    // pass empty mask; first-roll branch should ignore it
    let roller = sequenceRoller [ 6; 6; 6; 6; 6 ]
    let state = initial |> roll roller [] |> mustOk

    match state.Phase with
    | Rolled(dice, _) -> Assert.AreEqual<int list>([ 6; 6; 6; 6; 6 ], dice)
    | _ -> Assert.Fail "expected Rolled phase"

  [<TestMethod>]
  member _.``Re-roll honors keepMask``() =
    // first roll: [1;2;3;4;5]; re-roll keeping first two, others become 6
    let roller = sequenceRoller [ 1; 2; 3; 4; 5; 6; 6; 6 ]
    let afterFirst = initial |> roll roller [] |> mustOk

    let afterSecond =
      afterFirst |> roll roller [ true; true; false; false; false ] |> mustOk

    match afterSecond.Phase with
    | Rolled(dice, n) ->
      Assert.AreEqual<int list>([ 1; 2; 6; 6; 6 ], dice)
      Assert.AreEqual<int>(2, n)
    | _ -> Assert.Fail "expected Rolled phase"

  [<TestMethod>]
  member _.``Three rolls are allowed; fourth returns NoRollsLeft``() =
    let roller = List.replicate 15 3 |> sequenceRoller // enough for 3 rolls × 5 dice
    let keepNone = List.replicate 5 false
    let s1 = initial |> roll roller [] |> mustOk
    let s2 = s1 |> roll roller keepNone |> mustOk
    let s3 = s2 |> roll roller keepNone |> mustOk

    match s3.Phase with
    | Rolled(_, n) -> Assert.AreEqual<int>(3, n)
    | _ -> Assert.Fail "expected Rolled phase"

    Assert.AreEqual<GameError>(NoRollsLeft, s3 |> roll roller keepNone |> mustErr)

  [<TestMethod>]
  member _.``Re-roll with wrong-length keepMask returns InvalidKeepMask``() =
    let roller = List.replicate 10 3 |> sequenceRoller
    let afterFirst = initial |> roll roller [] |> mustOk
    Assert.AreEqual<GameError>(InvalidKeepMask, afterFirst |> roll roller [ true; false ] |> mustErr)

  [<TestMethod>]
  member _.``Roll after game over returns GameAlreadyOver``() =
    let roller = List.replicate 120 1 |> sequenceRoller
    let final = playFullGame roller
    final |> isGameOver |> Assert.IsTrue
    Assert.AreEqual<GameError>(GameAlreadyOver, final |> roll roller [] |> mustErr)

  [<TestMethod>]
  member _.``Previously-kept dice can be re-rolled in a later sub-roll``() =
    // Spec 2.1.2.2: "Dice previously kept may be returned to the re-roll set on a later sub-roll."
    // Roll 1: [1;2;3;4;5]. Roll 2: keep first three -> [1;2;3;6;6]. Roll 3: keep only last two,
    // releasing the previously-kept first three -> [7;7;7;6;6].
    let roller = sequenceRoller [ 1; 2; 3; 4; 5; 6; 6; 7; 7; 7 ]
    let s1 = initial |> roll roller [] |> mustOk
    let s2 = s1 |> roll roller [ true; true; true; false; false ] |> mustOk

    match s2.Phase with
    | Rolled(dice, _) -> Assert.AreEqual<int list>([ 1; 2; 3; 6; 6 ], dice)
    | _ -> Assert.Fail "expected Rolled phase"

    let s3 = s2 |> roll roller [ false; false; false; true; true ] |> mustOk

    match s3.Phase with
    | Rolled(dice, n) ->
      Assert.AreEqual<int list>([ 7; 7; 7; 6; 6 ], dice)
      Assert.AreEqual<int>(3, n)
    | _ -> Assert.Fail "expected Rolled phase"

[<TestClass>]
type RecordTests() =

  [<TestMethod>]
  member _.``Record before first roll returns NoDiceYet``() =
    Assert.AreEqual<GameError>(NoDiceYet, initial |> record Choice |> mustErr)

  [<TestMethod>]
  member _.``Record swaps current player and resets phase``() =
    let roller = sequenceRoller [ 1; 2; 3; 4; 5 ]

    let state =
      initial
      |> roll roller []
      |> mustOk
      |> record Choice
      |> mustOk

    Assert.AreEqual<PlayerSlot>(Player2, state.Current)
    Assert.AreEqual<TurnPhase>(AwaitingFirstRoll, state.Phase)
    Assert.AreEqual<int option>(Some 15, Scorecard.categoryScore Choice state.Player1)

  [<TestMethod>]
  member _.``Recording into category filled by current player returns CategoryAlreadyFilled``() =
    // P1 records Aces, P2 records Aces, then P1 tries Aces again
    let roller = List.replicate 30 1 |> sequenceRoller
    let s1 = initial |> roll roller [] |> mustOk |> record Aces |> mustOk // P1
    let s2 = s1 |> roll roller [] |> mustOk |> record Aces |> mustOk // P2
    let s3 = s2 |> roll roller [] |> mustOk // P1 rolling
    Assert.AreEqual<GameError>(CategoryAlreadyFilled, s3 |> record Aces |> mustErr)

  [<TestMethod>]
  member _.``Player2 can record a category that Player1 already filled``() =
    let roller = List.replicate 10 1 |> sequenceRoller
    let s1 = initial |> roll roller [] |> mustOk |> record Aces |> mustOk // P1 fills Aces
    Assert.AreEqual<PlayerSlot>(Player2, s1.Current)
    let s2 = s1 |> roll roller [] |> mustOk |> record Aces |> mustOk // P2 fills Aces — must succeed
    Assert.AreEqual<int option>(Some 5, Scorecard.categoryScore Aces s2.Player1)
    Assert.AreEqual<int option>(Some 5, Scorecard.categoryScore Aces s2.Player2)
    Assert.AreEqual<PlayerSlot>(Player1, s2.Current)

  [<TestMethod>]
  member _.``Record after game over returns GameAlreadyOver``() =
    let roller = List.replicate 120 1 |> sequenceRoller
    let final = playFullGame roller
    Assert.AreEqual<GameError>(GameAlreadyOver, final |> record Choice |> mustErr)

  [<TestMethod>]
  member _.``Record succeeds after the third roll``() =
    // Spec 2.1.2.4: after the third roll the player must record. Verify the state
    // machine still accepts a record action at rollsUsed = 3.
    let roller = List.replicate 15 1 |> sequenceRoller
    let keepNone = List.replicate 5 false

    let s3 =
      initial
      |> roll roller []
      |> mustOk
      |> roll roller keepNone
      |> mustOk
      |> roll roller keepNone
      |> mustOk

    let after = s3 |> record Aces |> mustOk
    Assert.AreEqual<int option>(Some 5, Scorecard.categoryScore Aces after.Player1)
    Assert.AreEqual<PlayerSlot>(Player2, after.Current)
    Assert.AreEqual<TurnPhase>(AwaitingFirstRoll, after.Phase)

[<TestClass>]
type TurnAlternationTests() =

  [<TestMethod>]
  member _.``Players strictly alternate after each record``() =
    let roller = List.replicate 20 1 |> sequenceRoller
    let s1 = initial |> roll roller [] |> mustOk |> record Aces |> mustOk
    Assert.AreEqual<PlayerSlot>(Player2, s1.Current)
    let s2 = s1 |> roll roller [] |> mustOk |> record Aces |> mustOk
    Assert.AreEqual<PlayerSlot>(Player1, s2.Current)
    let s3 = s2 |> roll roller [] |> mustOk |> record Deuces |> mustOk
    Assert.AreEqual<PlayerSlot>(Player2, s3.Current)

[<TestClass>]
type GameEndTests() =

  [<TestMethod>]
  member _.``Tie: identical dice every turn yields equal totals``() =
    // Both players record every category with [1;1;1;1;1].
    // Per-player total: Aces=5, Choice=5, FourOfAKind=5, Yacht=50, others=0  =>  65.
    let roller = List.replicate 120 1 |> sequenceRoller
    let final = playFullGame roller
    final |> isGameOver |> Assert.IsTrue
    Assert.AreEqual<int>(65, Scorecard.total final.Player1)
    Assert.AreEqual<int>(65, Scorecard.total final.Player2)
    Assert.AreEqual<GameOutcome>(Tie, outcome final)

  [<TestMethod>]
  member _.``Player2Wins: Player2 gets higher dice every turn``() =
    // P1 always [1;1;1;1;1] (total 65); P2 always [2;2;2;2;2] (total 80).
    // Roller alternates 5 ones then 5 twos for 12 rounds.
    let dice =
      [ for _ in 1..12 do
          yield! List.replicate 5 1
          yield! List.replicate 5 2 ]

    let final = dice |> sequenceRoller |> playFullGame
    Assert.AreEqual<int>(65, Scorecard.total final.Player1)
    Assert.AreEqual<int>(80, Scorecard.total final.Player2)
    Assert.AreEqual<GameOutcome>(Player2Wins, outcome final)

  [<TestMethod>]
  member _.``Player1Wins: Player1 gets higher dice every turn``() =
    let dice =
      [ for _ in 1..12 do
          yield! List.replicate 5 2
          yield! List.replicate 5 1 ]

    let final = dice |> sequenceRoller |> playFullGame
    Assert.AreEqual<int>(80, Scorecard.total final.Player1)
    Assert.AreEqual<int>(65, Scorecard.total final.Player2)
    Assert.AreEqual<GameOutcome>(Player1Wins, outcome final)
