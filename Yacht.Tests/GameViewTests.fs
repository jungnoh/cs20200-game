namespace yacht.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Yacht.GameState
open Yacht.Scoring
open Yacht.UI.GameView

[<TestClass>]
type GameViewControlStateTests() =

  let rolled rollsUsed =
    { initial with
        Phase = Rolled([ 1; 2; 3; 4; 5 ], rollsUsed) }

  [<TestMethod>]
  member _.``Initial turn only enables rolling``() =
    let controls = controlState false initial

    Assert.IsTrue controls.CanRoll
    Assert.IsFalse controls.CanChooseDice
    Assert.IsFalse controls.CanRecordCategory

  [<TestMethod>]
  member _.``First and second rolls enable roll dice and category choices``() =
    for rollsUsed in [ 1; 2 ] do
      let controls = controlState false (rolled rollsUsed)

      Assert.IsTrue controls.CanRoll
      Assert.IsTrue controls.CanChooseDice
      Assert.IsTrue controls.CanRecordCategory

  [<TestMethod>]
  member _.``Third roll only enables category choices``() =
    let controls = controlState false (rolled 3)

    Assert.IsFalse controls.CanRoll
    Assert.IsFalse controls.CanChooseDice
    Assert.IsTrue controls.CanRecordCategory

  [<TestMethod>]
  member _.``Locked controls disable game actions``() =
    let controls = controlState true (rolled 1)

    Assert.IsFalse controls.CanRoll
    Assert.IsFalse controls.CanChooseDice
    Assert.IsFalse controls.CanRecordCategory

[<TestClass>]
type GameViewFocusTargetTests() =

  [<TestMethod>]
  member _.``Initial turn prefers roll button``() =
    Assert.AreEqual<FocusTarget>(RollButton, initial |> controlState false |> preferredFocusTarget)

  [<TestMethod>]
  member _.``Reroll window prefers dice list``() =
    let state =
      { initial with
          Phase = Rolled([ 1; 2; 3; 4; 5 ], 1) }

    Assert.AreEqual<FocusTarget>(DiceList, state |> controlState false |> preferredFocusTarget)

  [<TestMethod>]
  member _.``Third roll prefers category selector``() =
    let state =
      { initial with
          Phase = Rolled([ 1; 2; 3; 4; 5 ], 3) }

    Assert.AreEqual<FocusTarget>(CategoryList, state |> controlState false |> preferredFocusTarget)

  [<TestMethod>]
  member _.``Locked controls fall back to back button``() =
    Assert.AreEqual<FocusTarget>(BackButton, initial |> controlState true |> preferredFocusTarget)

[<TestClass>]
type GameViewShakeFrameTests() =

  [<TestMethod>]
  member _.``Kept positions keep their face, rolled positions use the roller``() =
    let roller () = 4
    let frame = shakeFrame roller [ true; false; true; false; false ] [ 6; 1; 5; 2; 3 ]
    Assert.AreEqual<int list>([ 6; 4; 5; 4; 4 ], frame)

  [<TestMethod>]
  member _.``Frame always has five dice in 1..6``() =
    let rng = System.Random 42

    let frame =
      shakeFrame (fun () -> rng.Next(1, 7)) (List.replicate 5 false) (List.replicate 5 0)

    Assert.AreEqual<int>(5, List.length frame)
    Assert.IsTrue(frame |> List.forall (fun d -> d >= 1 && d <= 6))
