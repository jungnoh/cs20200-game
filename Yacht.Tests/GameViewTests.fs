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
