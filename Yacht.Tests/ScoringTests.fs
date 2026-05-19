namespace Yacht.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Yacht.Scoring

[<TestClass>]
type ScoreDiceTests() =

  [<TestMethod>]
  member _.``Aces sums dice showing 1``() =
    Assert.AreEqual<int>(2, scoreDice Aces [ 1; 1; 3; 5; 6 ])

  [<TestMethod>]
  member _.``Sixes sums dice showing 6``() =
    Assert.AreEqual<int>(18, scoreDice Sixes [ 6; 6; 6; 3; 1 ])

  [<TestMethod>]
  member _.``Upper section scores 0 when face absent``() =
    Assert.AreEqual<int>(0, scoreDice Threes [ 1; 2; 4; 5; 6 ])

  [<TestMethod>]
  member _.``Choice always sums all dice``() =
    Assert.AreEqual<int>(15, scoreDice Choice [ 1; 2; 3; 4; 5 ])

  [<TestMethod>]
  member _.``Four of a Kind with exactly four matching``() =
    Assert.AreEqual<int>(22, scoreDice FourOfAKind [ 5; 5; 5; 5; 2 ])

  [<TestMethod>]
  member _.``Four of a Kind satisfied by five of a kind``() =
    Assert.AreEqual<int>(25, scoreDice FourOfAKind [ 5; 5; 5; 5; 5 ])

  [<TestMethod>]
  member _.``Four of a Kind fails with only three matching``() =
    Assert.AreEqual<int>(0, scoreDice FourOfAKind [ 5; 5; 5; 3; 2 ])

  [<TestMethod>]
  member _.``Full House with three plus two``() =
    Assert.AreEqual<int>(12, scoreDice FullHouse [ 2; 2; 2; 3; 3 ])

  [<TestMethod>]
  member _.``Full House rejects five of a kind``() =
    Assert.AreEqual<int>(0, scoreDice FullHouse [ 5; 5; 5; 5; 5 ])

  [<TestMethod>]
  member _.``Full House rejects two pair``() =
    Assert.AreEqual<int>(0, scoreDice FullHouse [ 2; 2; 3; 3; 4 ])

  [<TestMethod>]
  member _.``Full House rejects four plus one``() =
    Assert.AreEqual<int>(0, scoreDice FullHouse [ 4; 4; 4; 4; 1 ])

  [<TestMethod>]
  member _.``Small Straight 1-2-3-4 with extra``() =
    Assert.AreEqual<int>(15, scoreDice SmallStraight [ 1; 2; 3; 4; 6 ])

  [<TestMethod>]
  member _.``Small Straight 2-3-4-5 with duplicate``() =
    Assert.AreEqual<int>(15, scoreDice SmallStraight [ 2; 3; 4; 5; 5 ])

  [<TestMethod>]
  member _.``Small Straight 3-4-5-6 unordered``() =
    Assert.AreEqual<int>(15, scoreDice SmallStraight [ 3; 4; 5; 6; 1 ])

  [<TestMethod>]
  member _.``Small Straight rejects broken run``() =
    Assert.AreEqual<int>(0, scoreDice SmallStraight [ 1; 2; 3; 5; 6 ])

  [<TestMethod>]
  member _.``Small Straight is satisfied by a Large Straight``() =
    Assert.AreEqual<int>(15, scoreDice SmallStraight [ 1; 2; 3; 4; 5 ])
    Assert.AreEqual<int>(15, scoreDice SmallStraight [ 2; 3; 4; 5; 6 ])

  [<TestMethod>]
  member _.``Large Straight 1-2-3-4-5``() =
    Assert.AreEqual<int>(30, scoreDice LargeStraight [ 1; 2; 3; 4; 5 ])

  [<TestMethod>]
  member _.``Large Straight 2-3-4-5-6``() =
    Assert.AreEqual<int>(30, scoreDice LargeStraight [ 2; 3; 4; 5; 6 ])

  [<TestMethod>]
  member _.``Large Straight rejects small straight``() =
    Assert.AreEqual<int>(0, scoreDice LargeStraight [ 1; 2; 3; 4; 6 ])

  [<TestMethod>]
  member _.``Yacht with all same``() =
    Assert.AreEqual<int>(50, scoreDice Yacht [ 4; 4; 4; 4; 4 ])

  [<TestMethod>]
  member _.``Yacht rejects four of a kind``() =
    Assert.AreEqual<int>(0, scoreDice Yacht [ 4; 4; 4; 4; 3 ])

[<TestClass>]
type ScorecardTests() =

  [<TestMethod>]
  member _.``Empty scorecard is not complete and has zero total``() =
    let sc = Scorecard.empty
    Assert.IsFalse(Scorecard.isComplete sc)
    Assert.AreEqual<int>(0, Scorecard.total sc)
    Assert.AreEqual<int>(0, Scorecard.upperBonus sc)

  [<TestMethod>]
  member _.``Record stores the computed score``() =
    let sc = Scorecard.empty |> Scorecard.record Aces [ 1; 1; 3; 5; 6 ]
    Assert.IsTrue(sc.IsSome)
    Assert.AreEqual<int option>(Some 2, Scorecard.categoryScore Aces sc.Value)

  [<TestMethod>]
  member _.``Record can store zero for non-matching dice``() =
    let sc = Scorecard.empty |> Scorecard.record Yacht [ 1; 2; 3; 4; 5 ]
    Assert.IsTrue(sc.IsSome)
    Assert.AreEqual<int option>(Some 0, Scorecard.categoryScore Yacht sc.Value)
    Assert.IsTrue(Scorecard.isFilled Yacht sc.Value)

  [<TestMethod>]
  member _.``Record into already-filled category returns None``() =
    let sc = Scorecard.empty |> Scorecard.record Aces [ 1; 1; 3; 5; 6 ] |> Option.get
    Assert.AreEqual<Scorecard option>(None, Scorecard.record Aces [ 1; 1; 1; 1; 1 ] sc)

  [<TestMethod>]
  member _.``Filling all 12 categories makes scorecard complete``() =
    let sc =
      allCategories
      |> List.fold (fun s c -> Scorecard.record c [ 1; 1; 1; 1; 1 ] s |> Option.get) Scorecard.empty

    Assert.IsTrue(Scorecard.isComplete sc)

  [<TestMethod>]
  member _.``Upper bonus is 35 at subtotal exactly 63``() =
    // 3+6+9+12+15+18 = 63 (three of each face)
    let sc =
      [ Aces, [ 1; 1; 1; 2; 2 ]
        Deuces, [ 2; 2; 2; 1; 1 ]
        Threes, [ 3; 3; 3; 1; 1 ]
        Fours, [ 4; 4; 4; 1; 1 ]
        Fives, [ 5; 5; 5; 1; 1 ]
        Sixes, [ 6; 6; 6; 1; 1 ] ]
      |> List.fold (fun s (c, d) -> Scorecard.record c d s |> Option.get) Scorecard.empty

    Assert.AreEqual<int>(63, Scorecard.upperSubtotal sc)
    Assert.AreEqual<int>(35, Scorecard.upperBonus sc)

  [<TestMethod>]
  member _.``Upper bonus is 0 at subtotal 62``() =
    // 5+10+15+16+10+6 = 62
    let sc =
      [ Aces, [ 1; 1; 1; 1; 1 ]
        Deuces, [ 2; 2; 2; 2; 2 ]
        Threes, [ 3; 3; 3; 3; 3 ]
        Fours, [ 4; 4; 4; 4; 1 ]
        Fives, [ 5; 5; 1; 1; 1 ]
        Sixes, [ 6; 1; 1; 1; 1 ] ]
      |> List.fold (fun s (c, d) -> Scorecard.record c d s |> Option.get) Scorecard.empty

    Assert.AreEqual<int>(62, Scorecard.upperSubtotal sc)
    Assert.AreEqual<int>(0, Scorecard.upperBonus sc)

  [<TestMethod>]
  member _.``Total is sum of recorded scores plus upper bonus``() =
    // Upper section: 63 -> bonus 35. Plus Yacht=50, Choice=15.
    let sc =
      [ Aces, [ 1; 1; 1; 2; 2 ]
        Deuces, [ 2; 2; 2; 1; 1 ]
        Threes, [ 3; 3; 3; 1; 1 ]
        Fours, [ 4; 4; 4; 1; 1 ]
        Fives, [ 5; 5; 5; 1; 1 ]
        Sixes, [ 6; 6; 6; 1; 1 ]
        Yacht, [ 4; 4; 4; 4; 4 ]
        Choice, [ 1; 2; 3; 4; 5 ] ]
      |> List.fold (fun s (c, d) -> Scorecard.record c d s |> Option.get) Scorecard.empty
    // recorded sum = 63 + 50 + 15 = 128; +35 bonus = 163
    Assert.AreEqual<int>(163, Scorecard.total sc)

[<TestClass>]
type ApplicableCategoriesTests() =

  [<TestMethod>]
  member _.``Large straight dice list applicable categories``() =
    let result = Scorecard.applicableCategories [ 1; 2; 3; 4; 5 ] Scorecard.empty

    let expected =
      [ Aces; Deuces; Threes; Fours; Fives; Choice; SmallStraight; LargeStraight ]

    Assert.AreEqual<Category list>(expected, result)

  [<TestMethod>]
  member _.``Five of a kind dice list applicable categories``() =
    let result = Scorecard.applicableCategories [ 5; 5; 5; 5; 5 ] Scorecard.empty
    let expected = [ Fives; Choice; FourOfAKind; Yacht ]
    Assert.AreEqual<Category list>(expected, result)

  [<TestMethod>]
  member _.``Filled categories are excluded from applicable list``() =
    let sc = Scorecard.empty |> Scorecard.record Fives [ 5; 5; 5; 5; 5 ] |> Option.get
    let result = Scorecard.applicableCategories [ 5; 5; 5; 5; 5 ] sc
    Assert.IsFalse(List.contains Fives result)
    // Choice, FourOfAKind, Yacht still present
    Assert.IsTrue(List.contains Choice result)
    Assert.IsTrue(List.contains FourOfAKind result)
    Assert.IsTrue(List.contains Yacht result)

  [<TestMethod>]
  member _.``Applicable list preserves allCategories order``() =
    let result = Scorecard.applicableCategories [ 1; 2; 3; 4; 5 ] Scorecard.empty
    let positions = result |> List.map (fun c -> List.findIndex ((=) c) allCategories)
    let isSorted = positions = List.sort positions
    Assert.IsTrue(isSorted)
