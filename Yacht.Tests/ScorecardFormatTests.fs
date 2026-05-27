namespace Yacht.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Yacht.Scoring
open Yacht.UI

[<TestClass>]
type ScorecardFormatTests() =

  [<TestMethod>]
  member _.``FormatForHeight keeps total visible when only one row fits``() =
    let result = ScorecardFormat.formatForHeight 1 Scorecard.empty

    Assert.IsTrue(result.StartsWith "Total")

  [<TestMethod>]
  member _.``FormatForHeight shows overflow indicator and summary rows when truncated``() =
    let lines = ScorecardFormat.formatForHeight 4 Scorecard.empty |> fun s -> s.Split '\n'

    Assert.AreEqual<int>(4, lines.Length)
    Assert.IsTrue(lines[1].StartsWith "Bonus")
    Assert.IsTrue(lines[2].Contains "more")
    Assert.IsTrue(lines[3].StartsWith "Total")

  [<TestMethod>]
  member _.``FormatForHeight shows the full scorecard when enough rows fit``() =
    let lines = ScorecardFormat.formatForHeight 17 Scorecard.empty |> fun s -> s.Split '\n'

    Assert.AreEqual<int>(17, lines.Length)
    Assert.IsFalse(lines |> Array.exists (fun line -> line.Contains "more"))
    Assert.IsTrue(lines[0].StartsWith "Aces")
    Assert.IsTrue(lines[5].StartsWith "Sixes")
    Assert.IsTrue(lines[6] |> Seq.forall ((=) '-'))
    Assert.IsTrue(lines[7].StartsWith "Bonus")
    Assert.IsTrue(lines[8] |> Seq.forall ((=) '-'))
    Assert.IsTrue(lines[15] |> Seq.forall ((=) '-'))
    Assert.IsTrue(lines[16].StartsWith "Total")

  [<TestMethod>]
  member _.``Format uses Switch-style upper bonus lower total order with line dividers``() =
    let lines = ScorecardFormat.format Scorecard.empty |> fun s -> s.Split '\n'

    Assert.IsTrue(lines[0].StartsWith "Aces")
    Assert.IsTrue(lines[5].StartsWith "Sixes")
    Assert.IsTrue(lines[6] |> Seq.forall ((=) '-'))
    Assert.IsTrue(lines[7].StartsWith "Bonus")
    Assert.IsTrue(lines[8] |> Seq.forall ((=) '-'))
    Assert.IsTrue(lines[9].StartsWith "Choice")
    Assert.IsTrue(lines[15] |> Seq.forall ((=) '-'))
    Assert.IsTrue(lines[16].StartsWith "Total")
