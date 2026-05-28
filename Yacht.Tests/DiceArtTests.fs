namespace yacht.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Yacht.UI.DiceArt

[<TestClass>]
type DiceArtTests() =

  [<TestMethod>]
  member _.``Cup renders eight lines``() =
    let art = renderCup [ 1; 2; 3; 4; 5 ] (List.replicate 5 false) Center "shake"
    Assert.AreEqual<int>(8, art.Split('\n').Length)

  [<TestMethod>]
  member _.``Grid rows share the dice-box width``() =
    let art = renderCup [ 6; 6; 6; 6; 6 ] (List.replicate 5 false) Center "shake"
    let lines = art.Split('\n')
    // lines 1..5 are the top border + three pip rows + bottom border, all 21 wide
    for i in 1..5 do
      Assert.AreEqual<int>(21, lines[i].Length)

  [<TestMethod>]
  member _.``Pips reflect the requested faces``() =
    // A row of ones shows a single centre pip per die and nothing else.
    let art = renderCup [ 1; 1; 1; 1; 1 ] (List.replicate 5 false) Center "shake"
    let lines = art.Split '\n'
    Assert.AreEqual<int>(5, lines[3] |> Seq.filter (fun c -> c = '●') |> Seq.length)
    Assert.AreEqual<int>(0, lines[2] |> Seq.filter (fun c -> c = '●') |> Seq.length)

  [<TestMethod>]
  member _.``Held dice are marked, free dice are not``() =
    let art =
      renderCup [ 2; 3; 4; 5; 6 ] [ true; false; false; false; false ] Center "shake"

    let held = art.Split '\n' |> Array.last
    Assert.AreEqual<int>(1, held |> Seq.filter (fun c -> c = '▲') |> Seq.length)

  [<TestMethod>]
  member _.``Tilt changes the shake arrows``() =
    let leftHeader =
      (renderCup [ 1; 1; 1; 1; 1 ] (List.replicate 5 false) Left "shake").Split '\n'
      |> Array.head

    let rightHeader =
      (renderCup [ 1; 1; 1; 1; 1 ] (List.replicate 5 false) Right "shake").Split '\n'
      |> Array.head

    Assert.IsTrue(leftHeader.Contains "«")
    Assert.IsTrue(rightHeader.Contains "»")
