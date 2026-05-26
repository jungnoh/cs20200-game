namespace yacht.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Yacht.UI.Scenes

[<TestClass>]
type ScenesTransitionTests() =

  [<TestMethod>]
  member _.``MainMenu + SelectOne -> OnePlayer``() =
    Assert.AreEqual<Scene>(OnePlayer, transition MainMenu SelectOne)

  [<TestMethod>]
  member _.``MainMenu + SelectTwo -> TwoPlayer``() =
    Assert.AreEqual<Scene>(TwoPlayer, transition MainMenu SelectTwo)

  [<TestMethod>]
  member _.``MainMenu + SelectExit -> Exiting``() =
    Assert.AreEqual<Scene>(Exiting, transition MainMenu SelectExit)

  [<TestMethod>]
  member _.``OnePlayer + BackToMenu -> MainMenu``() =
    Assert.AreEqual<Scene>(MainMenu, transition OnePlayer BackToMenu)

  [<TestMethod>]
  member _.``TwoPlayer + BackToMenu -> MainMenu``() =
    Assert.AreEqual<Scene>(MainMenu, transition TwoPlayer BackToMenu)

  [<TestMethod>]
  member _.``MainMenu + BackToMenu is a no-op``() =
    Assert.AreEqual<Scene>(MainMenu, transition MainMenu BackToMenu)

  [<TestMethod>]
  member _.``OnePlayer + SelectOne is a no-op``() =
    Assert.AreEqual<Scene>(OnePlayer, transition OnePlayer SelectOne)

  [<TestMethod>]
  member _.``Exiting state is absorbing``() =
    Assert.AreEqual<Scene>(Exiting, transition Exiting BackToMenu)
    Assert.AreEqual<Scene>(Exiting, transition Exiting SelectOne)

  [<TestMethod>]
  member _.``TwoPlayer + ShowGameOver -> GameOver``() =
    let final = Yacht.GameState.initial
    Assert.AreEqual<Scene>(GameOver final, transition TwoPlayer (ShowGameOver final))

  [<TestMethod>]
  member _.``GameOver + BackToMenu -> MainMenu``() =
    let final = Yacht.GameState.initial
    Assert.AreEqual<Scene>(MainMenu, transition (GameOver final) BackToMenu)

  [<TestMethod>]
  member _.``MainMenu + ShowGameOver is a no-op``() =
    let final = Yacht.GameState.initial
    Assert.AreEqual<Scene>(MainMenu, transition MainMenu (ShowGameOver final))

  [<TestMethod>]
  member _.``OnePlayer + ShowGameOver is a no-op``() =
    let final = Yacht.GameState.initial
    Assert.AreEqual<Scene>(OnePlayer, transition OnePlayer (ShowGameOver final))

  [<TestMethod>]
  member _.``GameOver + SelectOne is a no-op``() =
    let final = Yacht.GameState.initial
    Assert.AreEqual<Scene>(GameOver final, transition (GameOver final) SelectOne)
