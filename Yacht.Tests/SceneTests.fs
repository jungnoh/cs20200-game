namespace yacht.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Yacht.Difficulty
open Yacht.UI.Scenes

[<TestClass>]
type ScenesTransitionTests() =

  [<TestMethod>]
  member _.``MainMenu + SelectOne -> Difficulty``() =
    Assert.AreEqual<Scene>(Difficulty, transition MainMenu SelectOne)

  [<TestMethod>]
  member _.``MainMenu + SelectTwo -> TwoPlayer``() =
    Assert.AreEqual<Scene>(TwoPlayer, transition MainMenu SelectTwo)

  [<TestMethod>]
  member _.``MainMenu + SelectExit -> Exiting``() =
    Assert.AreEqual<Scene>(Exiting, transition MainMenu SelectExit)

  [<TestMethod>]
  member _.``OnePlayer + BackToMenu -> MainMenu``() =
    Assert.AreEqual<Scene>(MainMenu, transition (OnePlayer Easy) BackToMenu)

  [<TestMethod>]
  member _.``TwoPlayer + BackToMenu -> MainMenu``() =
    Assert.AreEqual<Scene>(MainMenu, transition TwoPlayer BackToMenu)

  [<TestMethod>]
  member _.``MainMenu + BackToMenu is a no-op``() =
    Assert.AreEqual<Scene>(MainMenu, transition MainMenu BackToMenu)

  [<TestMethod>]
  member _.``OnePlayer + SelectOne is a no-op``() =
    Assert.AreEqual<Scene>(OnePlayer Easy, transition (OnePlayer Easy) SelectOne)

  [<TestMethod>]
  member _.``Exiting state is absorbing``() =
    Assert.AreEqual<Scene>(Exiting, transition Exiting BackToMenu)
    Assert.AreEqual<Scene>(Exiting, transition Exiting SelectOne)

  [<TestMethod>]
  member _.``TwoPlayer + ShowGameOver -> GameOver``() =
    let final = Yacht.GameState.initial
    Assert.AreEqual<Scene>(GameOver(final, None), transition TwoPlayer (ShowGameOver(final, None)))

  [<TestMethod>]
  member _.``OnePlayer + ShowGameOver -> GameOver``() =
    let final = Yacht.GameState.initial
    let labels = Some("You", "Bot — Easy")

    Assert.AreEqual<Scene>(GameOver(final, labels), transition (OnePlayer Easy) (ShowGameOver(final, labels)))

  [<TestMethod>]
  member _.``GameOver + BackToMenu -> MainMenu``() =
    let final = Yacht.GameState.initial
    Assert.AreEqual<Scene>(MainMenu, transition (GameOver(final, None)) BackToMenu)

  [<TestMethod>]
  member _.``MainMenu + ShowGameOver is a no-op``() =
    let final = Yacht.GameState.initial
    Assert.AreEqual<Scene>(MainMenu, transition MainMenu (ShowGameOver(final, None)))

  [<TestMethod>]
  member _.``GameOver + SelectOne is a no-op``() =
    let final = Yacht.GameState.initial
    Assert.AreEqual<Scene>(GameOver(final, None), transition (GameOver(final, None)) SelectOne)

  [<TestMethod>]
  member _.``Difficulty + SelectDifficulty Easy -> OnePlayer Easy``() =
    Assert.AreEqual<Scene>(OnePlayer Easy, transition Difficulty (SelectDifficulty Easy))

  [<TestMethod>]
  member _.``Difficulty + SelectDifficulty Hard -> OnePlayer Hard``() =
    Assert.AreEqual<Scene>(OnePlayer Hard, transition Difficulty (SelectDifficulty Hard))

  [<TestMethod>]
  member _.``Difficulty + BackToMenu -> MainMenu``() =
    Assert.AreEqual<Scene>(MainMenu, transition Difficulty BackToMenu)

  [<TestMethod>]
  member _.``Difficulty + SelectOne is a no-op``() =
    Assert.AreEqual<Scene>(Difficulty, transition Difficulty SelectOne)
