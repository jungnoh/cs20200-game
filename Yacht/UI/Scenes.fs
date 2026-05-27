module Yacht.UI.Scenes

open Yacht.Difficulty
open Yacht.GameState

type Scene =
  | MainMenu
  | Difficulty
  | OnePlayer of Difficulty
  | TwoPlayer
  | GameOver of state: GameState * slotLabels: (string * string) option
  | Exiting

type Msg =
  | SelectOne
  | SelectTwo
  | SelectExit
  | SelectDifficulty of Difficulty
  | BackToMenu
  | ShowGameOver of state: GameState * slotLabels: (string * string) option

let transition (scene: Scene) (msg: Msg) : Scene =
  match scene, msg with
  | MainMenu, SelectOne -> Difficulty
  | MainMenu, SelectTwo -> TwoPlayer
  | MainMenu, SelectExit -> Exiting
  | Difficulty, SelectDifficulty d -> OnePlayer d
  | Difficulty, BackToMenu -> MainMenu
  | OnePlayer _, BackToMenu -> MainMenu
  | OnePlayer _, ShowGameOver(final, labels) -> GameOver(final, labels)
  | TwoPlayer, BackToMenu -> MainMenu
  | TwoPlayer, ShowGameOver(final, labels) -> GameOver(final, labels)
  | GameOver _, BackToMenu -> MainMenu
  | _ -> scene
