module Yacht.UI.Scenes

open Yacht.GameState

type Scene =
  | MainMenu
  | OnePlayer
  | TwoPlayer
  | GameOver of GameState
  | Exiting

type Msg =
  | SelectOne
  | SelectTwo
  | SelectExit
  | ShowGameOver of GameState
  | BackToMenu

let transition (scene: Scene) (msg: Msg) : Scene =
  match scene, msg with
  | MainMenu, SelectOne -> OnePlayer
  | MainMenu, SelectTwo -> TwoPlayer
  | MainMenu, SelectExit -> Exiting
  | OnePlayer, BackToMenu -> MainMenu
  | TwoPlayer, BackToMenu -> MainMenu
  | TwoPlayer, ShowGameOver final -> GameOver final
  | GameOver _, BackToMenu -> MainMenu
  | _ -> scene
