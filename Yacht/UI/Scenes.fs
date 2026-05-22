module Yacht.UI.Scenes

type Scene =
  | MainMenu
  | OnePlayer
  | TwoPlayer
  | Exiting

type Msg =
  | SelectOne
  | SelectTwo
  | SelectExit
  | BackToMenu

let transition (scene: Scene) (msg: Msg) : Scene =
  match scene, msg with
  | MainMenu, SelectOne -> OnePlayer
  | MainMenu, SelectTwo -> TwoPlayer
  | MainMenu, SelectExit -> Exiting
  | OnePlayer, BackToMenu -> MainMenu
  | TwoPlayer, BackToMenu -> MainMenu
  | _ -> scene
