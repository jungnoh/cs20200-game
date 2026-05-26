module Yacht.UI.AppHost

#nowarn "44" // Terminal.Gui v2 still exposes the legacy static `Application` API.

open Terminal.Gui.App
open Terminal.Gui.Input
open Terminal.Gui.ViewBase
open Terminal.Gui.Views
open Yacht.UI.Scenes

let run () : unit =
  Application.Init()

  try
    let root = new Window()
    root.Title <- "Yacht"

    let mutable currentScene = MainMenu
    let mutable currentView: View option = None
    let dispatchRef: (Msg -> unit) ref = ref (fun _ -> ())

    let render () =
      match currentView with
      | Some v ->
        root.Remove v |> ignore
        v.Dispose()
      | None -> ()

      let v =
        match currentScene with
        | MainMenu -> MainMenuView.create dispatchRef.Value
        | OnePlayer -> MainMenuView.create dispatchRef.Value
        | TwoPlayer -> GameView.create GameView.TwoPlayer "Two-Player Mode" dispatchRef.Value
        | GameOver final -> GameOverView.create final dispatchRef.Value
        | Exiting -> new View()

      root.Add v |> ignore
      currentView <- Some v

    dispatchRef.Value <-
      fun msg ->
        let next = transition currentScene msg
        currentScene <- next

        match next with
        | Exiting -> Application.RequestStop root
        | _ -> Application.Invoke(System.Action render)

    Application.KeyDown.Add(fun key ->
      if key.Equals Key.C.WithCtrl then
        key.Handled <- true
        Application.RequestStop root)

    render ()
    Application.Run(root, null)
  finally
    Application.Shutdown()
