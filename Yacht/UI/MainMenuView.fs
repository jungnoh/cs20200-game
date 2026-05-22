module Yacht.UI.MainMenuView

open System.Collections.ObjectModel
open Terminal.Gui.App
open Terminal.Gui.ViewBase
open Terminal.Gui.Views
open Yacht.UI.Scenes

let create (dispatch: Msg -> unit) : View =
  let frame = new FrameView()
  frame.Title <- "Yacht"
  frame.X <- Pos.Center()
  frame.Y <- Pos.Center()
  frame.Width <- Dim.Percent 50
  frame.Height <- Dim.Percent 50

  let items = ObservableCollection<string> [| "1 Player"; "2 Players"; "Exit" |]

  let list = new ListView()
  list.X <- Pos.Center()
  list.Y <- 1
  list.Width <- Dim.Fill 2
  list.Height <- Dim.Fill 2
  list.SetSource items
  list.SelectedItem <- System.Nullable 0

  list.Accepting.Add(fun _ ->
    let idx =
      if list.SelectedItem.HasValue then
        list.SelectedItem.Value
      else
        0

    match idx with
    | 0 -> dispatch SelectOne
    | 1 -> dispatch SelectTwo
    | 2 -> dispatch SelectExit
    | _ -> ())

  frame.Add(list) |> ignore
  frame :> View
