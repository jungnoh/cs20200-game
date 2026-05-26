module Yacht.UI.GameView

open System
open System.Collections.ObjectModel
open Terminal.Gui.App
open Terminal.Gui.Input
open Terminal.Gui.ViewBase
open Terminal.Gui.Views
open Yacht.GameState
open Yacht.Scoring
open Yacht.UI.Scenes

type PlayerMode =
  | SinglePlayer
  | TwoPlayer

let private scorecardWidth = 25

let private diceIn state =
  match state.Phase with
  | AwaitingFirstRoll -> None
  | Rolled(dice, rollsUsed) -> Some(dice, rollsUsed)

let private statusFor mode state =
  match mode with
  | SinglePlayer when Scorecard.isComplete state.Player1 ->
    sprintf "Game over: final score %d." (Scorecard.total state.Player1)
  | _ ->
    match outcome state with
    | InProgress -> sprintf "%O: %O." state.Current state.Phase
    | final -> sprintf "Game over: %O." final

let private playerTitle (slot: PlayerSlot) (current: PlayerSlot) =
  if slot = current then
    sprintf "%O (turn)" slot
  else
    string slot

let create (mode: PlayerMode) (title: string) (dispatch: Msg -> unit) : View =
  let rng = Random()
  let roller () = rng.Next(1, 7)

  let mutable state = initial
  let mutable keepMask = List.replicate 5 false
  let mutable categoryChoices: Category list = allCategories

  let frame = new FrameView()
  frame.Title <- title
  frame.X <- Pos.Center()
  frame.Y <- Pos.Center()
  frame.Width <- Dim.Percent 90
  frame.Height <- Dim.Percent 85

  let status = new Label()
  status.X <- 1
  status.Y <- 0
  status.Width <- Dim.Fill 2

  let p1Card = new FrameView()
  p1Card.Title <- "Player 1"
  p1Card.X <- 1
  p1Card.Y <- 2
  p1Card.Width <- Dim.Absolute scorecardWidth
  p1Card.Height <- Dim.Fill 10

  let p1Label = new Label()
  p1Label.X <- 1
  p1Label.Y <- 0
  p1Label.Width <- Dim.Fill 1
  p1Label.Height <- Dim.Fill 1
  p1Card.Add p1Label |> ignore

  let p2Card = new FrameView()
  p2Card.Title <- "Player 2"
  p2Card.X <- Pos.Right p1Card + Pos.op_Implicit 1
  p2Card.Y <- 2
  p2Card.Width <- Dim.Absolute scorecardWidth
  p2Card.Height <- Dim.Fill 10

  let p2Label = new Label()
  p2Label.X <- 1
  p2Label.Y <- 0
  p2Label.Width <- Dim.Fill 1
  p2Label.Height <- Dim.Fill 1
  p2Card.Add p2Label |> ignore

  let diceItems = ObservableCollection<string>()
  let diceList = new ListView()
  diceList.X <- 1
  diceList.Y <- Pos.AnchorEnd 9
  diceList.Width <- Dim.Percent 35
  diceList.Height <- 7
  diceList.SetSource diceItems

  let categoryItems = ObservableCollection<string>()
  let categoryList = new ListView()
  categoryList.X <- Pos.Percent 40
  categoryList.Y <- Pos.AnchorEnd 9
  categoryList.Width <- Dim.Fill 2
  categoryList.Height <- 7
  categoryList.SetSource categoryItems

  let rollButton = new Button()
  rollButton.Text <- "Roll (Enter)"
  rollButton.X <- 1
  rollButton.Y <- Pos.AnchorEnd 2

  let backButton = new Button()
  backButton.Text <- "Back"
  backButton.X <- Pos.AnchorEnd 8
  backButton.Y <- Pos.AnchorEnd 2

  let refresh () =
    status.Text <- statusFor mode state

    p1Label.Text <- ScorecardFormat.format state.Player1
    p1Card.Title <- playerTitle Player1 state.Current

    match mode with
    | SinglePlayer -> ()
    | TwoPlayer ->
      p2Label.Text <- ScorecardFormat.format state.Player2
      p2Card.Title <- playerTitle Player2 state.Current

    let prevDieIdx =
      if diceList.SelectedItem.HasValue then
        diceList.SelectedItem.Value
      else
        0

    diceItems.Clear()

    match diceIn state with
    | None ->
      for i in 1..5 do
        diceItems.Add(sprintf "[ ] Die %d: -" i)
    | Some(dice, _) ->
      dice
      |> List.iteri (fun i die ->
        let mark = if keepMask[i] then "x" else " "
        diceItems.Add(sprintf "[%s] Die %d: %d" mark (i + 1) die))

    if diceItems.Count > 0 then
      let idx = min prevDieIdx (diceItems.Count - 1)
      diceList.SelectedItem <- Nullable idx

    categoryChoices <-
      match diceIn state with
      | None ->
        allCategories
        |> List.filter (fun c -> not (Scorecard.isFilled c (currentScorecard state)))
      | Some(dice, _) -> Scorecard.applicableCategories dice (currentScorecard state)

    categoryItems.Clear()

    categoryChoices
    |> List.iter (fun category ->
      let value =
        match diceIn state with
        | None -> "-"
        | Some(dice, _) -> string (scoreDice category dice)

      categoryItems.Add(sprintf "%-16s %s" (string category) value))

    if categoryItems.Count > 0 && not categoryList.SelectedItem.HasValue then
      categoryList.SelectedItem <- Nullable 0

  let doRoll () =
    if mode = SinglePlayer && Scorecard.isComplete state.Player1 then
      status.Text <- statusFor mode state
    else
      let mask =
        match state.Phase with
        | AwaitingFirstRoll -> []
        | Rolled _ -> keepMask

      match roll roller mask state with
      | Ok next ->
        state <- next
        refresh ()
      | Error error -> status.Text <- string error

  let doToggleKeep () =
    match diceIn state with
    | None -> status.Text <- "Roll before keeping dice."
    | Some _ ->
      let idx =
        if diceList.SelectedItem.HasValue then
          diceList.SelectedItem.Value
        else
          0

      keepMask <- keepMask |> List.mapi (fun i keep -> if i = idx then not keep else keep)
      refresh ()

  let doRecord () =
    let idx =
      if categoryList.SelectedItem.HasValue then
        categoryList.SelectedItem.Value
      else
        0

    if idx >= 0 && idx < List.length categoryChoices then
      match record categoryChoices[idx] state with
      | Ok next ->
        state <-
          match mode with
          | SinglePlayer -> { next with Current = Player1 }
          | TwoPlayer -> next

        keepMask <- List.replicate 5 false
        refresh ()

        if mode = TwoPlayer && isGameOver state then
          dispatch (ShowGameOver state)
      | Error error -> status.Text <- string error

  rollButton.Accepting.Add(fun _ -> doRoll ())
  diceList.Accepting.Add(fun args ->
    args.Handled <- true
    doRoll ())
  categoryList.Accepting.Add(fun args ->
    args.Handled <- true
    doRecord ())
  backButton.Accepting.Add(fun _ -> dispatch BackToMenu)

  let handleSpace (key: Key) =
    if key.Equals Key.Space then
      key.Handled <- true
      doToggleKeep ()

  frame.KeyDown.Add handleSpace
  diceList.KeyDown.Add handleSpace
  categoryList.KeyDown.Add handleSpace

  frame.Add status |> ignore
  frame.Add p1Card |> ignore

  match mode with
  | SinglePlayer -> ()
  | TwoPlayer -> frame.Add p2Card |> ignore

  frame.Add diceList |> ignore
  frame.Add categoryList |> ignore
  frame.Add rollButton |> ignore
  frame.Add backButton |> ignore

  refresh ()
  frame :> View
