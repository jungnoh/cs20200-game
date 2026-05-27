module Yacht.UI.GameView

#nowarn "44" // Terminal.Gui v2 still exposes the legacy static `Application` API.

open System
open System.Collections.ObjectModel
open Terminal.Gui.App
open Terminal.Gui.Input
open Terminal.Gui.ViewBase
open Terminal.Gui.Views
open Yacht.Difficulty
open Yacht.Bot
open Yacht.GameState
open Yacht.Scoring
open Yacht.UI.Scenes

type PlayerMode =
  | SinglePlayer of Difficulty
  | TwoPlayer

let private scorecardWidth = 25

let private diceIn state =
  match state.Phase with
  | AwaitingFirstRoll -> None
  | Rolled(dice, rollsUsed) -> Some(dice, rollsUsed)

let private playerNames (mode: PlayerMode) : string * string =
  match mode with
  | SinglePlayer d -> "You", sprintf "Bot — %O" d
  | TwoPlayer -> "Player 1", "Player 2"

let private nameFor (mode: PlayerMode) (slot: PlayerSlot) : string =
  let p1, p2 = playerNames mode

  match slot with
  | Player1 -> p1
  | Player2 -> p2

let private cardTitle (mode: PlayerMode) (slot: PlayerSlot) (current: PlayerSlot) : string =
  let name = nameFor mode slot
  if slot = current then sprintf "%s (turn)" name else name

let private statusFor (mode: PlayerMode) (state: GameState) =
  match outcome state with
  | InProgress -> sprintf "%s: %O." (nameFor mode state.Current) state.Phase
  | final -> sprintf "Game over: %O." final

let private formatAction action =
  match action with
  | BotRolled(dice, n) -> sprintf "Bot rolled (%d/3): %s" n (dice |> List.map string |> String.concat " ")
  | BotKept mask ->
    let indices keep =
      mask
      |> List.indexed
      |> List.choose (fun (i, k) -> if k = keep then Some(sprintf "#%d" (i + 1)) else None)
      |> String.concat ", "
      |> function
        | "" -> "(none)"
        | s -> s

    sprintf "Bot kept %s; re-rolled %s" (indices true) (indices false)
  | BotStopped(_, n) -> sprintf "Bot stopped after roll %d" n
  | BotRecorded(cat, score) -> sprintf "Bot recorded %O: %d" cat score

let private labelsFor (mode: PlayerMode) =
  match mode with
  | SinglePlayer _ -> Some(playerNames mode)
  | TwoPlayer -> None

let create (mode: PlayerMode) (title: string) (dispatch: Msg -> unit) : View =
  let rng = Random()
  let roller () = rng.Next(1, 7)
  let strategyRandom = Random()

  let mutable state = initial
  let mutable keepMask = List.replicate 5 false
  let mutable categoryChoices: Category list = allCategories
  let mutable controlsLocked = false
  let mutable cancelled = false

  let isSingle =
    match mode with
    | SinglePlayer _ -> true
    | TwoPlayer -> false

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
  categoryList.Width <- if isSingle then Dim.Percent 30 else Dim.Fill 2
  categoryList.Height <- 7
  categoryList.SetSource categoryItems

  let rollButton = new Button()
  rollButton.Text <- "Roll (Enter)"
  rollButton.X <- 1
  rollButton.Y <- Pos.AnchorEnd 2

  let botLogItems = ObservableCollection<string>()
  let botLog = new ListView()
  botLog.X <- Pos.Percent 70
  botLog.Y <- Pos.AnchorEnd 9
  botLog.Width <- Dim.Fill 2
  botLog.Height <- 7
  botLog.SetSource botLogItems

  let backButton = new Button()
  backButton.Text <- "Back"
  backButton.X <- Pos.AnchorEnd 8
  backButton.Y <- Pos.AnchorEnd 2

  let refresh () =
    status.Text <- statusFor mode state

    p1Label.Text <- ScorecardFormat.format state.Player1
    p2Label.Text <- ScorecardFormat.format state.Player2

    p1Card.Title <- cardTitle mode Player1 state.Current
    p2Card.Title <- cardTitle mode Player2 state.Current

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

    categoryChoices <- Scorecard.unfilledCategories (currentScorecard state)
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

  let lockUi locked =
    controlsLocked <- locked
    rollButton.Enabled <- not locked
    diceList.Enabled <- not locked
    categoryList.Enabled <- not locked

  let appendLog (line: string) =
    botLogItems.Add line
    botLog.SelectedItem <- Nullable(botLogItems.Count - 1)

  let rec scheduleStep (difficulty: Difficulty) =
    status.Text <- "Rolling.."

    Application.AddTimeout(
      TimeSpan.FromSeconds 1.0,
      fun () ->
        if not cancelled then
          runOneStep difficulty

        false
    )
    |> ignore

  and runOneStep (difficulty: Difficulty) =
    let mask =
      match state.Phase with
      | AwaitingFirstRoll -> []
      | Rolled _ -> keepMask

    match roll roller mask state with
    | Ok next ->
      state <- next

      match next.Phase with
      | Rolled(dice, n) -> appendLog (formatAction (BotRolled(dice, n)))
      | _ -> ()

      refresh ()
      decideNext difficulty
    | Error _ -> lockUi false

  and decideNext (difficulty: Difficulty) =
    match state.Phase with
    | AwaitingFirstRoll -> ()
    | Rolled(dice, rollsUsed) ->
      match decideRoll difficulty strategyRandom dice rollsUsed (currentScorecard state) with
      | KeepAndReroll mask ->
        keepMask <- mask
        appendLog (formatAction (BotKept mask))
        refresh ()
        scheduleStep difficulty
      | StopRolling ->
        appendLog (formatAction (BotStopped(dice, rollsUsed)))

        let category =
          decideCategory difficulty strategyRandom dice (currentScorecard state)

        let score = scoreDice category dice

        match record category state with
        | Ok next ->
          state <- next
          keepMask <- List.replicate 5 false
          appendLog (formatAction (BotRecorded(category, score)))
          refresh ()

          if isGameOver state then
            dispatch (ShowGameOver(state, labelsFor mode))
          else
            lockUi false
        | Error _ -> lockUi false

  let startBotTurn () =
    match mode with
    | TwoPlayer -> ()
    | SinglePlayer d ->
      lockUi true
      scheduleStep d

  let doRoll () =
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
    if not controlsLocked then
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
        state <- next
        keepMask <- List.replicate 5 false
        refresh ()

        if isGameOver state then
          dispatch (ShowGameOver(state, labelsFor mode))
        else
          match mode with
          | SinglePlayer _ when state.Current = Player2 -> startBotTurn ()
          | _ -> ()
      | Error error -> status.Text <- string error

  rollButton.Accepting.Add(fun _ -> doRoll ())

  diceList.Accepting.Add(fun args ->
    args.Handled <- true
    doRoll ())

  categoryList.Accepting.Add(fun args ->
    args.Handled <- true
    doRecord ())

  backButton.Accepting.Add(fun _ ->
    cancelled <- true
    dispatch BackToMenu)

  let handleSpace (key: Key) =
    if key.Equals Key.Space then
      key.Handled <- true
      doToggleKeep ()

  frame.KeyDown.Add handleSpace
  diceList.KeyDown.Add handleSpace
  categoryList.KeyDown.Add handleSpace

  frame.Add status |> ignore
  frame.Add p1Card |> ignore
  frame.Add p2Card |> ignore
  frame.Add diceList |> ignore
  frame.Add categoryList |> ignore

  if isSingle then
    frame.Add botLog |> ignore

  frame.Add rollButton |> ignore
  frame.Add backButton |> ignore

  refresh ()
  frame :> View
