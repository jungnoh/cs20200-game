module Yacht.UI.GameView

#nowarn "44" // Terminal.Gui v2 still exposes the legacy static `Application` API.

open System
open System.Collections.ObjectModel
open Terminal.Gui.App
open Terminal.Gui.Drawing
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

type GameControlsState =
  { CanRoll: bool
    CanChooseDice: bool
    CanRecordCategory: bool }

type FocusTarget =
  | DiceList
  | CategoryList

let private scorecardWidth = 25
let private categorySelectorWidth = 20

let controlState (controlsLocked: bool) (state: GameState) : GameControlsState =
  if controlsLocked then
    { CanRoll = false
      CanChooseDice = false
      CanRecordCategory = false }
  else
    match state.Phase with
    | AwaitingFirstRoll ->
      { CanRoll = true
        CanChooseDice = false
        CanRecordCategory = false }
    | Rolled(_, rollsUsed) ->
      { CanRoll = rollsUsed < 3
        CanChooseDice = rollsUsed < 3
        CanRecordCategory = true }

let preferredFocusTarget (controls: GameControlsState) : FocusTarget =
  if controls.CanRecordCategory && not controls.CanRoll then
    CategoryList
  elif controls.CanChooseDice || controls.CanRoll then
    DiceList
  else
    CategoryList

let shakeFrame (roller: unit -> int) (keepMask: bool list) (currentDice: int list) : int list =
  List.init 5 (fun i ->
    let kept = i < List.length keepMask && keepMask[i]
    if kept then currentDice[i] else roller ())

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
  let mutable animating = false
  let mutable throwing = false
  let mutable animFaces = List.replicate 5 0
  let mutable animTilt = DiceArt.Center
  let mutable animLabel = "shake"

  let frame = new FrameView()
  frame.Title <- title
  frame.X <- 0
  frame.Y <- 0
  frame.Width <- Dim.Fill 0
  frame.Height <- Dim.Fill 0

  let status = new Label()
  status.X <- 1
  status.Y <- 0
  status.Width <- Dim.Fill 2

  let boldScheme () =
    let mutable fg = Color.None
    let mutable bg = Color.None
    let mutable style = TextStyle.Bold
    Scheme(Attribute(&fg, &bg, &style))

  let addScorecardLabels (card: FrameView) =
    let upper = new Label()
    upper.X <- 1
    upper.Y <- 0
    upper.Width <- Dim.Fill 1
    upper.CanFocus <- false

    let bonus = new Label()
    bonus.X <- 1
    bonus.Y <- Pos.Bottom upper
    bonus.Width <- Dim.Fill 1
    bonus.CanFocus <- false
    bonus.SetScheme(boldScheme ()) |> ignore

    let lower = new Label()
    lower.X <- 1
    lower.Y <- Pos.Bottom bonus
    lower.Width <- Dim.Fill 1
    lower.CanFocus <- false

    let total = new Label()
    total.X <- 1
    total.Y <- Pos.Bottom lower
    total.Width <- Dim.Fill 1
    total.CanFocus <- false
    total.SetScheme(boldScheme ()) |> ignore

    card.Add upper |> ignore
    card.Add bonus |> ignore
    card.Add lower |> ignore
    card.Add total |> ignore
    upper, bonus, lower, total

  let p1Card = new FrameView()
  p1Card.Title <- "Player 1"
  p1Card.X <- 1
  p1Card.Y <- 2
  p1Card.Width <- Dim.Absolute scorecardWidth
  p1Card.Height <- Dim.Fill 3
  p1Card.CanFocus <- false

  let p1Upper, p1Bonus, p1Lower, p1Total = addScorecardLabels p1Card

  let p2Card = new FrameView()
  p2Card.Title <- "Player 2"
  p2Card.X <- Pos.Right p1Card + Pos.op_Implicit 1
  p2Card.Y <- 2
  p2Card.Width <- Dim.Absolute scorecardWidth
  p2Card.Height <- Dim.Fill 3
  p2Card.CanFocus <- false

  let p2Upper, p2Bonus, p2Lower, p2Total = addScorecardLabels p2Card

  let diceItems = ObservableCollection<string>()
  let diceList = new ListView()
  let categoryItems = ObservableCollection<string>()
  let categoryList = new ListView()
  categoryList.X <- Pos.Right p2Card + Pos.op_Implicit 1
  categoryList.Y <- 2
  categoryList.Width <- Dim.Absolute categorySelectorWidth
  categoryList.Height <- Dim.Fill 3
  categoryList.SetSource categoryItems

  diceList.X <- Pos.Right categoryList + Pos.op_Implicit 1
  diceList.Y <- 2
  diceList.Width <- Dim.Fill 2
  diceList.Height <- 7
  diceList.SetSource diceItems

  let botLogItems = ObservableCollection<string>()
  let botLog = new ListView()
  botLog.X <- Pos.Right categoryList + Pos.op_Implicit 1
  botLog.Y <- Pos.Bottom diceList + Pos.op_Implicit 1
  botLog.Width <- Dim.Fill 2
  botLog.Height <- Dim.Fill 3
  botLog.SetSource botLogItems
  botLog.CanFocus <- false

  let animStage = new Label()
  animStage.X <- Pos.Right categoryList + Pos.op_Implicit 1
  animStage.Y <- 2
  animStage.Width <- Dim.Fill 2
  animStage.Height <- Dim.Absolute 10
  animStage.Visible <- false
  animStage.CanFocus <- false

  let throwHint = new Label()
  throwHint.Text <- "Press enter to throw"
  throwHint.X <- Pos.Right categoryList + Pos.op_Implicit 1
  throwHint.Y <- Pos.AnchorEnd 2
  throwHint.Visible <- false
  throwHint.CanFocus <- false
  throwHint.SetScheme(boldScheme ()) |> ignore

  let setViewActive active (view: View) =
    view.Enabled <- active
    view.CanFocus <- active

  let focusTarget target =
    match target with
    | DiceList -> diceList.SetFocus() |> ignore
    | CategoryList -> categoryList.SetFocus() |> ignore

  let isInactiveFocus (view: View) =
    view.HasFocus && (not view.Enabled || not view.CanFocus)

  let allDiceKept () =
    match state.Phase with
    | Rolled _ -> List.length keepMask = 5 && List.forall id keepMask
    | _ -> false

  let refreshControlState () =
    let controls = controlState controlsLocked state
    categoryList |> setViewActive (controls.CanRecordCategory && not animating)

    if isInactiveFocus categoryList then
      focusTarget (preferredFocusTarget controls)

  let refresh () =
    if animating then
      let hint =
        if throwing then
          "Rolling the dice…"
        else
          "Shake! ← / → to roll, Enter to throw, Esc to cancel."

      status.Text <- hint
    else
      status.Text <- statusFor mode state

    let controls = controlState controlsLocked state

    if controlsLocked then
      throwHint.Text <- "Bot is playing"
      throwHint.Visible <- true
    elif (controls.CanRoll && not (allDiceKept ())) || (animating && not throwing) then
      throwHint.Text <- "Press enter to throw"
      throwHint.Visible <- true
    else
      throwHint.Visible <- false

    p1Upper.Text <- ScorecardFormat.upperBlock state.Player1
    p1Bonus.Text <- ScorecardFormat.bonusLine state.Player1
    p1Lower.Text <- ScorecardFormat.lowerBlock state.Player1
    p1Total.Text <- ScorecardFormat.totalLine state.Player1
    p2Upper.Text <- ScorecardFormat.upperBlock state.Player2
    p2Bonus.Text <- ScorecardFormat.bonusLine state.Player2
    p2Lower.Text <- ScorecardFormat.lowerBlock state.Player2
    p2Total.Text <- ScorecardFormat.totalLine state.Player2

    p1Card.Title <- cardTitle mode Player1 state.Current
    p2Card.Title <- cardTitle mode Player2 state.Current

    if animating then
      animStage.Text <- DiceArt.renderCup animFaces keepMask animTilt animLabel
      animStage.Visible <- true
    else
      animStage.Visible <- false

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

    let rolledDice = diceIn state |> Option.map fst

    let scoreOf category =
      rolledDice |> Option.map (scoreDice category) |> Option.defaultValue 0

    let bestScore =
      match rolledDice with
      | Some _ -> categoryChoices |> List.map scoreOf |> List.fold max 0
      | None -> 0

    let isBest category = bestScore > 0 && scoreOf category = bestScore

    categoryChoices
    |> List.iter (fun category ->
      let value =
        match rolledDice with
        | None -> "-"
        | Some dice -> string (scoreDice category dice)

      let marker = if isBest category then "*" else " "
      categoryItems.Add(sprintf "%-14s %2s %s" (string category) value marker))

    if categoryItems.Count > 0 then
      let bestIdx =
        categoryChoices |> List.tryFindIndex isBest |> Option.defaultValue 0

      categoryList.SelectedItem <- Nullable bestIdx

    refreshControlState ()

  let lockUi locked =
    controlsLocked <- locked
    refresh ()

    if not locked then
      controlState controlsLocked state |> preferredFocusTarget |> focusTarget

  let appendLog (line: string) =
    botLogItems.Add line
    botLog.SelectedItem <- Nullable(botLogItems.Count - 1)

  let rec scheduleStep (difficulty: Difficulty) =
    status.Text <- "Rolling.."

    Application.AddTimeout(
      TimeSpan.FromSeconds 1.0,
      fun () ->
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

  let currentDiceOrPlaceholder () =
    match diceIn state with
    | Some(dice, _) -> dice
    | None -> List.replicate 5 0

  let beginShake (tilt: DiceArt.Tilt) =
    let controls = controlState controlsLocked state

    if controls.CanRoll && not animating && not (allDiceKept ()) then
      animating <- true
      throwing <- false
      animTilt <- tilt
      animLabel <- "shake"
      animFaces <- shakeFrame roller keepMask (currentDiceOrPlaceholder ())
      diceList.SetFocus() |> ignore
      refresh ()

  let shakeOnce (tilt: DiceArt.Tilt) =
    if animating && not throwing then
      animTilt <- tilt
      animLabel <- "shake"
      animFaces <- shakeFrame roller keepMask (currentDiceOrPlaceholder ())
      refresh ()

  let cancelShake () =
    if animating && not throwing then
      animating <- false
      refresh ()

  let finishThrow () =
    animating <- false
    throwing <- false
    refresh ()
    controlState controlsLocked state |> preferredFocusTarget |> focusTarget

  let rec stepThrow (frameIdx: int) (finalState: GameState) (finalFaces: int list) =
    if frameIdx >= 6 then
      state <- finalState
      animFaces <- finalFaces
      animTilt <- DiceArt.Center
      animLabel <- "landed!"
      refresh ()

      Application.AddTimeout(
        TimeSpan.FromMilliseconds 350.0,
        fun () ->
          finishThrow ()
          false
      )
      |> ignore
    else
      animFaces <- shakeFrame roller keepMask (currentDiceOrPlaceholder ())
      animTilt <- (if frameIdx % 2 = 0 then DiceArt.Left else DiceArt.Right)
      animLabel <- "rolling"
      refresh ()

      Application.AddTimeout(
        TimeSpan.FromMilliseconds 80.0,
        fun () ->
          stepThrow (frameIdx + 1) finalState finalFaces
          false
      )
      |> ignore

  let throwDice () =
    if animating && not throwing then
      let mask =
        match state.Phase with
        | AwaitingFirstRoll -> []
        | Rolled _ -> keepMask

      match roll roller mask state with
      | Ok next ->
        let finalFaces =
          match next.Phase with
          | Rolled(dice, _) -> dice
          | AwaitingFirstRoll -> animFaces

        throwing <- true
        stepThrow 0 next finalFaces
      | Error error ->
        animating <- false
        refresh ()
        status.Text <- string error

  let doRoll () =
    if throwing then ()
    elif animating then throwDice ()
    else beginShake DiceArt.Center

  let doToggleKeep () =
    let controls = controlState controlsLocked state

    if controls.CanChooseDice && not animating then
      let idx =
        if diceList.SelectedItem.HasValue then
          diceList.SelectedItem.Value
        else
          0

      keepMask <- keepMask |> List.mapi (fun i keep -> if i = idx then not keep else keep)
      refresh ()

  let doRecord () =
    let controls = controlState controlsLocked state

    if controls.CanRecordCategory && not animating then
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

  diceList.Accepting.Add(fun args ->
    args.Handled <- true
    doRoll ())

  categoryList.Accepting.Add(fun args ->
    args.Handled <- true
    doRecord ())

  let handleSpace (key: Key) =
    if key.Equals Key.Space && not animating then
      key.Handled <- true
      doToggleKeep ()

  let handleRollKeys (key: Key) =
    if animating && not throwing then
      if key.Equals Key.CursorLeft then
        key.Handled <- true
        shakeOnce DiceArt.Left
      elif key.Equals Key.CursorRight then
        key.Handled <- true
        shakeOnce DiceArt.Right
      elif key.Equals Key.Esc then
        key.Handled <- true
        cancelShake ()

  diceList.KeyDown.Add handleSpace
  diceList.KeyDown.Add handleRollKeys
  frame.KeyDown.Add handleRollKeys

  frame.Add status |> ignore
  frame.Add p1Card |> ignore
  frame.Add p2Card |> ignore
  frame.Add diceList |> ignore
  frame.Add categoryList |> ignore

  frame.Add botLog |> ignore
  frame.Add animStage |> ignore
  frame.Add throwHint |> ignore

  refresh ()
  frame :> View
