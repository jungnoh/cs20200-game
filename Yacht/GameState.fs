module Yacht.GameState

open Yacht.Scoring

type DiceRoller = unit -> int

type PlayerSlot =
  | Player1
  | Player2

  override this.ToString() =
    match this with
    | Player1 -> "Player 1"
    | Player2 -> "Player 2"

type TurnPhase =
  | AwaitingFirstRoll
  | Rolled of dice: int list * rollsUsed: int

  override this.ToString() =
    match this with
    | AwaitingFirstRoll -> "Roll to start the turn"
    | Rolled(_, rollsUsed) -> sprintf "Rolls used: %d/3" rollsUsed

type GameError =
  | NoRollsLeft
  | NoDiceYet
  | CategoryAlreadyFilled
  | GameAlreadyOver
  | InvalidKeepMask

  override this.ToString() =
    match this with
    | NoRollsLeft -> "No rolls left. Record a category."
    | NoDiceYet -> "Roll before recording a score."
    | CategoryAlreadyFilled -> "That category is already filled."
    | GameAlreadyOver -> "The game is already over."
    | InvalidKeepMask -> "Select exactly five keep flags."

type GameOutcome =
  | InProgress
  | Player1Wins
  | Player2Wins
  | Tie

  override this.ToString() =
    match this with
    | InProgress -> "In progress"
    | Player1Wins -> "Player 1 wins"
    | Player2Wins -> "Player 2 wins"
    | Tie -> "Tie"

type GameState =
  { Player1: Scorecard
    Player2: Scorecard
    Current: PlayerSlot
    Phase: TurnPhase }

let initial: GameState =
  { Player1 = Scorecard.empty
    Player2 = Scorecard.empty
    Current = Player1
    Phase = AwaitingFirstRoll }

let otherSlot slot =
  match slot with
  | Player1 -> Player2
  | Player2 -> Player1

let currentScorecard (state: GameState) : Scorecard =
  match state.Current with
  | Player1 -> state.Player1
  | Player2 -> state.Player2

let isGameOver (state: GameState) : bool =
  Scorecard.isComplete state.Player1 && Scorecard.isComplete state.Player2

let outcome (state: GameState) : GameOutcome =
  if not (isGameOver state) then
    InProgress
  else
    let s1 = Scorecard.total state.Player1
    let s2 = Scorecard.total state.Player2

    if s1 > s2 then Player1Wins
    elif s2 > s1 then Player2Wins
    else Tie

let roll (roller: DiceRoller) (keepMask: bool list) (state: GameState) : Result<GameState, GameError> =
  if isGameOver state then
    Error GameAlreadyOver
  else
    match state.Phase with
    | AwaitingFirstRoll ->
      let dice = [ for _ in 1..5 -> roller () ]
      Ok { state with Phase = Rolled(dice, 1) }
    | Rolled(_, n) when n >= 3 -> Error NoRollsLeft
    | Rolled(currentDice, n) ->
      if List.length keepMask <> 5 then
        Error InvalidKeepMask
      else
        let newDice =
          List.zip currentDice keepMask
          |> List.map (fun (d, keep) -> if keep then d else roller ())

        Ok
          { state with
              Phase = Rolled(newDice, n + 1) }

let record (category: Category) (state: GameState) : Result<GameState, GameError> =
  if isGameOver state then
    Error GameAlreadyOver
  else
    match state.Phase with
    | AwaitingFirstRoll -> Error NoDiceYet
    | Rolled(dice, _) ->
      match Scorecard.record category dice (currentScorecard state) with
      | None -> Error CategoryAlreadyFilled
      | Some newSc ->
        let updated =
          match state.Current with
          | Player1 -> { state with Player1 = newSc }
          | Player2 -> { state with Player2 = newSc }

        Ok
          { updated with
              Current = otherSlot state.Current
              Phase = AwaitingFirstRoll }
