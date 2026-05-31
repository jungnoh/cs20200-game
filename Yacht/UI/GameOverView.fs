module Yacht.UI.GameOverView

open Terminal.Gui.App
open Terminal.Gui.Drawing
open Terminal.Gui.ViewBase
open Terminal.Gui.Views
open Yacht.GameState
open Yacht.Scoring
open Yacht.UI.Scenes

let create (state: GameState) (slotLabels: (string * string) option) (dispatch: Msg -> unit) : View =
  let p1Name, p2Name = slotLabels |> Option.defaultValue ("Player 1", "Player 2")
  let frame = new FrameView()
  frame.Title <- "Game Over"
  frame.X <- Pos.Center()
  frame.Y <- Pos.Center()
  frame.Width <- Dim.Percent 90
  frame.Height <- Dim.Percent 85

  let header = new Label()
  header.X <- 1
  header.Y <- 0
  header.Width <- Dim.Fill 2

  let winsText name =
    if name = "You" then sprintf "%s win" name else sprintf "%s wins" name

  let outcomeText =
    match outcome state with
    | Player1Wins -> winsText p1Name
    | Player2Wins -> winsText p2Name
    | Tie -> "Tie"
    | InProgress -> "In progress"

  header.Text <-
    sprintf
      "%s  —  %s: %d   |   %s: %d"
      outcomeText
      p1Name
      (Scorecard.total state.Player1)
      p2Name
      (Scorecard.total state.Player2)

  let boldScheme () =
    let mutable fg = Color.None
    let mutable bg = Color.None
    let mutable style = TextStyle.Bold
    Scheme(Attribute(&fg, &bg, &style))

  let addScorecardLabels (card: FrameView) (sc: Scorecard) =
    let upper = new Label()
    upper.X <- 1
    upper.Y <- 0
    upper.Width <- Dim.Fill 1
    upper.CanFocus <- false
    upper.Text <- ScorecardFormat.upperBlock sc

    let bonus = new Label()
    bonus.X <- 1
    bonus.Y <- Pos.Bottom upper
    bonus.Width <- Dim.Fill 1
    bonus.CanFocus <- false
    bonus.Text <- ScorecardFormat.bonusLine sc
    bonus.SetScheme(boldScheme ()) |> ignore

    let lower = new Label()
    lower.X <- 1
    lower.Y <- Pos.Bottom bonus
    lower.Width <- Dim.Fill 1
    lower.CanFocus <- false
    lower.Text <- ScorecardFormat.lowerBlock sc

    let total = new Label()
    total.X <- 1
    total.Y <- Pos.Bottom lower
    total.Width <- Dim.Fill 1
    total.CanFocus <- false
    total.Text <- ScorecardFormat.totalLine sc
    total.SetScheme(boldScheme ()) |> ignore

    card.Add upper |> ignore
    card.Add bonus |> ignore
    card.Add lower |> ignore
    card.Add total |> ignore

  let p1Card = new FrameView()
  p1Card.Title <- p1Name
  p1Card.X <- 1
  p1Card.Y <- 2
  p1Card.Width <- Dim.Percent 48
  p1Card.Height <- Dim.Fill 3
  p1Card.CanFocus <- false
  addScorecardLabels p1Card state.Player1

  let p2Card = new FrameView()
  p2Card.Title <- p2Name
  p2Card.X <- Pos.Percent 50
  p2Card.Y <- 2
  p2Card.Width <- Dim.Percent 48
  p2Card.Height <- Dim.Fill 3
  p2Card.CanFocus <- false
  addScorecardLabels p2Card state.Player2

  let backButton = new Button()
  backButton.Text <- "Back to Menu"
  backButton.X <- Pos.Center()
  backButton.Y <- Pos.AnchorEnd 2
  backButton.Accepting.Add(fun _ -> dispatch BackToMenu)

  frame.Add header |> ignore
  frame.Add p1Card |> ignore
  frame.Add p2Card |> ignore
  frame.Add backButton |> ignore
  frame :> View
