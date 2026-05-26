module Yacht.UI.GameOverView

open Terminal.Gui.App
open Terminal.Gui.ViewBase
open Terminal.Gui.Views
open Yacht.GameState
open Yacht.Scoring
open Yacht.UI.Scenes

let create (state: GameState) (dispatch: Msg -> unit) : View =
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

  header.Text <-
    sprintf
      "%s  —  Player 1: %d   |   Player 2: %d"
      (string (outcome state))
      (Scorecard.total state.Player1)
      (Scorecard.total state.Player2)

  let p1Card = new FrameView()
  p1Card.Title <- "Player 1"
  p1Card.X <- 1
  p1Card.Y <- 2
  p1Card.Width <- Dim.Percent 48
  p1Card.Height <- Dim.Fill 3

  let p1Label = new Label()
  p1Label.X <- 1
  p1Label.Y <- 0
  p1Label.Width <- Dim.Fill 1
  p1Label.Height <- Dim.Fill 1
  p1Label.Text <- ScorecardFormat.format state.Player1
  p1Card.Add p1Label |> ignore

  let p2Card = new FrameView()
  p2Card.Title <- "Player 2"
  p2Card.X <- Pos.Percent 50
  p2Card.Y <- 2
  p2Card.Width <- Dim.Percent 48
  p2Card.Height <- Dim.Fill 3

  let p2Label = new Label()
  p2Label.X <- 1
  p2Label.Y <- 0
  p2Label.Width <- Dim.Fill 1
  p2Label.Height <- Dim.Fill 1
  p2Label.Text <- ScorecardFormat.format state.Player2
  p2Card.Add p2Label |> ignore

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
