module Yacht.UI.DiceArt

type Tilt =
  | Left
  | Right
  | Center

let private pipRows (face: int) : string list =
  match face with
  | 1 -> [ "   "; " ● "; "   " ]
  | 2 -> [ "●  "; "   "; "  ●" ]
  | 3 -> [ "●  "; " ● "; "  ●" ]
  | 4 -> [ "● ●"; "   "; "● ●" ]
  | 5 -> [ "● ●"; " ● "; "● ●" ]
  | 6 -> [ "● ●"; "● ●"; "● ●" ]
  | _ -> [ "   "; "   "; "   " ]

let private top = "┌───┬───┬───┬───┬───┐"
let private bottom = "└───┴───┴───┴───┴───┘"

/// Render five dice as a tumbling "cup" of pip-art, 8 lines tall.
/// `tilt` leans the shake arrows left/right; `label` is the header word.
let renderCup (faces: int list) (keepMask: bool list) (tilt: Tilt) (label: string) : string =
  let face i =
    if i < List.length faces then faces[i] else 0

  let kept i = i < List.length keepMask && keepMask[i]

  let gridRow r =
    let cells = [ for i in 0..4 -> List.item r (pipRows (face i)) ]
    "│" + String.concat "│" cells + "│"

  let numbers =
    let cells = [ for i in 0..4 -> sprintf " %d " (i + 1) ]
    " " + String.concat " " cells + " "

  let held =
    let cells = [ for i in 0..4 -> if kept i then " ▲ " else "   " ]
    " " + String.concat " " cells + " "

  let arrows =
    match tilt with
    | Left -> "«««"
    | Right -> "»»»"
    | Center -> "‹ ›"

  let header = sprintf "  %s  %s  %s" arrows label arrows

  [ header; top; gridRow 0; gridRow 1; gridRow 2; bottom; numbers; held ]
  |> String.concat "\n"
