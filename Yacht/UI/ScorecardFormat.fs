module Yacht.UI.ScorecardFormat

open Yacht.Scoring

let private cellValue (sc: Scorecard) (category: Category) : string =
  match Scorecard.categoryScore category sc with
  | Some n -> string n
  | None -> "-"

let format (sc: Scorecard) : string =
  let categoryRows =
    allCategories
    |> List.map (fun c -> sprintf "%-16s %4s" (string c) (cellValue sc c))

  let summaryRows =
    [ sprintf "%-16s %4d" "Upper sub" (Scorecard.upperSubtotal sc)
      sprintf "%-16s %4d" "Bonus" (Scorecard.upperBonus sc)
      sprintf "%-16s %4d" "Total" (Scorecard.total sc) ]

  String.concat "\n" (categoryRows @ summaryRows)
