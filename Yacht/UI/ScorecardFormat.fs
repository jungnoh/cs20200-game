module Yacht.UI.ScorecardFormat

open Yacht.Scoring

let private cellValue (sc: Scorecard) (category: Category) : string =
  match Scorecard.categoryScore category sc with
  | Some n -> string n
  | None -> "-"

let private bonusRow (sc: Scorecard) =
  sprintf "%-16s %4d" "Bonus" (Scorecard.upperBonus sc)

let private totalRow (sc: Scorecard) =
  sprintf "%-16s %4d" "Total" (Scorecard.total sc)

let private overflowRow = "↓ more scores"

let private divider = String.replicate 21 "-"

let private scorecardRows (sc: Scorecard) =
  let upperRows = upperCategories |> List.map (fun c -> sprintf "%-16s %4s" (string c) (cellValue sc c))

  let lowerRows =
    allCategories
    |> List.filter (fun c -> not (List.contains c upperCategories))
    |> List.map (fun c -> sprintf "%-16s %4s" (string c) (cellValue sc c))

  [ yield! upperRows
    divider
    bonusRow sc
    divider
    yield! lowerRows
    divider
    totalRow sc ]

let format (sc: Scorecard) : string =
  String.concat "\n" (scorecardRows sc)

let formatForHeight (maxRows: int) (sc: Scorecard) : string =
  let rows = scorecardRows sc

  if maxRows >= List.length rows then
    String.concat "\n" rows
  elif maxRows <= 0 then
    ""
  else
    match maxRows with
    | 1 -> totalRow sc
    | 2 -> String.concat "\n" [ overflowRow; totalRow sc ]
    | 3 -> String.concat "\n" [ bonusRow sc; overflowRow; totalRow sc ]
    | _ ->
      let rowsBeforeTotal = rows |> List.filter (fun row -> row <> totalRow sc)
      let prefix = rowsBeforeTotal |> List.truncate (maxRows - 2)

      let visibleRows =
        if List.contains (bonusRow sc) prefix then
          prefix
        else
          let prefixWithoutBonus = rowsBeforeTotal |> List.filter (fun row -> row <> bonusRow sc) |> List.truncate (maxRows - 3)
          prefixWithoutBonus @ [ bonusRow sc ]

      String.concat "\n" (visibleRows @ [ overflowRow; totalRow sc ])
