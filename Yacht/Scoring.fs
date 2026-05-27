module Yacht.Scoring

type Category =
  | Aces
  | Deuces
  | Threes
  | Fours
  | Fives
  | Sixes
  | Choice
  | FourOfAKind
  | FullHouse
  | SmallStraight
  | LargeStraight
  | Yacht

  override this.ToString() =
    match this with
    | Aces -> "Aces"
    | Deuces -> "Deuces"
    | Threes -> "Threes"
    | Fours -> "Fours"
    | Fives -> "Fives"
    | Sixes -> "Sixes"
    | Choice -> "Choice"
    | FourOfAKind -> "Four of a Kind"
    | FullHouse -> "Full House"
    | SmallStraight -> "Small Straight"
    | LargeStraight -> "Large Straight"
    | Yacht -> "Yacht"

let allCategories =
  [ Aces
    Deuces
    Threes
    Fours
    Fives
    Sixes
    Choice
    FourOfAKind
    FullHouse
    SmallStraight
    LargeStraight
    Yacht ]

let upperCategories = [ Aces; Deuces; Threes; Fours; Fives; Sixes ]

let private upperFace category =
  match category with
  | Aces -> Some 1
  | Deuces -> Some 2
  | Threes -> Some 3
  | Fours -> Some 4
  | Fives -> Some 5
  | Sixes -> Some 6
  | _ -> None

let private faceCounts (dice: int list) : int list =
  dice |> List.countBy id |> List.map snd |> List.sortDescending

let private distinctSorted (dice: int list) : int list = dice |> List.distinct |> List.sort

let private containsRun (dice: int list) (run: int list) : bool =
  let set = Set.ofList dice
  run |> List.forall (fun v -> Set.contains v set)

let scoreDice (category: Category) (dice: int list) : int =
  match category with
  | Aces
  | Deuces
  | Threes
  | Fours
  | Fives
  | Sixes ->
    let face = (upperFace category).Value
    dice |> List.filter ((=) face) |> List.sum
  | Choice -> List.sum dice
  | FourOfAKind ->
    let counts = faceCounts dice
    if List.head counts >= 4 then List.sum dice else 0
  | FullHouse ->
    let counts = faceCounts dice
    if counts = [ 3; 2 ] then List.sum dice else 0
  | SmallStraight ->
    let runs = [ [ 1; 2; 3; 4 ]; [ 2; 3; 4; 5 ]; [ 3; 4; 5; 6 ] ]
    if runs |> List.exists (containsRun dice) then 15 else 0
  | LargeStraight ->
    let sorted = distinctSorted dice

    if sorted = [ 1; 2; 3; 4; 5 ] || sorted = [ 2; 3; 4; 5; 6 ] then
      30
    else
      0
  | Yacht ->
    let counts = faceCounts dice
    if List.head counts = 5 then 50 else 0

type Scorecard = { Scores: Map<Category, int> }

module Scorecard =

  let empty: Scorecard = { Scores = Map.empty }

  let isFilled (category: Category) (sc: Scorecard) : bool = Map.containsKey category sc.Scores

  let unfilledCategories (sc: Scorecard) : Category list =
    allCategories |> List.filter (fun c -> not (isFilled c sc))

  let isComplete (sc: Scorecard) : bool =
    allCategories |> List.forall (fun c -> isFilled c sc)

  let categoryScore (category: Category) (sc: Scorecard) : int option = Map.tryFind category sc.Scores

  let record (category: Category) (dice: int list) (sc: Scorecard) : Scorecard option =
    if isFilled category sc then
      None
    else
      let score = scoreDice category dice
      Some { Scores = Map.add category score sc.Scores }

  let applicableCategories (dice: int list) (sc: Scorecard) : Category list =
    sc
    |> unfilledCategories
    |> List.filter (fun c -> c = Choice || scoreDice c dice > 0)

  let upperSubtotal (sc: Scorecard) : int =
    upperCategories
    |> List.sumBy (fun c -> Map.tryFind c sc.Scores |> Option.defaultValue 0)

  let upperBonus (sc: Scorecard) : int =
    if upperSubtotal sc >= 63 then 35 else 0

  let total (sc: Scorecard) : int =
    let recorded = sc.Scores |> Map.toSeq |> Seq.sumBy snd
    recorded + upperBonus sc
