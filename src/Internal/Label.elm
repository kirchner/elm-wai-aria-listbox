module Internal.Label exposing (Label(..))


type Label
    = LabelledBy String
    | Label String
    | NoLabel
