module VirtualList.Measurable exposing (..)


type Measurable a
    = Measured a
    | Unmeasured a


value : Measurable a -> a
value measurable =
    case measurable of
        Measured a ->
            a

        Unmeasured a ->
            a
