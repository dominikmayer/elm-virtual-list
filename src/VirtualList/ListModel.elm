module VirtualList.ListModel exposing (ListModel(..), init, setVisibility)

import VirtualList.Config as Config exposing (Config)
import VirtualList.Constants as Constants
import VirtualList.Measurable as Measurable exposing (Measurable(..))


type ListModel
    = ListModel InternalList


type alias InternalList =
    { isVisible : Bool
    }


init : Config -> ListModel
init config =
    ListModel
        { isVisible = Config.getShowListDuringMeasurement config
        }


setVisibility : Bool -> ListModel -> ListModel
setVisibility visibility (ListModel list) =
    ListModel { list | isVisible = visibility }
