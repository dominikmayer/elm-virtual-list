module VirtualList.Config exposing
    ( Config, default
    , setListId, setInitialHeight, setDefaultItemHeight, setShowListDuringMeasurement, setBuffer, setDynamicBuffer
    , getListId, getInitialHeight, getDefaultItemHeight, getShowListDuringMeasurement, getBuffer, getDynamicBuffer
    )

{-| **Configure a virtual list** with settings for size estimation, buffering, and visibility during measurement.

@docs Config, default


# Setters

You can **combine** the following setters with the pipe operator:

    VirtualList.Config.default
        |> VirtualList.Config.setInitialHeight 650
        |> VirtualList.Config.setBuffer 10

@docs setListId, setInitialHeight, setDefaultItemHeight, setShowListDuringMeasurement, setBuffer, setDynamicBuffer


# Getters

You will probably have **no need** to access these values directly.

@docs getListId, getInitialHeight, getDefaultItemHeight, getShowListDuringMeasurement, getBuffer, getDynamicBuffer

-}


{-| Defines the **configuration** for a virtual list, including estimated item heights, buffer size, and visibility settings during measurement.

Start with [default](VirtualList.Config#default) and customize it using setter **functions:**

    VirtualList.Config.default
        |> VirtualList.Config.setInitialHeight 650
        |> VirtualList.Config.setBuffer 10

-}
type Config
    = Config InternalConfig


type alias InternalConfig =
    { listId : String
    , initialHeight : Float
    , defaultItemHeight : Float
    , showListDuringMeasurement : Bool
    , buffer : Int
    , dynamicBuffer : Bool
    }


{-| Returns a **default configuration** with reasonable initial values.
-}
default : Config
default =
    Config internalDefault


internalDefault : InternalConfig
internalDefault =
    { listId = "virtual-list"
    , initialHeight = 500
    , defaultItemHeight = 26
    , showListDuringMeasurement = False
    , buffer = 5
    , dynamicBuffer = True
    }


{-| Sets the ID of the **list container** in the DOM.

Uses the **default ID** if an empty string is provided: `"virtual-list"`

-}
setListId : String -> Config -> Config
setListId listId (Config config) =
    if String.isEmpty listId then
        Config { config | listId = internalDefault.listId }

    else
        Config { config | listId = listId }


{-| Defines the **estimated list height** before measurement.

Uses the **default** if negative: `500`

-}
setInitialHeight : Float -> Config -> Config
setInitialHeight newHeight (Config config) =
    Config
        { config
            | initialHeight =
                ensurePositiveOr internalDefault.initialHeight newHeight
        }


{-| Sets the **estimated item height** before measurement.

Uses the **default** if negative: **Default:** `26`

-}
setDefaultItemHeight : Float -> Config -> Config
setDefaultItemHeight newHeight (Config config) =
    Config
        { config
            | defaultItemHeight =
                ensurePositiveOr internalDefault.defaultItemHeight newHeight
        }


{-| Controls whether the list remains **visible during measurement.**

If `True`, it may cause **spacing issues.**

**Default:** `False`

-}
setShowListDuringMeasurement : Bool -> Config -> Config
setShowListDuringMeasurement showList (Config conf) =
    Config { conf | showListDuringMeasurement = showList }


{-| Defines how many **extra items** are rendered outside the visible range for smoother scrolling.

**Default:** `5`

-}
setBuffer : Int -> Config -> Config
setBuffer newBuffer (Config conf) =
    Config { conf | buffer = ensurePositive newBuffer }


{-| Enables dynamic buffer sizing based on **scroll speed.**

**Default:** `True`

-}
setDynamicBuffer : Bool -> Config -> Config
setDynamicBuffer newBuffer (Config conf) =
    Config { conf | dynamicBuffer = newBuffer }


ensurePositive : number -> number
ensurePositive number =
    ensurePositiveOr 0 number


ensurePositiveOr : number -> number -> number
ensurePositiveOr fallback number =
    if number >= 0 then
        number

    else
        fallback


{-| Returns the **list container’s ID.**
-}
getListId : Config -> String
getListId (Config config) =
    config.listId


{-| Returns the list’s **estimated height** before measurement.
-}
getInitialHeight : Config -> Float
getInitialHeight (Config config) =
    config.initialHeight


{-| Returns the **estimated item height** before measurement.
-}
getDefaultItemHeight : Config -> Float
getDefaultItemHeight (Config config) =
    config.defaultItemHeight


{-| Returns whether the list remains **visible during measurement,** which may affect layout.
-}
getShowListDuringMeasurement : Config -> Bool
getShowListDuringMeasurement (Config config) =
    config.showListDuringMeasurement


{-| Returns the number of **extra items** rendered outside the visible area.
-}
getBuffer : Config -> Int
getBuffer (Config config) =
    config.buffer


{-| Returns whether buffer sizing is dynamic based on **scroll speed.**
-}
getDynamicBuffer : Config -> Bool
getDynamicBuffer (Config config) =
    config.dynamicBuffer
