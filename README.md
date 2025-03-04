# Virtual List

VirtualList is an Elm module for **efficiently displaying large lists** by rendering only the visible items within the viewport, plus a configurable buffer. It dynamically measures the height of displayed elements to ensure smooth scrolling and performance.

## Features

- **Virtualization:** Only renders visible items for improved performance.
- **Dynamic Measurement:** Adjusts heights dynamically for accurate layout.
- **Configurable Buffering:** Allows fine-tuning for smooth scrolling.
- **Supports Scrolling to Items:** Provides methods to navigate programmatically.

## Alternative

In case you **know the heights in advance** you might get a better performance by using [`FabienHenon/elm-infinite-list-view`](https://package.elm-lang.org/packages/FabienHenon/elm-infinite-list-view/latest/).

## Installation

In your **project directory** run:

```sh
elm install dominikmayer/elm-virtual-list
```

## Usage

### Model Setup

Include `VirtualList.Model` in your app's **model:**

```elm
import VirtualList

type alias Model =
    { virtualList : VirtualList.Model
    -- other fields
    }

defaultModel : Model
defaultModel =
    { virtualList = VirtualList.init
    -- other fields
    }
```

### Update Function

Include `VirtualList.Msg` in your app’s **update function:**

```elm
type Msg
    = VirtualListMsg VirtualList.Msg
    -- other messages

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VirtualListMsg virtualListMsg ->
            let
                ( virtualListModel, virtualListCmd ) =
                    VirtualList.update virtualListMsg model.virtualList
            in
                ( { model | virtualList = virtualListModel }, Cmd.map VirtualListMsg virtualListCmd )
        -- other cases
```

### Rendering

Render the virtual list in your **view:**

```elm
view : Model -> Html Msg
view model =
    VirtualList.view (renderRow model) model.virtualList VirtualListMsg

renderRow : Model -> String -> Html Msg
renderRow model id =
    div [] [text id]
```

### Example

Check out [Obsidian Note ID](https://github.com/dominikmayer/obsidian-note-id) for an example implementation.
