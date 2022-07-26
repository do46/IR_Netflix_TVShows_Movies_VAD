module Main exposing (..)

import Browser
import Html
import Html.Events exposing (onClick)
import IconPlot
import ParallelCoordinates
import Scatterplot
import Text


type alias Model =
    { 
     scatterplotModel : Scatterplot.Model
    , parallelCoordinatesModel : ParallelCoordinates.Model
    , iconPlotModel : IconPlot.Model
    , textModel : Text.Model
    , active : Active
    }

type Active
    = Text
    | Scatterplot
    | ParallelCoordinates
    | IconPlot


type Msg
    = TextMsg
    | ScatterplotMsg Scatterplot.Msg
    | ParallelCoordinatesMsg ParallelCoordinates.Msg
    | IconPlotMsg IconPlot.Msg
    | SwitchView Active


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    Model Scatterplot.init ParallelCoordinates.init IconPlot.init  Text.main Text

--

view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.button [ onClick (SwitchView Scatterplot) ] [ Html.text "Scatterplot" ]
        , Html.button [ onClick (SwitchView ParallelCoordinates) ] [ Html.text "Parallel Coordinates" ]
        , case model.active of
            Scatterplot ->
                Html.map ScatterplotMsg (Scatterplot.view model.scatterplotModel)

            ParallelCoordinates ->
                Html.map ParallelCoordinatesMsg (ParallelCoordinates.view model.parallelCoordinatesModel)
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextMsg ->
            { model | textModel = Text.String}
        
        ScatterplotMsg scatterplotMsg ->
            { model | scatterplotModel = Scatterplot.update scatterplotMsg model.scatterplotModel }

        ParallelCoordinatesMsg parallelCoordinatesMsg ->
            { model | parallelCoordinatesModel = ParallelCoordinates.update ParallelCoordinatesMsg model.parallelCoordinatesModel }

        SwitchView newActitve ->
            { model | active = newActitve }
