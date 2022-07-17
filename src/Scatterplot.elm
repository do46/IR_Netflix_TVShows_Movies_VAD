module Scatterplot exposing (..)

import Axis
import Html exposing (Html)
import Http
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px)
import Csv
import Csv.Decode
import Browser
import Html exposing (ul)
import Html exposing (li)
import Html.Events exposing (onClick)
import TypedSvg.Core exposing (Svg, text)

main : Program () Model Msg
main =
  Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Laden
    , holenVonCsv ErhalteText
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    case model of
        Fehlschlag ->
            Html.text "Ich konnte Ihre Daten nicht öffnen."

        Laden ->
            Html.text "Daten werden geöffnet..."

        Erfolg l ->
            let
                filteredTitles =
                    filterAndReduceTitles l.data 

                numberTitles =
                    List.length l.data

            in
                Html.div []
                    [ Html.p []
                    [ Html.text "Number of titles: "
                    , Html.text <| String.fromInt numberTitles
                    ]
                        , scatterplot filteredTitles
                    ]
                

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ErhalteText result ->
            case result of
                Ok fullText ->
                    ( Erfolg <| { data = titleListe [ fullText ]}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        
type Model
  = Fehlschlag
  | Laden
  | Erfolg 
    { data : List Title
    }

type Msg
    = ErhalteText (Result Http.Error String)

filterAndReduceTitles : List Title -> XyData
filterAndReduceTitles my_titles =
    XyData "IMDb votes" "IMDb Score" (List.filterMap title2point my_titles)

pointLabel : String -> Float -> Int -> Point
pointLabel title imdb_score imdb_votes =
    Point (title ++ " (" ++ String.fromInt imdb_votes ++ ", " ++ String.fromFloat imdb_score ++ ")") (toFloat imdb_votes) (imdb_score)

title2point : Title -> Maybe Point
title2point title =
    Maybe.map3 pointLabel (Just title.title) (Just title.imdb_score) (Just title.imdb_votes) 

holenVonCsv : (Result Http.Error String -> Msg) -> Cmd Msg
holenVonCsv x = 
    liste
        |> List.map
            (\dataset ->
                Http.get
                    { url = "https://raw.githubusercontent.com/do46/IR_Netflix_TVShows_Movies_VAD/main/Data/AufbereiteteDaten/" ++ dataset                    
                    , expect = Http.expectString ErhalteText
                    }
            )
        |> Cmd.batch

liste : List String
liste =
    [ "titleslesslessdf.csv"]

csvStringZuDaten : String -> List Title
csvStringZuDaten csvRoh =
    Csv.parse csvRoh
        |> Csv.Decode.decodeCsv dekodierenTitle
        |> Result.toMaybe
        |> Maybe.withDefault []

dekodierenTitle : Csv.Decode.Decoder (Title -> a) a
dekodierenTitle =
    Csv.Decode.map Title
        (Csv.Decode.field "id" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "title" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "type" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "release_year" (String.toInt >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "runtime"(String.toInt >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "imdb_score"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "imdb_votes"(String.toInt >> Result.fromMaybe "error parsing string"))
        )

titleListe :List String -> List Title
titleListe liste1 =
    List.map(\t -> csvStringZuDaten t) liste1
        |> List.concat

type alias Point =
    { pointName : String, x : Float, y : Float }

type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }


type alias Title =
    { id : String
    , title : String
    , typ : String
    , release_year : Int
    , runtime : Int
    , imdb_score : Float
    , imdb_votes : Int
    }



scatterplot : XyData -> Svg msg
scatterplot model =
    let
        
        xValues : List Float
        xValues =
            List.map .x model.data

        yValues : List Float
        yValues =
            List.map .y model.data

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
   
    in
    -- output als svg => scatter plot
    
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
    -- plot x axis    
         , g[ transform [ Translate (60) (390)]]
            [
                xAxis xValues
                , text_
                [ x (Scale.convert xScaleLocal labelPositions.x)
                , y 35

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ text model.xDescription ]
                ]
    -- plot y axis             
         ,g[transform [Translate(60) (60)]]
         [
             yAxis yValues
             , text_
                [ x -30
                , y -30

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ text model.yDescription ]
             ]
    -- plot points and description     
         ,g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
            -- map data with the defined variables
        ]

point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY xyPoint =
    g
        [ class [ "point" ]
        , fontSize <| Px 10.0
        , fontFamily [ "sans-serif" ]
        , transform
            [ Translate
                (Scale.convert scaleX xyPoint.x) -- x
                (Scale.convert scaleY xyPoint.y) -- y
-- Given a value from the domain, returns the corresponding value from 
-- the range. If the given value is outside the domain the mapping may 
-- be extrapolated such that the returned value is outside the range.
            ]
            -- Verschieben entlang der x/y-Achse
        ]
        -- Formatierung von class "point"
        [ circle [ cx 0, cy 0, r 5 ] []
        , text_ [ x 10, y -20, textAnchor AnchorMiddle ] [ Html.text xyPoint.pointName ]
        ]
w : Float
w =
    900
-- width

h : Float
h =
    450
-- height

padding : Float
padding =
    60
-- 

radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5

defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )
 
addieren : (Float, Float) -> Float-> (Float, Float) 
addieren (min, max) shift =
    if min >= 0 then
        ( 0, max + shift)
    else 
        (min - shift, max + shift)
-- shift the scale

wideExtent : List Float -> ( Float, Float )
wideExtent values = 
    let
        result = 
            Maybe.withDefault (0, 0)
            (Statistics.extent values)
        -- Statistics.extent: Returns the minimum and maximum value in the list
        max =          
            Maybe.withDefault (0)
            (List.maximum values)
            
        result1 = 
            
            addieren result (max*0.1)
        
          
    in
     result1
    
xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)
--draw ticks under axis

yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)
--draw ticks in the left of the axis