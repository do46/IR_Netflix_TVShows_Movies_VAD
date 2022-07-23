module Scatterplot exposing (..)

import Axis
import Html exposing (Html, button, div, option, select)
import Html.Attributes exposing (id, value)
import Http
import Scale exposing (ContinuousScale)
import TypedSvg exposing (circle, g, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px)
import Browser
import Html exposing (ul,li)
import Html.Events exposing (onClick)
import TypedSvg.Core exposing (Svg, text)
import List.Extra
import Html exposing (p)
import Html exposing (h1)
import Data exposing (DB(..), Title, defaultExtent, padding, h, w, tickCount, titleListe)
import Data exposing (wideExtent)

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
    , holenVonCsv GotText 0
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    case model of
        Fehlschlag ->
            text "Ich konnte Ihre Daten nicht öffnen."

        Laden ->
            text "Daten werden geöffnet..."

        Erfolg l ->
            let
                filteredTitles =
                    filterAndReduceTitles l.data 

                numberTitles =
                    List.length l.data

            in
                div []
                    [ h1 []
                    [ text "1. Scatterplot"
                    ]
                    , p []
                    [ text "Number of titles: "
                    , text <| String.fromInt numberTitles
                    ]
                    , p []
                    [ text "Please choose the database:"
                    ]
                    , p []
                    [ button [onClick(ChangeDB(All))][text "All"]
                    , button [onClick(ChangeDB(Movies))][text "Movies"]
                    , button [onClick(ChangeDB(Series))][text "Series"]
                    ]
                    , p []
                    [ text "Please choose the data type to visualize:"
                    ]
                    , p []
                    [ select [ id "my-id" , Html.Events.onInput DropDown]
                        [ option [ value "a" ] [ text "IMDb votes and IMDb score" ]
                        , option [ value "b" ] [ text "TMDb popularity and TMDb score" ]
                        , option [ value "c" ] [ text "Runtime and IMDb score" ] 
                        , option [ value "d" ] [ text "Runtime and TMDb score" ] 
                        , option [ value "e" ] [ text "Runtime and TMDb popularity" ] 
                        , option [ value "f" ] [ text "Number of seasons and TMDb score" ] 
                        , option [ value "g" ] [ text "Number of seasons and IMDb score" ] 
                        , option [ value "h" ] [ text "Number of seasons and TMDb popularity" ]    
                        ]
                    ]
                    , scatterplot filteredTitles l.att
                    ]
                
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Erfolg <| { data = Data.titleListe [fullText] , att = IMVS }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeDB para ->
            case para of
                All ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data , att = m.att},holenVonCsv GotText 0)
                        _ ->
                            ( model, Cmd.none )
                   
                Movies ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, att = m.att},holenVonCsv GotText 1)
                        _ ->
                            ( model, Cmd.none )
                Series ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, att = m.att},holenVonCsv GotText 2)
                        _ ->
                            ( model, Cmd.none )
        DropDown str ->
            case str of
                "a" ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, att = IMVS}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                "b" ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, att = TMPS}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                "c" ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, att = IMRS}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                "d" ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, att = TMRS}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                "e" ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, att = TMRP}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                "f" ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, att = TMSS}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                "g" ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, att = IMSS}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                "h" ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, att = TMSP}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                _ ->
                    (model, Cmd.none) 
                
holenVonCsv : (Result Http.Error String -> Msg) -> Int -> Cmd Msg
holenVonCsv x db = 
    (List.Extra.getAt db Data.liste) |> Maybe.withDefault("titleslesslessdf.csv")|> String.words
        |> List.map
            (\dataset ->
                Http.get
                    { url = "https://raw.githubusercontent.com/do46/IR_Netflix_TVShows_Movies_VAD/main/Data/AufbereiteteDaten/" ++ dataset                   
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch


type Model
  = Fehlschlag
  | Laden
  | Erfolg 
    { data : List Title
    , att : Att  
    }

type Msg
    = GotText (Result Http.Error String)
    | ChangeDB (Data.DB)
    | DropDown String


filterAndReduceTitles : List Title -> XyData
filterAndReduceTitles my_titles =
    XyData <| List.filterMap title2point my_titles -- xmts

pointLabel : String -> Float -> Float -> Float -> Float -> Float -> Float -> Point -- xmts
pointLabel title imdb_score imdb_votes runtime tmdb_popularity tmdb_score seasons = -- xmts
    Point (title ++ " (" ++ String.fromFloat imdb_votes ++ ", " ++ String.fromFloat imdb_score
        ++ String.fromFloat runtime ++ ", " ++ String.fromFloat tmdb_popularity ++ ", " 
        ++ String.fromFloat tmdb_score ++ ", " ++ String.fromFloat seasons ++")") 
        (imdb_votes) (imdb_score) (runtime) (tmdb_popularity) (tmdb_score) (seasons) -- xmts

andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap = Maybe.map2 (|>)

title2point : Title -> Maybe Point
title2point title =
    Maybe.map pointLabel 
        (Just title.title) 
            |> andMap (Just title.imdb_score) 
            |> andMap (Just title.imdb_votes)
            |> andMap (Just title.runtime)
            |> andMap (Just title.tmdb_popularity)
            |> andMap (Just title.tmdb_score)
            |> andMap (Just title.seasons) 
-- ???


-- xmts



attToString : Att -> List String
attToString att = 
    case att of
        IMVS -> ["IMDb votes","IMDb score"] -- imdb vote vs score
        TMPS -> ["TMDb popularity","TMDb score"] -- tmdb pop vs score
        IMRS -> ["Runtime","IMDb score"] -- imdb runtime vs score
        TMRS -> ["Runtime","TMDb score"] -- tmdb runtime vs score
        TMRP -> ["Runtime","TMDb popularity"] -- tmdb runtime vs pop
        TMSS -> ["Seasons","TMDb score"] -- tmdb season vs score
        IMSS -> ["Seasons","IMDb score"] -- imdb season vs score
        TMSP -> ["Seasons","TMDb popularity"] -- imdb season vs pop

type Att
    = IMVS -- imdb vote vs score
    | TMPS -- tmdb pop vs score
    | IMRS -- imdb runtime vs score
    | TMRS -- tmdb runtime vs score
    | TMRP -- tmdb runtime vs pop
    | TMSS -- tmdb season vs score
    | IMSS -- imdb season vs score
    | TMSP -- tmdb season vs pop

scatterplot : XyData -> Att -> Svg msg
scatterplot model att =
    let 
        xValues : List Float
        xValues =
            List.map .x model.data -- x

        yValues : List Float
        yValues =
            List.map .y model.data -- y

        zValues : List Float
        zValues =
            List.map .z model.data -- z

        aValues : List Float
        aValues =
            List.map .a model.data -- a
            
        bValues : List Float
        bValues =
            List.map .b model.data -- b

        cValues : List Float
        cValues =
            List.map .c model.data -- c

        dataPoint : ( List Float, List Float )
        dataPoint =
            case att of
                IMVS -> ( xValues, yValues ) -- imdb vote vs score
                TMPS -> ( aValues, bValues ) -- tmdb pop vs score
                IMRS -> ( zValues, yValues ) -- imdb runtime vs score
                TMRS -> ( zValues, bValues ) -- tmdb runtime vs score
                TMRP -> ( zValues, aValues ) -- tmdb runtime vs pop
                TMSS -> ( cValues, bValues ) -- tmdb season vs score
                IMSS -> ( cValues, yValues ) -- imdb season vs score
                TMSP -> ( cValues, aValues ) -- imdb season vs pop

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale <| Tuple.first(dataPoint)

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale <| Tuple.second(dataPoint)

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) *8/9

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = (wideExtent (Tuple.first(dataPoint)) |> half)
            , y = (wideExtent (Tuple.second(dataPoint)) |> Tuple.second)
            }
   
    in
    -- output als svg => scatter plot
    
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: black; fill: rgba(255, 255, 255, 0.5); }
            .point text { display: none; }
            .point:hover circle { stroke: black; fill: rgba(4, 244, 251, 1); }
            .point:hover text { display: inline; }
          """ ]
    -- plot x axis    
         , g[ transform [ Translate (60) (390)]]
            [
                xAxis (Tuple.first(dataPoint))
                , text_
                [ x (Scale.convert xScaleLocal labelPositions.x)
                , y 35

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 15)

                --, fontWeight FontWeightBold
                ]
                [ text <| Maybe.withDefault("") <| List.Extra.getAt 0 <| attToString att ] -- name x
                ]
    -- plot y axis             
         ,g[ transform [Translate(60) (60)]]
           [
             yAxis (Tuple.second(dataPoint))
             , text_
                [ x -30
                , y -30

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 15)

                --, fontWeight FontWeightBold
                ]
                [ text <| Maybe.withDefault("") <| List.Extra.getAt 1 <| attToString att ] -- name y
             ]
    -- plot points and description     
         
         ,g [ transform [ Translate padding padding ] ]
            (List.map (point att xScaleLocal yScaleLocal) model.data)
        ]

point : Att -> ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point att scaleX scaleY xyPoint =
    let
        dataPoint : ( Float, Float )
        dataPoint =
            case att of
                IMVS -> ( xyPoint.x, xyPoint.y ) -- imdb vote vs score
                TMPS -> ( xyPoint.a, xyPoint.b ) -- tmdb pop vs score
                IMRS -> ( xyPoint.z, xyPoint.y ) -- imdb runtime vs score
                TMRS -> ( xyPoint.z, xyPoint.b ) -- tmdb runtime vs score
                TMRP -> ( xyPoint.z, xyPoint.a ) -- tmdb runtime vs pop
                TMSS -> ( xyPoint.c, xyPoint.b ) -- tmdb season vs score
                IMSS -> ( xyPoint.c, xyPoint.y ) -- imdb season vs score
                TMSP -> ( xyPoint.c, xyPoint.a ) -- imdb season vs pop
    in
    
        g
            [ class [ "point" ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            , transform
                [ Translate
                    (Scale.convert scaleX <| Tuple.first(dataPoint)) 
                    (Scale.convert scaleY <| Tuple.second(dataPoint)) 
                ]
                -- Verschieben entlang der x/y-Achse
            ]
            -- Formatierung von class "point"
            [ circle [ cx 0, cy 0, r 5 ] []
            , text_ [ x 10, y -20, textAnchor AnchorMiddle ] [ Html.text xyPoint.pointName ]
            ]


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( Data.wideExtent values )


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( Data.wideExtent values )
 

    
xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

type alias Point =
    { pointName : String, x : Float, y : Float, z : Float, a : Float, b : Float , c : Float } -- xmts

type alias XyData =
    {  data : List Point
    }