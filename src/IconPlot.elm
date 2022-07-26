module IconPlot exposing (..)

import Axis
import Html exposing (Html)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (circle, g, polyline, style, svg, text_)
import TypedSvg.Attributes as TSA exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types as TST exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px) 
import Data exposing (DB(..), Title, defaultExtent, padding, h, w, tickCount, titleListe, wideExtent)
import Color
import Browser
import Html exposing (div, h1, p, button)
import Html.Events exposing (onClick)
import Html.Attributes as HA exposing (type_)
import Http
import List.Extra

--lange : Float
--lange = 
--    7

inDegree : List Float -> List Float
inDegree listvalue =
    List.map (\x -> (180 * (x - (Maybe.withDefault 0 (List.minimum listvalue)))/(((Maybe.withDefault 10000 (List.maximum listvalue))) - ((Maybe.withDefault 0 (List.minimum listvalue)))))) listvalue 

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
                    [ text "3. Iconplot"
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
                    {--, div []
                    [ Html.input [ type_ "range"
                            , HA.min "2"
                            , HA.max "15"
                            , HA.value <| String.fromFloat l.len
                            , Html.Events.onInput ChangeLen
                            ]
                            []
                            , text <| String.fromFloat l.len    
                    ] 
                    --} -- slider doesn't work well .-.
                    , p [] 
                    [
                        text "Please adjust the size of stick figure:"
                    ]
                    , p []
                            [ button [ onClick Increment ] [ text "+1" ]
                            , text <| " " ++ (String.fromFloat l.len) ++ " "
                            , button [ onClick Decrement ] [ text "-1" ]
                            ]
                    , stickfigureplot filteredTitles l.len
                    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Erfolg <| { data = Data.titleListe [fullText], len = 5 }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeDB para ->
            case para of
                All ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, len = m.len },holenVonCsv GotText 0)
                        _ ->
                            ( model, Cmd.none )
                   
                Movies ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, len = m.len },holenVonCsv GotText 1)
                        _ ->
                            ( model, Cmd.none )
                Series ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, len = m.len },holenVonCsv GotText 2)
                        _ ->
                            ( model, Cmd.none )
        {-- ChangeLen v ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, len = Maybe.withDefault 0 <| String.toFloat v }, Cmd.none)
                        _ ->
                            ( model, Cmd.none ) --}
        Increment ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, len = m.len + 1 }, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
        Decrement ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, len = m.len - 1 }, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                   
                

                

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
    , len : Float 
    }

type Msg
    = GotText (Result Http.Error String)
    | ChangeDB (Data.DB)
    --| ChangeLen (String)
    | Increment
    | Decrement

filterAndReduceTitles : List Title -> XyData
filterAndReduceTitles my_titles =
    XyData <| List.filterMap title2point my_titles -- xmts

pointLabel : String -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Point -- xmts
pointLabel title imdb_score imdb_votes runtime tmdb_popularity tmdb_score seasons release_year numberTags = -- xmts
    Point (title ++ " (" ++ String.fromFloat imdb_votes ++ ", " ++ String.fromFloat imdb_score
        ++ String.fromFloat runtime ++ ", " ++ String.fromFloat tmdb_popularity ++ ", " 
        ++ String.fromFloat tmdb_score ++ ", " ++ String.fromFloat seasons ++ ", "
        ++ String.fromFloat release_year ++ ", " ++ String.fromFloat numberTags ++ ")") 
        (imdb_votes) (imdb_score) (runtime) (tmdb_popularity) (tmdb_score) (seasons) (release_year) (numberTags) -- xmts

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
            |> andMap (Just title.release_year)
            |> andMap (Just title.numberTags)

stickfigureplot : XyData -> Float -> Svg msg
stickfigureplot model len =
 -- funktionen und parameter deklarieren
    let
        
        xValues : List Float
        xValues =
            List.map .b model.data --x

        yValues : List Float
        yValues =
            List.map .y model.data --y

        uValues : List Float
        uValues =
            List.map .z model.data --u

        vValues : List Float
        vValues =
            List.map .a model.data --v

        pValues : List Float
        pValues =
            List.map .d model.data --p

        qValues : List Float
        qValues =
            List.map .x model.data --q

        zValues : List Float
        zValues =
            List.map .e model.data --z

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        uDegree : List Float
        uDegree = 
            List.map (\x -> (270 - (x))) (inDegree uValues)

        vDegree : List Float
        vDegree = 
            List.map (\x -> (270 - (x))) (inDegree vValues)

        pDegree : List Float
        pDegree = 
            List.map (\x -> (270 - (x))) (inDegree pValues)

        qDegree : List Float
        qDegree = 
            List.map (\x -> (270 - (x))) (inDegree qValues)

        zDegree : List Float
        zDegree = 
            List.map (\x -> (270 - (x))) (inDegree zValues)

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) *8/9

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = (wideExtent xValues |> Tuple.second) - 1
            , y = (wideExtent yValues |> Tuple.second)
            }
   
    in
    -- output als svg => scatter plot
    
    svg [ viewBox 0 0 w h, TSA.width <| TST.Percent 100, TSA.height <| TST.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .line polyline { stroke: lightGrey; fill: rgba(255, 255, 255,0.3); ; stroke-width:1; }
            .line text { display: none; }
            .line:hover polyline { stroke: black; stroke-width:1.5; }
            .line:hover text { display: inline; font-size: small }
          """ ]
    -- plot x axis    
         , g[ transform [ Translate (60) (390)]]
            [
                xAxis xValues
                , text_
                [ x ( Scale.convert xScaleLocal (labelPositions.x))
                , y 35

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 15)

                --, fontWeight FontWeightBold
                ]
                [ text "TMDb Score" ] -- x
                ]
    -- plot y axis             
         ,g[transform [Translate(60) (60)]]
         [
             yAxis yValues
             , text_
                [ x -30
                , y -30

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 15)

                --, fontWeight FontWeightBold
                ]
                [ text "iMDB Score" ] -- y
             ]
    -- plot points and description     
         ,g [ transform [ Translate padding padding ] ]
            (List.map (stickfigure xScaleLocal yScaleLocal len) 
                uDegree 
                |> andMapl vDegree 
                |> andMapl pDegree 
                |> andMapl qDegree 
                |> andMapl zDegree 
                |> andMapl model.data
                
            )
            -- map data with the defined variables
        ]

andMapl : List a -> List (a -> b) -> List b
andMapl = List.map2 (|>)


stickfigure : ContinuousScale Float -> ContinuousScale Float -> Float -> Float -> Float -> Float -> Float -> Float -> Point -> Svg msg
stickfigure scaleX scaleY lange uDegree vDegree pDegree qDegree zDegree xyPoint  =
        g [ class [ "line"] ]
          [
            g  
                [ transform [ Translate (padding) padding ]
                ]
                [ text_ [ x  320, y -100, textAnchor AnchorMiddle ] [ Html.text xyPoint.pointName ]
                ]
            , g
                [   transform
                    [ Translate
                        (Scale.convert scaleX xyPoint.b) -- x
                        (Scale.convert scaleY xyPoint.y) -- y
                    ]
                ]
                
                [ polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree), -lange/2*sin(degrees uDegree)) ]
                        ]
                        []
                , polyline
                        [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( (-lange/2)*cos(degrees uDegree) + lange*cos(degrees vDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees vDegree) ) ]
                        ]
                        []

                , polyline
                        [ TSA.points [ ( (-lange/2)*cos(degrees uDegree), (-lange/2)*sin(degrees uDegree) ), ( -lange/2*cos(degrees uDegree) - lange*cos(degrees pDegree), -lange/2*sin(degrees uDegree) - lange*sin(degrees pDegree) ) ]
                        ]
                        []

                , polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) + lange*cos(degrees qDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees qDegree) ) ]
                        ]
                        []
                    
                , polyline
                        [ TSA.points [ ( lange/2*cos(degrees uDegree), lange/2*sin(degrees uDegree) ), ( lange/2*cos(degrees uDegree) - lange*cos(degrees zDegree), lange/2*sin(degrees uDegree) + lange*sin(degrees zDegree) ) ]
                        ]
                        []
                ]
          ]



 
-- shift the scale
 
xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)
--draw ticks under axis

yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )

type alias Point =
    { pointName : String, x : Float, y : Float, z : Float, a : Float, b : Float , c : Float , d : Float , e : Float } -- xmts

type alias XyData =
    {  data : List Point
    }