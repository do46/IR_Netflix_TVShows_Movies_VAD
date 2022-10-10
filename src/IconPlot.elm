module IconPlot exposing (..)

import Axis
import Html exposing (Html)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (g, polyline, style, svg, text_)
import TypedSvg.Attributes as TSA exposing (class, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types as TST exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px) 
import Data exposing (DB(..), Title, padding, h, w, tickCount, titleListe, wideExtent)
import Browser
import Html exposing (div, h1, p, button, ul, a)
import Html.Events exposing (onClick)
import Http
import List.Extra
import Html.Attributes exposing (id, value, href)

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
    ( Loading
    , getData GotText 0
    )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model =
    case model of
        Error ->
            text "I cannot open the database"

        Loading ->
            text "Loading ..."

        Success ml ->
            let
                filteredTitles =
                    filterAndReduceTitles ml.data 

                numberTitles =
                    List.length ml.data

            in
                div []
                    [ Html.a [ href "Main.elm" ] [ Html.text "Back to homepage" ] 
                    , h1 []
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
                            , HA.value <| String.fromFloat ml.len
                            , Html.Events.onInput ChangeLen
                            ]
                            []
                            , text <| String.fromFloat ml.len    
                    ] 
                    --} -- slider doesn't work well .-.
                    , p [] 
                    [
                        text "Please adjust the size of stick figure:"
                    ]
                    , p []
                            [ button [ onClick Decrement ] [ text "-" ]
                            , text <| " " ++ (String.fromFloat ml.len) ++ " "
                            , button [ onClick Increment ] [ text "+" ]
                            ]
                    , stickfigureplot filteredTitles ml.len
                    , div[][stickfiguretest]
                    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = titleListe [fullText], len = 5 }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeDB para ->
            case para of
                All ->
                    case model of
                        Success m ->
                            (Success <| {data = m.data, len = m.len },getData GotText 0)
                        _ ->
                            ( model, Cmd.none )
                   
                Movies ->
                    case model of
                        Success m ->
                            (Success <| {data = m.data, len = m.len },getData GotText 1)
                        _ ->
                            ( model, Cmd.none )
                Series ->
                    case model of
                        Success m ->
                            (Success <| {data = m.data, len = m.len },getData GotText 2)
                        _ ->
                            ( model, Cmd.none )
        {-- ChangeLen v ->
                    case model of
                        Success m ->
                            (Success <| {data = m.data, len = Maybe.withDefault 0 <| String.toFloat v }, Cmd.none)
                        _ ->
                            ( model, Cmd.none ) --}
        Increment ->
                    case model of
                        Success m ->
                            (Success <| {data = m.data, len = m.len + 1 }, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
        Decrement ->
                    case model of
                        Success m ->
                            (Success <| {data = m.data, len = m.len - 1 }, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                   
                

                

getData : (Result Http.Error String -> Msg) -> Int -> Cmd Msg
getData x db = 
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
  = Error
  | Loading
  | Success 
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
            { x = (wideExtent xValues |> half) 
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
            .text text {vertical-align: middle; display: inline-block; font-size: large; stroke: black}
            .line1 polyline { stroke: black; fill: rgba(255, 255, 255,0.3); ; stroke-width:2; }
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

-- Test --
a_ = 0
a = 90-a_
b_ = 90
b = 360 - 90 - b_
c_= 135
c = 360 - 90 - c_
d_ = 135
d = 360 - 90 - d_
e_ = 90

e = 360 - 90 - e_

l : Float
l = 30

stickfiguretest : Svg msg
stickfiguretest =
    svg [ viewBox 0 0 w h, TSA.width <| TST.Percent 100, TSA.height <| TST.Percent 100 ]
        [ g
            [ transform [ Translate (padding - 1) (padding - 1) ]
            , class ["line1"]
            ]
            [ polyline
                [ TSA.points [ ( l/2*cos(degrees a), l/2*sin(degrees a) ), ( -l/2*cos(degrees a), -l/2*sin(degrees a) ) ]
                ]
                []
            , polyline
                [ TSA.points [ ( -l/2*cos(degrees a), -l/2*sin(degrees a) ), ( -l/2*cos(degrees a)+l*cos(degrees b), -l/2*sin(degrees a) - l * sin(degrees b) ) ]
                ]
                []

            , polyline
                [ TSA.points [ ( -l/2*cos(degrees a), -l/2*sin(degrees a) ), ( (-l/2)*cos(degrees a) - l*cos(degrees c), (-l/2)*sin(degrees a) - l*sin(degrees c) ) ]
                ]
                []

            , polyline
                [ TSA.points [ ( l/2*cos(degrees a), l/2*sin(degrees a) ), ( (l/2)*cos(degrees a) + l*cos(degrees d), l/2*sin(degrees a)+l*sin(degrees d) ) ]
                ]
                []
            
            , polyline
                [ TSA.points [ ( l/2*cos(degrees a), l/2*sin(degrees a) ), ( (l/2)*cos(degrees a) - l*cos(degrees e), l/2*sin(degrees a)+l*sin(degrees e) ) ]
                ]
                []
            
            ]
            , text_ [ x 100 , y 20 ,fontSize (px 10) , class["text"]]
            [text "- body: runtime"]
            , text_ [ x 100 , y 40 ,fontSize (px 10) , class["text"]]
            [text "- left hand: TMDb popularity"]
            , text_ [ x 100 , y 60 ,fontSize (px 10) , class["text"]]
            [text "- right hand: release year"]
            , text_ [ x 100 , y 80 ,fontSize (px 10) , class["text"]]
            [text "- left foot: IMDb votes"]
            , text_ [ x 100 , y 100 ,fontSize (px 10) , class["text"]]
            [text "- right foot: number of tags"]

        ]