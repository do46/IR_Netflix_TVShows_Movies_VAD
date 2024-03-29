module ParallelCoordinates exposing (..)


import Browser
import Axis
import Html exposing (Html, a)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import TypedSvg exposing ( g, rect, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..), Paint(..), Opacity(..))
import Path
import Color exposing (Color)
import Shape
import Data exposing (DB(..), Title, padding, h, w)
import Http
import List.Extra
import Html exposing (p)
import Html exposing (h1)
import Color exposing (black, white)
import Scale
import Html.Attributes exposing (href)

type Model
  = Error
  | Loading
  | Success 
    { data : List Title
    , firstFunction : Title -> Float
    , secondFunction : Title -> Float
    , thirdFunction : Title -> Float
    , fourthFunction : Title -> Float
    , firstName : String
    , secondName : String
    , thirdName : String
    , fourthName : String
    , cl : Int
    }


type Msg
    = GotText (Result Http.Error String)
    | ChangeAtt1 (Title -> Float, String)
    | ChangeAtt2 (Title -> Float, String)
    | ChangeAtt3 (Title -> Float, String)
    | ChangeAtt4 (Title -> Float, String)
    | ChangeDB (Data.DB)
    | ChangeMode (Mode)

type Mode
    = B
    | W

type alias MultiDimPoint =
    { pointName : String, value : List Float }

type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }



getData : (Result Http.Error String -> Msg) -> Int -> Cmd Msg
getData x db = 
    (List.Extra.getAt db Data.liste) |> Maybe.withDefault("titleslesslessdf.csv")|> String.words
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/do46/IR_Netflix_TVShows_Movies_VAD/main/Data/AufbereiteteDaten/" ++ datensatz
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch

noAxis : Int
noAxis =
    8

bgrc : List Color
bgrc = [ black , white ]


parCoord : Float -> Float -> Int -> MultiDimData -> Svg msg
parCoord w ar bgrC model =
    let
        h : Float
        h =
            w / ar

        listeTransformieren : List (List Float)
        listeTransformieren =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose

        listeWeiteErweiterung : List ( Float, Float )
        listeWeiteErweiterung =
            listeTransformieren |> List.map Data.wideExtent

        listeSkala =
            List.map (Scale.linear ( h, 0 )) listeWeiteErweiterung

        listeAchse =
            List.map (Axis.left [ Axis.tickCount noAxis ]) listeSkala

        xSkala =
            Scale.linear ( 0, w ) ( 1, List.length model.dimDescription |> toFloat )
    in
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.stroke <| PaintNone -- xmts
        , TypedSvg.Attributes.fill <| Paint <| Maybe.withDefault white <|List.Extra.getAt bgrC bgrc
        ]
    <|
        [ TypedSvg.style []
            [
                TypedSvg.Core.text """
                .xray {stroke: white; stroke-width:1; opacity:0.1; fill: none}
                .xray text {display: none;}
                .normal { stroke: black; fill:none; opacity: 0.5;stroke-width:0.5}
                .normal:hover {stroke: rgba(4, 244, 251, 1); stroke-width: 2.5}
                .normal text { display: none; }
                .normal:hover text { display: inline; stroke: black; stroke-width: 0.1; font-size: small; font-family: calibri}  
                """
            ]
        , rect [x  0 , y 0, width (w + 2 * padding), height (h + 2 * padding)][]
        , g [TypedSvg.Attributes.stroke <| Paint <| black ]
            [ g [ transform [ Translate (padding - 1) padding ] ] <|
                List.indexedMap
                    (\i axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert xSkala (toFloat i + 1)) 0
                                ] 
                                
                            ]
                            [ axis ]
                    )
                    listeAchse
            , g [ transform [ Translate (padding - 1) 0 ] ] <|
                List.indexedMap
                    (\i desc ->
                        text_
                            [ fontFamily [ "calibri" ]
                            , fontSize (Px 12)
                            , x <| Scale.convert xSkala (toFloat i + 1)
                            , y <| padding * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text desc ]
                    )
                    model.dimDescription
            ]
        ]
            ++ (let
                    drawPoint p name description =
                        let
                            linePath : Path.Path
                            linePath =
                                List.map3
                                    (\desc s px ->
                                        Just
                                            ( Scale.convert xSkala <| toFloat desc
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length model.dimDescription))
                                    listeSkala
                                    p
                                    |> Shape.line Shape.linearCurve
                        in
                        g [ if bgrC == 1 then class ["normal"] else class ["xray"]][
                            Path.element linePath
                            []
                            , text_
                                [ x 300
                                , y -50
                                , TypedSvg.Attributes.textAnchor AnchorMiddle
                                , TypedSvg.Attributes.fill <| Paint <| black

                                
                                ]
                                [ TypedSvg.Core.text (name++ (String.concat<|(List.map2(\a b-> ", " ++b++ ": "++ (String.fromFloat a))p description)))]
                                
                        ]
                        
                in
                model.data
                    |> List.map
                        (\dataset ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (\a -> drawPoint a.value a.pointName model.dimDescription) dataset)
                        )
               )



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
            Html.text "I cannot open your data."

        Loading ->
            Html.text "Loading..."

        Success l ->
                    let
                        multiDimDaten : List Title -> (Title -> Float) -> (Title -> Float) -> (Title -> Float) -> (Title -> Float) -> (Title -> String) -> String -> String -> String -> String-> MultiDimData
                        multiDimDaten listeTitle a b c d e f g h i=
                         MultiDimData [f, g, h, i]
                            [ List.map
                                (\x ->
                                    [(a x), (b x), (c x), (d x)]
                                        |> MultiDimPoint (e x)
                                )
                                listeTitle
                            ]

                        numberTitles =
                            List.length l.data

                        plotDaten = 
                            multiDimDaten l.data l.firstFunction l.secondFunction l.thirdFunction l.fourthFunction .title l.firstName l.secondName l.thirdName l.fourthName       
                    in
                    div []
                        [   a [ href "Main.elm" ] [ Html.text "Back to homepage" ] 
                            , Html.br [][]
                            , Html.a [ href "Scatterplot.elm" ] [ Html.text "Scatterplot" ]
                            , Html.br [][]
                            , Html.a [ href "IconPlot.elm" ] [ Html.text "Iconplot" ]
                            , Html.br [][]
                            , h1 []
                            [ text "2. Parallel Coordinates" ]
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
                            , p[][
                                
                                    Html.text <| "Please choose one of the following attributes for axis 1: "
                                    , Html.button [onClick (ChangeAtt1 (.runtime, "Runtime"))][Html.text "Runtime"]
                                    , Html.button [onClick (ChangeAtt1 (.imdb_score, "IMDb score"))][Html.text "IMDb score"]
                                    , Html.button [onClick (ChangeAtt1 (.imdb_votes, "IMDb votes"))][Html.text "IMDb votes"]
                                    , Html.button [onClick (ChangeAtt1 (.tmdb_popularity, "TMDb popularity"))][Html.text "TMDb popularity"]
                                    , Html.button [onClick (ChangeAtt1 (.tmdb_score, "TMDb score"))][Html.text "TMDb score"]
                                    , Html.button [onClick (ChangeAtt1 (.seasons, "Seasons"))][Html.text "Seasons"]
                                    , Html.button [onClick (ChangeAtt1 (.release_year, "Release year"))][Html.text "Release year"]
                                    , Html.button [onClick (ChangeAtt1 (.numberTags, "Number of tags"))][Html.text "Number of tags"]
                            ]
                            , p[][
                                
                                    Html.text <| "Please choose one of the following attributes for axis 2: "
                                    , Html.button [onClick (ChangeAtt2 (.runtime, "Runtime"))][Html.text "Runtime"]
                                    , Html.button [onClick (ChangeAtt2 (.imdb_score, "IMDb score"))][Html.text "IMDb score"]
                                    , Html.button [onClick (ChangeAtt2 (.imdb_votes, "IMDb votes"))][Html.text "IMDb votes"]
                                    , Html.button [onClick (ChangeAtt2 (.tmdb_popularity, "TMDb popularity"))][Html.text "TMDb popularity"]
                                    , Html.button [onClick (ChangeAtt2 (.tmdb_score, "TMDb score"))][Html.text "TMDb score"]
                                    , Html.button [onClick (ChangeAtt2 (.seasons, "Seasons"))][Html.text "Seasons"]
                                    , Html.button [onClick (ChangeAtt2 (.release_year, "Release year"))][Html.text "Release year"]
                                    , Html.button [onClick (ChangeAtt2 (.numberTags, "Number of tags"))][Html.text "Number of tags"]
                            ]
                            , p[][
                                
                                    Html.text <| "Please choose one of the following attributes for axis 3: "
                                    , Html.button [onClick (ChangeAtt3 (.runtime, "Runtime"))][Html.text "Runtime"]
                                    , Html.button [onClick (ChangeAtt3 (.imdb_score, "IMDb score"))][Html.text "IMDb score"]
                                    , Html.button [onClick (ChangeAtt3 (.imdb_votes, "IMDb votes"))][Html.text "IMDb votes"]
                                    , Html.button [onClick (ChangeAtt3 (.tmdb_popularity, "TMDb popularity"))][Html.text "TMDb popularity"]
                                    , Html.button [onClick (ChangeAtt3 (.tmdb_score, "TMDb score"))][Html.text "TMDb score"]
                                    , Html.button [onClick (ChangeAtt3 (.seasons, "Seasons"))][Html.text "Seasons"]
                                    , Html.button [onClick (ChangeAtt3 (.release_year, "Release year"))][Html.text "Release year"]
                                    , Html.button [onClick (ChangeAtt3 (.numberTags, "Number of tags"))][Html.text "Number of tags"]
                            ]
                            , p[][
                                
                                    Html.text <| "Please choose one of the following attributes for axis 4: "
                                    , Html.button [onClick (ChangeAtt4 (.runtime, "Runtime"))][Html.text "Runtime"]
                                    , Html.button [onClick (ChangeAtt4 (.imdb_score, "IMDb score"))][Html.text "IMDb score"]
                                    , Html.button [onClick (ChangeAtt4 (.imdb_votes, "IMDb votes"))][Html.text "IMDb votes"]
                                    , Html.button [onClick (ChangeAtt4 (.tmdb_popularity, "TMDb popularity"))][Html.text "TMDb popularity"]
                                    , Html.button [onClick (ChangeAtt4 (.tmdb_score, "TMDb score"))][Html.text "TMDb score"]
                                    , Html.button [onClick (ChangeAtt4 (.seasons, "Seasons"))][Html.text "Seasons"]
                                    , Html.button [onClick (ChangeAtt4 (.release_year, "Release year"))][Html.text "Release year"]
                                    , Html.button [onClick (ChangeAtt4 (.numberTags, "Number of tags"))][Html.text "Number of tags"]
                                
                            ]
                            , p[][
                                
                                    Html.text <| "Please choose the mode: "
                                    , Html.button [onClick (ChangeMode(B))][Html.text "X-Ray"]
                                    , Html.button [onClick (ChangeMode(W))][Html.text "Normal"]
                                
                            ]
                                
                            , div []
                                    [ parCoord 600 2 l.cl plotDaten 
                                    ] 
                        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText output ->
            case output of
                Ok fullText ->
                    ( Success <| { data = Data.titleListe [ fullText ], firstFunction = .runtime, secondFunction = .imdb_score, thirdFunction = .imdb_votes, fourthFunction = .tmdb_popularity , firstName = "Runtime", secondName = "IMDb score", thirdName = "IMDb votes", fourthName = "TMDb popularity", cl = 1}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeAtt1 (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFunction = x, secondFunction = m.secondFunction, thirdFunction = m.thirdFunction, fourthFunction = m.fourthFunction , firstName = a, secondName = m.secondName, thirdName = m.thirdName, fourthName = m.fourthName, cl= m.cl}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeAtt2 (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFunction = m.firstFunction, secondFunction = y, thirdFunction = m.thirdFunction, fourthFunction = m.fourthFunction , firstName = m.firstName, secondName = a, thirdName = m.thirdName, fourthName = m.fourthName, cl= m.cl}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeAtt3 (z, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFunction = m.firstFunction, secondFunction = m.secondFunction, thirdFunction = z, fourthFunction = m.fourthFunction , firstName = m.firstName, secondName = m.secondName, thirdName = a, fourthName = m.fourthName, cl= m.cl}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeAtt4 (c, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, firstFunction = m.firstFunction, secondFunction = m.secondFunction, thirdFunction = m.thirdFunction, fourthFunction = c , firstName = m.firstName, secondName = m.secondName, thirdName = m.thirdName, fourthName = a, cl= m.cl}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeDB para ->
            case para of
                All ->
                    case model of
                        Success m ->
                            (Success <| {data = m.data, firstFunction = m.firstFunction, secondFunction = m.secondFunction, thirdFunction = m.thirdFunction, fourthFunction = m.fourthFunction , firstName = m.firstName, secondName = m.secondName, thirdName = m.thirdName, fourthName = m.fourthName, cl= m.cl},getData GotText 0)
                        _ ->
                            ( model, Cmd.none )
                   
                Movies ->
                    case model of
                        Success m ->
                            (Success <| {data = m.data, firstFunction = m.firstFunction, secondFunction = m.secondFunction, thirdFunction = m.thirdFunction, fourthFunction = m.fourthFunction , firstName = m.firstName, secondName = m.secondName, thirdName = m.thirdName, fourthName = m.fourthName, cl= m.cl},getData GotText 1)
                        _ ->
                            ( model, Cmd.none )
                Series ->
                    case model of
                        Success m ->
                            (Success <| {data = m.data, firstFunction = m.firstFunction, secondFunction = m.secondFunction, thirdFunction = m.thirdFunction, fourthFunction = m.fourthFunction , firstName = m.firstName, secondName = m.secondName, thirdName = m.thirdName, fourthName = m.fourthName, cl= m.cl},getData GotText 2)
                        _ ->
                            ( model, Cmd.none )
        ChangeMode color ->
            case color of
                B -> 
                    case model of 
                        Success m ->
                            (Success <| {data = m.data, firstFunction = m.firstFunction, secondFunction = m.secondFunction, thirdFunction = m.thirdFunction, fourthFunction = m.fourthFunction , firstName = m.firstName, secondName = m.secondName, thirdName = m.thirdName, fourthName = m.fourthName, cl= 0}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                W -> 
                    case model of 
                        Success m ->
                            (Success <| {data = m.data, firstFunction = m.firstFunction, secondFunction = m.secondFunction, thirdFunction = m.thirdFunction, fourthFunction = m.fourthFunction , firstName = m.firstName, secondName = m.secondName, thirdName = m.thirdName, fourthName = m.fourthName, cl= 1}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )


