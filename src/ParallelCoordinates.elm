module ParallelCoordinates exposing (main)


import Browser
import Axis
import Html exposing (Html, a, li, ul)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (circle, g, rect, style, svg, text_, polygon, line)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox, fill, stroke, points, rotate, opacity)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y, strokeWidth, x1, y1, x2, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..), Paint(..), Opacity(..))
import Path
import Color exposing (Color)
import Shape
import Array exposing (Array)
import Html.Attributes exposing (style)
import Data exposing (DB(..), Title, defaultExtent, padding, h, w, tickCount, liste)
import Http
import List.Extra
import Html.Attributes exposing (style)
import Html exposing (p)
import Html exposing (h1)
import Color exposing (black, white)

type Model
  = Fehlschlag
  | Laden
  | Erfolg 
    { data : List Title
    , ersteFunktion : Title -> Float
    , zweiteFunktion : Title -> Float
    , dritteFunktion : Title -> Float
    , vierteFunktion : Title -> Float
    , ersterName : String
    , zweiterName : String
    , dritterName : String
    , vierterName : String
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

type alias MultiDimPunkt =
    { punktName : String, value : List Float }

type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPunkt)
    }



holenVonCsv : (Result Http.Error String -> Msg) -> Int -> Cmd Msg
holenVonCsv x db = 
    (List.Extra.getAt db Data.liste) |> Maybe.withDefault("titleslesslessdf.csv")|> String.words
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/do46/IR_Netflix_TVShows_Movies_VAD/main/Data/AufbereiteteDaten/" ++ datensatz
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch

einteilungAchseZahl : Int
einteilungAchseZahl =
    8

bgrc : List Color
bgrc = [ black , white ]


paralleleKoordinatenPlan : Float -> Float -> Int -> MultiDimData -> Svg msg
paralleleKoordinatenPlan w ar bgrC model =
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
            List.map (Axis.left [ Axis.tickCount einteilungAchseZahl ]) listeSkala

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
                .parallelerPunkt { stroke: black; fill:none; opacity: 0.5;stroke-width:0.5}
                .parallelerPunkt:hover {stroke: rgba(4, 244, 251, 1); stroke-width: 2.5}
                .parallelerPunkt text { display: none; }
                .parallelerPunkt:hover text { display: inline; stroke: black; stroke-width: 0.1; font-size: small; font-family: calibri}  
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
                    zeichnePunkt p name beschreibung =
                        let
                            linienWeg : Path.Path
                            linienWeg =
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
                        g [ if bgrC == 1 then class ["parallelerPunkt"] else class ["xray"]][
                            Path.element linienWeg
                            [
                            --if bgrC == 1 then class ["parallelerPunkt"] else class ["xray"] 
                             -- xmts
                            ]
                            , text_
                                [ x 300
                                , y -50
                                , TypedSvg.Attributes.textAnchor AnchorMiddle
                                , TypedSvg.Attributes.fill <| Paint <| black

                                
                                ]
                                [ TypedSvg.Core.text (name++ (String.concat<|(List.map2(\a b-> ", " ++b++ ": "++ (String.fromFloat a))p beschreibung)))]
                                
                        ]
                        
                in
                model.data
                    |> List.map
                        (\dataset ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (\a -> zeichnePunkt a.value a.punktName model.dimDescription) dataset)
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
            Html.text "Ich konnte Ihre Title nicht öffnen."

        Laden ->
            Html.text "Title werden geöffnet..."

        Erfolg l ->
                    let
                        multiDimDaten : List Title -> (Title -> Float) -> (Title -> Float) -> (Title -> Float) -> (Title -> Float) -> (Title -> String) -> String -> String -> String -> String-> MultiDimData
                        multiDimDaten listeTitle a b c d e f g h i=
                         MultiDimData [f, g, h, i]
                            [ List.map
                                (\x ->
                                    [(a x), (b x), (c x), (d x)]
                                        |> MultiDimPunkt (e x)
                                )
                                listeTitle
                            ]

                        numberTitles =
                            List.length l.data

                        plotDaten = 
                            multiDimDaten l.data l.ersteFunktion l.zweiteFunktion l.dritteFunktion l.vierteFunktion .title l.ersterName l.zweiterName l.dritterName l.vierterName       
                    in
                    div []
                        [   h1 []
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
                                
                                    Html.text <| "Please choose one of the following data type for axis 1 "
                                    , Html.button [onClick (ChangeAtt1 (.runtime, "Runtime"))][Html.text "Runtime"]
                                    , Html.button [onClick (ChangeAtt1 (.imdb_score, "IMDb score"))][Html.text "IMDb score"]
                                    , Html.button [onClick (ChangeAtt1 (.imdb_votes, "IMDb votes"))][Html.text "IMDb votes"]
                                    , Html.button [onClick (ChangeAtt1 (.tmdb_popularity, "TMDb popularity"))][Html.text "TMDb popularity"]
                                    , Html.button [onClick (ChangeAtt1 (.tmdb_score, "TMDb score"))][Html.text "TMDb score"]
                                    , Html.button [onClick (ChangeAtt1 (.seasons, "Seasons"))][Html.text "Seasons"]
                                
                            ]
                            , p[][
                                
                                    Html.text <| "Please choose one of the following data type for axis 2 "
                                    , Html.button [onClick (ChangeAtt2 (.runtime, "Runtime"))][Html.text "Runtime"]
                                    , Html.button [onClick (ChangeAtt2 (.imdb_score, "IMDb score"))][Html.text "IMDb score"]
                                    , Html.button [onClick (ChangeAtt2 (.imdb_votes, "IMDb votes"))][Html.text "IMDb votes"]
                                    , Html.button [onClick (ChangeAtt2 (.tmdb_popularity, "TMDb popularity"))][Html.text "TMDb popularity"]
                                    , Html.button [onClick (ChangeAtt2 (.tmdb_score, "TMDb score"))][Html.text "TMDb score"]
                                    , Html.button [onClick (ChangeAtt2 (.seasons, "Seasons"))][Html.text "Seasons"]
                                
                            ]
                            , p[][
                                
                                    Html.text <| "Please choose one of the following data type for axis 3 "
                                    , Html.button [onClick (ChangeAtt3 (.runtime, "Runtime"))][Html.text "Runtime"]
                                    , Html.button [onClick (ChangeAtt3 (.imdb_score, "IMDb score"))][Html.text "IMDb score"]
                                    , Html.button [onClick (ChangeAtt3 (.imdb_votes, "IMDb votes"))][Html.text "IMDb votes"]
                                    , Html.button [onClick (ChangeAtt3 (.tmdb_popularity, "TMDb popularity"))][Html.text "TMDb popularity"]
                                    , Html.button [onClick (ChangeAtt3 (.tmdb_score, "TMDb score"))][Html.text "TMDb score"]
                                    , Html.button [onClick (ChangeAtt3 (.seasons, "Seasons"))][Html.text "Seasons"]
                                
                            ]
                            , p[][
                                
                                    Html.text <| "Please choose one of the following data type for axis 4 "
                                    , Html.button [onClick (ChangeAtt4 (.runtime, "Runtime"))][Html.text "Runtime"]
                                    , Html.button [onClick (ChangeAtt4 (.imdb_score, "IMDb score"))][Html.text "IMDb score"]
                                    , Html.button [onClick (ChangeAtt4 (.imdb_votes, "IMDb votes"))][Html.text "IMDb votes"]
                                    , Html.button [onClick (ChangeAtt4 (.tmdb_popularity, "TMDb popularity"))][Html.text "TMDb popularity"]
                                    , Html.button [onClick (ChangeAtt4 (.tmdb_score, "TMDb score"))][Html.text "TMDb score"]
                                    , Html.button [onClick (ChangeAtt4 (.seasons, "Seasons"))][Html.text "Seasons"]
                                
                            ]
                            , p[][
                                
                                    Html.text <| "Please choose the mode "
                                    , Html.button [onClick (ChangeMode(B))][Html.text "X-Ray"]
                                    , Html.button [onClick (ChangeMode(W))][Html.text "Normal"]
                                
                            ]
                                
                            , div []
                                    [ paralleleKoordinatenPlan 600 2 l.cl plotDaten 
                                    ] 
                        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText ergebnis ->
            case ergebnis of
                Ok fullText ->
                    ( Erfolg <| { data = Data.titleListe [ fullText ], ersteFunktion = .runtime, zweiteFunktion = .imdb_score, dritteFunktion = .imdb_votes, vierteFunktion = .tmdb_popularity , ersterName = "Runtime", zweiterName = "IMDb score", dritterName = "IMDb votes", vierterName = "TMDb popularity", cl = 1}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeAtt1 (x, a) ->
            case model of
                Erfolg m ->
                    ( Erfolg <| { data = m.data, ersteFunktion = x, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = a, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = m.vierterName, cl= m.cl}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeAtt2 (y, a) ->
            case model of
                Erfolg m ->
                    ( Erfolg <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = y, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = a, dritterName = m.dritterName, vierterName = m.vierterName, cl= m.cl}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeAtt3 (z, a) ->
            case model of
                Erfolg m ->
                    ( Erfolg <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = z, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = a, vierterName = m.vierterName, cl= m.cl}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeAtt4 (c, a) ->
            case model of
                Erfolg m ->
                    ( Erfolg <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = c , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = a, cl= m.cl}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeDB para ->
            case para of
                All ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = m.vierterName, cl= m.cl},holenVonCsv GotText 0)
                        _ ->
                            ( model, Cmd.none )
                   
                Movies ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = m.vierterName, cl= m.cl},holenVonCsv GotText 1)
                        _ ->
                            ( model, Cmd.none )
                Series ->
                    case model of
                        Erfolg m ->
                            (Erfolg <| {data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = m.vierterName, cl= m.cl},holenVonCsv GotText 2)
                        _ ->
                            ( model, Cmd.none )
        ChangeMode color ->
            case color of
                B -> 
                    case model of 
                        Erfolg m ->
                            (Erfolg <| {data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = m.vierterName, cl= 0}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )
                W -> 
                    case model of 
                        Erfolg m ->
                            (Erfolg <| {data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = m.vierterName, cl= 1}, Cmd.none)
                        _ ->
                            ( model, Cmd.none )


