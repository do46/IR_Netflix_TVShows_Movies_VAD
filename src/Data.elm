module Data exposing (Title, DB(..), w, h, padding, tickCount, defaultExtent, titleListe, wideExtent, liste)

import Csv
import Csv.Decode
import Statistics

type DB
    = All
    | Movies
    | Series

type alias Title =
    { id : String
    , title : String
    , typ : String
    , release_year : Float
    , runtime : Float
    , imdb_score : Float
    , imdb_votes : Float
    , tmdb_popularity : Float
    , tmdb_score : Float
    , seasons : Float
    }

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
            |> Csv.Decode.andMap (Csv.Decode.field "release_year" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "runtime"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "imdb_score"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "imdb_votes"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "tmdb_popularity"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "tmdb_score"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "seasons"(String.toFloat >> Result.fromMaybe "error parsing string"))
        ) -- xmts

titleListe :List String -> List Title
titleListe liste1 =
    List.map(\t -> csvStringZuDaten t) liste1
        |> List.concat

wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        scale: (Float, Float)
        scale = Maybe.withDefault defaultExtent (Statistics.extent values)

        range: Float
        range = -(Tuple.first scale) + (Tuple.second scale)

        down: Float
        down = (Tuple.first scale) - range/(toFloat (2*tickCount))

        up: Float
        up = (Tuple.second scale) + range/(toFloat (2*tickCount))
    in         
        if (down < 0) then 
            (0, up)
        else
            (down, up)


liste : List String
liste =
    [ 
    "titleslesslessdf.csv",
    "moviedf.csv",
    "showdf.csv"
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
