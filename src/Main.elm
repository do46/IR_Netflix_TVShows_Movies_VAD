module Main exposing (main)

import Html exposing (..)
import Html.Attributes as HA exposing (href)
import Css exposing (..)

main : Html msg
main =
    Html.div[HA.style "padding" "20px"]
    [Html.h2[HA.style "fontSize" "30px"] 
        [ Html.text "Modul Information Retrieval und Visualisierung SS 2022"
        ]
    , Html.h3[HA.style "fontSize" "20px"] 
        [ Html.text "Projekt: Netflix TV Shows and Movies"
        ]
    , Html.h4[HA.style "fontSize" "20px"] 
        [ Html.text "von: Viet-Anh Do"
        ]
    , Html.p [HA.style "fontSize" "18px"]
        [ Html.text "Im Rahmen dieses Projektes wird eine Filmdatenbank analysiert und anschließend visualisiert anhands drei unterschiedlichen Visualisierungstechniken."
        ]
    , Html.p [HA.style "fontSize" "18px"]
        [ Html.text "Die Quelldaten sind auf der Webseite Kaggle.com zu finden:"
        ]
    , Html.p [HA.style "fontSize" "18px"]
        [ Html.a[ href "https://www.kaggle.com/datasets/victorsoeiro/netflix-tv-shows-and-movies?select=titles.csv" ] [ Html.text "Kaggle.com"]
        ]
    
    , Html.p [HA.style "fontSize" "18px"]
        [ Html.text "Klicken die unten stehenden URLs an, um zwischen den drei Visualisierungen zu navigieren."
        ]
    , Html.p [HA.style "fontSize" "16px"]
        [ Html.a[ href "Scatterplot.elm" ] [ Html.text "Scatterplot" ]
        , Html.br [][]
        , Html.a [ href "ParallelCoordinates.elm" ] [ Html.text "Parallel Coordinates" ]
        , Html.br [][]
        , Html.a [ href "IconPlot.elm" ] [ Html.text "Iconplot" ]
        ]
    , Html.p [HA.style "fontSize" "18px"]
        [ Html.text "Das Projekt befindet sich auch auf:"
        ]
    , Html.p [HA.style "fontSize" "18px"]
        [ Html.a[ href "https://github.com/do46/IR_Netflix_TVShows_Movies_VAD" ] [ Html.text "https://github.com/do46/IR_Netflix_TVShows_Movies_VAD"]
        ]
    ]

