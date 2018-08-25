module Structure exposing (Dimension, LoadedDimension, Structure, dimensions, sortLinesByDistanceToPoint, structure)

import Array exposing (Array)
import LineSegment3d as LineSegment3d exposing (LineSegment3d)
import Point3d as Point3d exposing (Point3d)
import Vector3d as Vector3d exposing (Vector3d)


type alias Structure =
    { points : List Point
    , lines : List Line
    , value : Point3d
    , coordinates : List Line
    , focalPoint : Point3d
    }


type alias LoadedDimension =
    { value : Float
    , min : Float
    , max : Float
    , title : String
    , name : String
    }


type alias Dimension =
    { direction : Vector3d
    , distance : Float
    , value : Float
    , number : Int
    , name : String
    , title : String
    , min : Float
    , max : Float
    , step : Float
    , color : String
    }


type alias Point =
    { number : Int
    , color : String
    , point : Point3d
    }


type alias Line =
    { number : Int
    , color : String
    , line : LineSegment3d
    }


directions : Array Vector3d
directions =
    Array.fromList
        [ Vector3d.normalize (Vector3d.fromComponents ( -1, 0, 0 ))
        , Vector3d.normalize (Vector3d.fromComponents ( 0, 0, -1 ))
        , Vector3d.normalize (Vector3d.fromComponents ( 0, 1, 0 ))
        , Vector3d.normalize (Vector3d.fromComponents ( -1, 1, 1 ))
        , Vector3d.normalize (Vector3d.fromComponents ( 1, 1, 1 ))
        , Vector3d.normalize (Vector3d.fromComponents ( 1, -1, 1 ))
        , Vector3d.normalize (Vector3d.fromComponents ( 1, 1, -1 ))
        , Vector3d.normalize (Vector3d.fromComponents ( -1, -1, -1 ))

        -- TODO: better directions for 8+
        , Vector3d.normalize (Vector3d.fromComponents ( 0, 1, 1 ))
        , Vector3d.normalize (Vector3d.fromComponents ( 1, 0, 1 ))
        , Vector3d.normalize (Vector3d.fromComponents ( 1, 1, 0 ))
        , Vector3d.normalize (Vector3d.fromComponents ( 0, 2, 1 ))
        , Vector3d.normalize (Vector3d.fromComponents ( 2, 0, 1 ))
        , Vector3d.normalize (Vector3d.fromComponents ( 2, 1, 0 ))
        , Vector3d.normalize (Vector3d.fromComponents ( 0, 1, 2 ))
        ]


colors : Array String
colors =
    Array.fromList
        [ "#1a1919"
        , "#d62631"
        , "#00974c"
        , "#00a4d8"
        , "#dd5893"
        , "#f1b13b"
        , "#cccccc"
        , "#e4e4e3"

        -- TODO: better colors for 8+
        , "#2a2929"
        , "#e63641"
        , "#10a75c"
        , "#10b4e8"
        , "#ed68a3"
        , "#cccc00"
        , "#00e4e3"
        ]


dimensions : List LoadedDimension -> List Dimension
dimensions =
    List.indexedMap
        (\number { value, min, max, title, name } ->
            { value = value
            , min = min
            , max = max
            , title = title
            , name = name
            , number = number
            , direction = Array.get number directions |> Maybe.withDefault (Vector3d.fromComponents ( -1, -1, 1 ))
            , color = Array.get number colors |> Maybe.withDefault "transparent"
            , distance = toFloat (round (1.618 ^ toFloat number) * 10) / 10
            , step = 1
            }
        )


sortLinesByDistanceToPoint : Point3d -> List Line -> List Line
sortLinesByDistanceToPoint point =
    List.sortBy (.line >> LineSegment3d.midpoint >> Point3d.distanceFrom point >> (*) -1)


structure : List Dimension -> Int -> Structure
structure dimensions_ n =
    let
        activeDimensions =
            List.take n dimensions_

        newStructure =
            structureHelp activeDimensions initial

        focalPoint =
            List.foldl
                (\{ distance, direction, value, min, max } ->
                    Point3d.translateBy
                        (Vector3d.scaleBy (distance * 0.5) direction)
                )
                newStructure.value
                activeDimensions

        scale3d =
            newStructure.points
                |> List.foldl (\{ point } -> max (Point3d.distanceFrom focalPoint point)) 0
                |> (/) 0.45
                |> min 1

        currentValue =
            List.foldl
                (\{ distance, direction, value, min, max } ->
                    Point3d.translateBy
                        (Vector3d.scaleBy (distance * (value - min) / (max - min)) direction)
                )
                newStructure.value
                activeDimensions
    in
    { newStructure
        | focalPoint = focalPoint
        , value = Point3d.scaleAbout focalPoint scale3d currentValue
        , points = List.map (\point -> { point | point = Point3d.scaleAbout focalPoint scale3d point.point }) newStructure.points
        , lines = List.map (\line -> { line | line = LineSegment3d.scaleAbout focalPoint scale3d line.line }) newStructure.lines
        , coordinates =
            List.map
                (\{ number, color, distance, direction, value, min, max } ->
                    { line =
                        LineSegment3d.from
                            (Point3d.translateBy
                                (Vector3d.scaleBy (-distance * (value - min) / (max - min)) direction)
                                currentValue
                            )
                            currentValue
                            |> LineSegment3d.scaleAbout focalPoint scale3d
                    , number = number
                    , color = color
                    }
                )
                activeDimensions
    }


initial : Structure
initial =
    { points =
        [ { number = 0
          , point = Point3d.origin
          , color = Array.get 0 colors |> Maybe.withDefault "transparent"
          }
        ]
    , lines = []
    , coordinates = []
    , value = Point3d.origin
    , focalPoint = Point3d.origin
    }


structureHelp : List Dimension -> Structure -> Structure
structureHelp dimensions_ structure_ =
    case dimensions_ of
        [] ->
            structure_

        dimension :: restDimensions ->
            structureHelp restDimensions (addDimension dimension structure_)


addDimension : Dimension -> Structure -> Structure
addDimension { number, color, direction, distance } { value, coordinates, points, lines, focalPoint } =
    { points =
        List.concat
            [ List.map
                (\point ->
                    { number = number
                    , color = color
                    , point = Point3d.translateBy (Vector3d.scaleBy distance direction) point.point
                    }
                )
                points
            , points
            ]
    , lines =
        List.concat
            [ List.map
                (\point ->
                    { number = number
                    , color = color
                    , line = LineSegment3d.from point.point (Point3d.translateBy (Vector3d.scaleBy distance direction) point.point)
                    }
                )
                points
            , List.map
                (\line ->
                    { line | line = LineSegment3d.translateBy (Vector3d.scaleBy distance direction) line.line }
                )
                lines
            , lines
            ]
    , focalPoint = focalPoint
    , value = value
    , coordinates = coordinates
    }
