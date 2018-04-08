module Structure exposing (Structure, Dimension, structure, dimensions, sortLinesByDistanceToPoint)

import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.LineSegment3d as LineSegment3d exposing (LineSegment3d)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


type alias Structure =
    { points : List Point
    , lines : List Line
    , value : Point3d
    , coordinates : List Line
    , focalPoint : Point3d
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
    }


type alias Point =
    { number : Int
    , point : Point3d
    }


type alias Line =
    { number : Int
    , line : LineSegment3d
    }


dimensions : List Dimension
dimensions =
    [ { direction = Vector3d.fromComponents ( 1, 0, 0 )
      , distance = 1
      , name = "wght"
      , title = "weight"
      , min = 28
      , max = 194
      }
    , { direction = Vector3d.fromComponents ( 0, 0, -1 )
      , distance = 1.618
      , name = "wdth"
      , title = "width"
      , min = 50
      , max = 130
      }
    , { direction = Vector3d.fromComponents ( 0, 1, 0 )
      , distance = 1.618 ^ 2
      , name = "opsz"
      , title = "optical size"
      , min = 12
      , max = 72
      }
    , { direction = Vector3d.fromComponents ( -1, 1, 1 )
      , distance = 1.618 ^ 3
      , name = "x-height"
      , title = "x-height"
      , min = 450
      , max = 550
      }
    , { direction = Vector3d.fromComponents ( 1, 1, 1 )
      , distance = 1.618 ^ 4
      , name = "ascender"
      , title = "ascender"
      , min = 650
      , max = 750
      }
    , { direction = Vector3d.fromComponents ( 1, -1, 1 )
      , distance = 1.618 ^ 5
      , name = "descender"
      , title = "descender"
      , min = -200
      , max = -300
      }
    , { direction = Vector3d.fromComponents ( 1, 1, -1 )
      , distance = 1.618 ^ 7
      , name = "contrast"
      , title = "contrast"
      , min = 20
      , max = 90
      }
    , { direction = Vector3d.fromComponents ( -1, -1, -1 )
      , distance = 1.618 ^ 8
      , name = "serifcurve"
      , title = "serif curve"
      , min = 0
      , max = 10
      }
    ]
        |> List.indexedMap
            (\n { direction, distance, name, title, min, max } ->
                { direction = Vector3d.normalize direction
                , distance = distance
                , number = n
                , value = 0
                , step = 1
                , name = name
                , title = title
                , min = min
                , max = max
                }
            )


sortLinesByDistanceToPoint : Point3d -> List Line -> List Line
sortLinesByDistanceToPoint point =
    List.sortBy (.line >> LineSegment3d.midpoint >> Point3d.distanceFrom point >> (*) -1)


structure : List Dimension -> Int -> Structure
structure dimensions n =
    let
        activeDimensions =
            List.take n dimensions

        newStructure =
            structureHelp activeDimensions initial

        hull =
            Point3d.hullOf (List.map .point newStructure.points)
                |> Maybe.withDefault (BoundingBox3d.singleton Point3d.origin)

        ( x, y, z ) =
            BoundingBox3d.dimensions hull

        scale3d =
            1 / (List.maximum [ x, y, z, 1 ] |> Maybe.withDefault 1) / 1.4

        focalPoint =
            BoundingBox3d.centroid hull

        currentValue =
            List.foldl
                (\{ number, distance, direction, value } ->
                    Point3d.translateBy
                        (Vector3d.scaleBy (distance * value) direction)
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
                    (\{ number, distance, direction, value } ->
                        { line =
                            LineSegment3d.from
                                (Point3d.translateBy
                                    (Vector3d.scaleBy (-distance * value) direction)
                                    currentValue
                                )
                                currentValue
                                |> LineSegment3d.scaleAbout focalPoint scale3d
                        , number = number
                        }
                    )
                    activeDimensions
        }


initial : Structure
initial =
    { points =
        [ { number = 0
          , point = Point3d.origin
          }
        ]
    , lines = []
    , coordinates = []
    , value = Point3d.origin
    , focalPoint = Point3d.origin
    }


structureHelp : List Dimension -> Structure -> Structure
structureHelp dimensions structure =
    case dimensions of
        [] ->
            structure

        dimension :: restDimensions ->
            structureHelp restDimensions (addDimension dimension structure)


addDimension : Dimension -> Structure -> Structure
addDimension { number, direction, distance } { value, coordinates, points, lines, focalPoint } =
    { points =
        List.concat
            [ List.map
                (\point ->
                    { number = number
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
