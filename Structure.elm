module Structure exposing (Structure, structure, sortLinesByDistanceToPoint)

import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.LineSegment3d as LineSegment3d exposing (LineSegment3d)
import OpenSolid.BoundingBox3d as BoundingBox3d exposing (BoundingBox3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)
import Array exposing (Array)


type alias Dimension =
    { direction : Vector3d
    , distance : Float
    , number : Int
    }


dimensions : List Dimension
dimensions =
    [ { direction = Vector3d.fromComponents ( 1, 0, 0 )
      , distance = 1
      }
    , { direction = Vector3d.fromComponents ( 0, 0, -1 )
      , distance = 1.618
      }
    , { direction = Vector3d.fromComponents ( 0, 1, 0 )
      , distance = 1.618 ^ 2
      }
    , { direction = Vector3d.fromComponents ( -1, 1, 1 )
      , distance = 1.618 ^ 3
      }
    , { direction = Vector3d.fromComponents ( 1, 1, 1 )
      , distance = 1.618 ^ 4
      }
    , { direction = Vector3d.fromComponents ( 1, -1, 1 )
      , distance = 1.618 ^ 5
      }
    , { direction = Vector3d.fromComponents ( 1, 1, -1 )
      , distance = 1.618 ^ 7
      }
    , { direction = Vector3d.fromComponents ( -1, -1, -1 )
      , distance = 1.618 ^ 8
      }
    ]
        |> List.indexedMap
            (\n { direction, distance } ->
                { direction = Vector3d.normalize direction
                , distance = distance
                , number = n
                }
            )


type alias Structure =
    { points : List Point
    , lines : List Line
    , value : Point3d
    , coordinates : List Line
    , hull : BoundingBox3d
    }


type alias Point =
    { number : Int
    , point : Point3d
    }


type alias Line =
    { number : Int
    , line : LineSegment3d
    }


sortLinesByDistanceToPoint : Point3d -> List Line -> List Line
sortLinesByDistanceToPoint point =
    List.sortBy (.line >> LineSegment3d.midpoint >> Point3d.distanceFrom point >> (*) -1)


structure : Array Float -> Int -> Structure
structure values n =
    let
        activeDimensions =
            List.take n dimensions

        newStructure =
            structureHelp activeDimensions values initial

        value =
            List.foldl
                (\{ number, distance, direction } ->
                    Point3d.translateBy
                        (Vector3d.scaleBy
                            (distance * (Array.get number values |> Maybe.withDefault 0))
                            direction
                        )
                )
                newStructure.value
                activeDimensions
    in
        { newStructure
            | hull =
                Point3d.hullOf (List.map .point newStructure.points)
                    |> Maybe.withDefault initial.hull
            , value = value
            , coordinates =
                List.map
                    (\{ number, distance, direction } ->
                        { line =
                            LineSegment3d.from
                                (Point3d.translateBy
                                    (Vector3d.scaleBy
                                        (-distance * (Array.get number values |> Maybe.withDefault 0))
                                        direction
                                    )
                                    value
                                )
                                value
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
    , hull = BoundingBox3d.singleton Point3d.origin
    }


structureHelp : List Dimension -> Array Float -> Structure -> Structure
structureHelp dimensions values structure =
    case dimensions of
        [] ->
            structure

        dimension :: restDimensions ->
            structureHelp restDimensions values (addDimension values dimension structure)


addDimension : Array Float -> Dimension -> Structure -> Structure
addDimension values { number, direction, distance } { value, coordinates, points, lines, hull } =
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
    , hull = hull
    , value = value
    , coordinates = coordinates
    }
