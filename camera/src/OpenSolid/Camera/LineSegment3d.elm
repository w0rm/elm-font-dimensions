module OpenSolid.Camera.LineSegment3d exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import OpenSolid.Camera exposing (Camera)
import OpenSolid.Camera.Point3d as Point3d
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.LineSegment3d as LineSegment3d exposing (LineSegment3d)


{-| Convert a line segment from 3D space to 2D screen (pixel) coordinates. The
result will be in a coordinate system where (0,0) is the bottom left of the
screen.
-}
toScreenSpace : Camera -> LineSegment3d -> LineSegment2d
toScreenSpace camera =
    let
        project =
            Point3d.toScreenSpace camera
    in
    \lineSegment ->
        let
            ( p1, p2 ) =
                LineSegment3d.endpoints lineSegment
        in
        LineSegment2d.fromEndpoints ( project p1, project p2 )
