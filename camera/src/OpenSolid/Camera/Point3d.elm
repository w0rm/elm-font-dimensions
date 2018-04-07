module OpenSolid.Camera.Point3d exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import Math.Matrix4
import OpenSolid.Camera as Camera exposing (Camera)
import OpenSolid.Interop.LinearAlgebra.Point3d as Point3d
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)


{-| Convert a point from 3D space to 2D screen (pixel) coordinates. The result
will be in a coordinate system where (0,0) is the bottom left of the screen.
-}
toScreenSpace : Camera -> Point3d -> Point2d
toScreenSpace camera =
    let
        projectionMatrix =
            Camera.projectionMatrix camera

        viewMatrix =
            Camera.viewMatrix camera

        viewProjectionMatrix =
            Math.Matrix4.mul projectionMatrix viewMatrix

        halfWidth =
            0.5 * Camera.screenWidth camera

        halfHeight =
            0.5 * Camera.screenHeight camera
    in
    \point ->
        let
            ndcPoint =
                Point3d.transformBy viewProjectionMatrix point

            ( ndcX, ndcY, _ ) =
                Point3d.coordinates ndcPoint
        in
        Point2d.fromCoordinates
            ( halfWidth + halfWidth * ndcX
            , halfHeight + halfHeight * ndcY
            )
