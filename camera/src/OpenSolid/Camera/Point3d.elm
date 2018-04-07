module OpenSolid.Camera.Point3d exposing (toScreenSpace)

{-|

@docs toScreenSpace

-}

import Math.Matrix4
import OpenSolid.Camera as Camera exposing (Camera)
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

        { m11, m12, m13, m14, m21, m22, m23, m24, m41, m42, m43, m44 } =
            Math.Matrix4.toRecord viewProjectionMatrix

        halfWidth =
            0.5 * Camera.screenWidth camera

        halfHeight =
            0.5 * Camera.screenHeight camera
    in
    \point ->
        let
            ( x, y, z ) =
                Point3d.coordinates point

            w =
                m41 * x + m42 * y + m43 * z + m44

            ndcX =
                (m11 * x + m12 * y + m13 * z + m14) / w

            ndcY =
                (m21 * x + m22 * y + m23 * z + m24) / w
        in
        Point2d.fromCoordinates
            ( halfWidth + halfWidth * ndcX
            , halfHeight + halfHeight * ndcY
            )
