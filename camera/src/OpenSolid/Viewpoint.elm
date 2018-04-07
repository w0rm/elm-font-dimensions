module OpenSolid.Viewpoint
    exposing
        ( Viewpoint
        , eyePoint
        , lookAt
        , modelViewMatrix
        , viewDirection
        , viewMatrix
        , viewPlane
        , xDirection
        , yDirection
        )

{-|

@docs Viewpoint

Constructors

@docs lookAt

Properties

@docs eyePoint, viewDirection, viewPlane, xDirection, yDirection

Matrices

@docs viewMatrix, modelViewMatrix

-}

import Math.Matrix4 exposing (Mat4)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Interop.LinearAlgebra.Frame3d as Frame3d
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


{-| A `Viewpoint` represents the position and orientation of a camera in 3D.
-}
type Viewpoint
    = Viewpoint Frame3d


{-| Construct a `Viewpoint` at the given eye point looking towards the given
focal point, with the given global up direction (which will typically be
`Direction3d.positiveZ` or `Direction3d.positiveY`). For example, to construct a
viewpoint at the point (10, 0, 5) looking towards the origin:

    viewpoint =
        Viewpoint.lookAt
            { eyePoint = Point3d.fromCoordinates ( 10, 0, 5 )
            , focalPoint = Point3d.origin
            , upDirection = Direction3d.positiveZ
            }

    Viewpoint.eyePoint viewpoint
    --> Point3d.fromCoordinates ( 10, 0, 5 )

    Viewpoint.xDirection viewpoint
    --> Direction3d.positiveY

    Viewpoint.yDirection viewpoint
    --> Direction3d.with
    -->     { azimuth = degrees 180
    -->     , elevation = degrees 63.43
    -->     }

    Viewpoint.viewDirection viewpoint
    --> Direction3d.with
    -->     { azimuth = degrees 180
    -->     , elevation = degrees -26.57
    -->     }

That is likely all you need to know but if you are interested in the details and
corner cases, read on!

The view direction of the returned viewpoint will point from the eye point to
the focal point. The Y direction will be chosen to be as close to the global up
direction as possible (the viewpoint will not have any 'roll') and the X
direction will point to the right.

If the direction from the eye point to the focal point is parallel to the global
up direction (that is, the viewpoint represents looking straight up or straight
down) then the X and Y directions will be chosen arbitrarily.

If the given eye point and focal point are coincident (so that there is no well-
defined view direction), then the returned frame will have its Y direction set
to the global up direction and its X and view directions will be chosen
arbitrarily.

-}
lookAt : { focalPoint : Point3d, eyePoint : Point3d, upDirection : Direction3d } -> Viewpoint
lookAt { focalPoint, eyePoint, upDirection } =
    let
        zVector =
            Vector3d.from focalPoint eyePoint

        yVector =
            Direction3d.toVector upDirection

        xVector =
            Vector3d.crossProduct yVector zVector
    in
    case Direction3d.orthonormalize ( zVector, yVector, xVector ) of
        Just ( zDirection, yDirection, xDirection ) ->
            Viewpoint <|
                Frame3d.unsafe
                    { originPoint = eyePoint
                    , xDirection = xDirection
                    , yDirection = yDirection
                    , zDirection = zDirection
                    }

        Nothing ->
            case Vector3d.direction zVector of
                Just zDirection ->
                    -- The view vector must be parallel to the up direction,
                    -- since it is non-zero and therefore otherwise would have
                    -- resulted in a valid orthonormalization; therefore, choose
                    -- an arbitrary 'up' direction that is perpendicular to the
                    -- view direction
                    Viewpoint <|
                        Frame3d.with
                            { originPoint = eyePoint
                            , zDirection = zDirection
                            }

                Nothing ->
                    -- The view vector is zero (the eye point and focal point
                    -- are coincident), so construct an arbitrary frame with the
                    -- given up direction
                    let
                        ( zDirection, xDirection ) =
                            Direction3d.perpendicularBasis upDirection
                    in
                    Viewpoint <|
                        Frame3d.unsafe
                            { originPoint = eyePoint
                            , xDirection = xDirection
                            , yDirection = upDirection
                            , zDirection = zDirection
                            }


{-| Get the actual eye point of a viewpoint.
-}
eyePoint : Viewpoint -> Point3d
eyePoint (Viewpoint frame) =
    Frame3d.originPoint frame


{-| Get the viewing direction of a viewpoint.
-}
viewDirection : Viewpoint -> Direction3d
viewDirection (Viewpoint frame) =
    Direction3d.flip (Frame3d.zDirection frame)


{-| The view plane of a viewpoint is a `SketchPlane3d` perpendicular to the view
direction, with origin point equal to the eye point. For an observer looking
straight down the view direction, the X direction of the view plane points to
the right and the Y direction points up; this means that the view plane's normal
direction is the _opposite_ of the view direction. (Note that the Y direction
will _not_ be equal to the global up direction unless the view direction is
horizontal).
-}
viewPlane : Viewpoint -> SketchPlane3d
viewPlane (Viewpoint frame) =
    Frame3d.xySketchPlane frame


{-| Get the X (right) direction of a viewpoint's view plane.
-}
xDirection : Viewpoint -> Direction3d
xDirection (Viewpoint frame) =
    Frame3d.xDirection frame


{-| Get the Y (local up) direction of a viewpoint's view plane.
-}
yDirection : Viewpoint -> Direction3d
yDirection (Viewpoint frame) =
    Frame3d.yDirection frame


{-| Get the [view matrix](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#the-view-matrix)
of a viewpoint. Multiplying by this matrix transforms from world coordinates to
eye coordinates.
-}
viewMatrix : Viewpoint -> Mat4
viewMatrix (Viewpoint frame) =
    Frame3d.toMat4 (Frame3d.relativeTo frame Frame3d.xyz)


{-| Construct a WebGL model-view matrix given a viewpoint and a `Frame3d` that
defines the position and orientation of an object;

    Viewpoint.modelViewMatrix viewpoint modelFrame

is equivalent to

    Matrix4.mul
        (Viewpoint.viewMatrix viewpoint)
        (Frame3d.toMat4 modelFrame)

Multiplying by this matrix transforms from object coordinates to eye
coordinates.

-}
modelViewMatrix : Viewpoint -> Frame3d -> Mat4
modelViewMatrix (Viewpoint frame) modelFrame =
    Frame3d.toMat4 (Frame3d.relativeTo frame modelFrame)
