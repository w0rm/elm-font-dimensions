module OpenSolid.Camera
    exposing
        ( Camera
        , modelViewMatrix
        , modelViewProjectionMatrix
        , orthographic
        , perspective
        , projectionMatrix
        , screenHeight
        , screenWidth
        , viewMatrix
        , viewpoint
        )

{-| This module contains functions for defining cameras in 3D and obtaining
WebGL view and projection matrices from them. This provides a convenient way
to construct view and projection matrices which then can be used with any
rendering framework or shaders that you want.

@docs Camera


# Constructors

Cameras have some commmon properties regardless of how they are constructed:

  - `viewpoint` defines the position and orientation of the camera in 3D space
  - `nearClipDistance` and `farClipDistance` specify the standard near and far
    clipping planes used when rendering
  - `screenWidth` and `screenHeight` specify the dimensions in pixels of the
    screen that will be rendered to (the dimensions of the actual WebGl
    element). This is used to determine aspect ratio when constructing the
    camera's projection matrix, but also when using other functions in this
    package such as `Point3d.toScreenSpace` which convert from 3D model space
    to 2D pixel coordinates for a given camera.

@docs perspective, orthographic


# Properties

@docs viewpoint, screenHeight, screenWidth


# Matrices

@docs viewMatrix, modelViewMatrix, projectionMatrix, modelViewProjectionMatrix

-}

import Basics.Extra exposing (inDegrees)
import Math.Matrix4 as Matrix4 exposing (Mat4)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Viewpoint as Viewpoint exposing (Viewpoint)


{-| A perspective or orthographic camera in 3D, encapsulating the camera's
viewpoint and projection matrix as well as the dimensions of the screen the
camera renders to.
-}
type Camera
    = Camera
        { viewpoint : Viewpoint
        , projectionMatrix : Mat4
        , screenWidth : Float
        , screenHeight : Float
        }


{-| Create a perspective camera with the common camera properties plus vertical
field of view given in radians. (The horizontal field of view will be chosen to
match the aspect ratio of the screen given by `screenWidth` and `screenHeight`.)

    perspectiveCamera =
        Camera.perspective
            { viewpoint = cameraViewpoint
            , verticalFieldOfView = degrees 30
            , nearClipDistance = 0.1
            , farClipDistance = 1000
            , screenWidth = 1024
            , screenHeight = 768
            }

-}
perspective : { viewpoint : Viewpoint, screenWidth : Float, screenHeight : Float, verticalFieldOfView : Float, nearClipDistance : Float, farClipDistance : Float } -> Camera
perspective { viewpoint, screenWidth, screenHeight, verticalFieldOfView, nearClipDistance, farClipDistance } =
    let
        aspectRatio =
            screenWidth / screenHeight

        fovInDegrees =
            verticalFieldOfView |> inDegrees

        projectionMatrix =
            Matrix4.makePerspective fovInDegrees aspectRatio nearClipDistance farClipDistance
    in
    Camera
        { viewpoint = viewpoint
        , screenWidth = screenWidth
        , screenHeight = screenHeight
        , projectionMatrix = projectionMatrix
        }


{-| Create an orthographic camera with the common camera properties plus the
height of the orthographic viewport: this is the height in 3D model units of the
section of the model to be rendered. (The width will be chosen to match the
aspect ratio of the screen given by `screenWidth` and `screenHeight`.)

    orthographicCamera =
        Camera.orthographic
            { viewpoint = cameraViewpoint
            , viewportHeight = 5
            , nearClipDistance = 0.1
            , farClipDistance = 1000
            , screenWidth = 1024
            , screenHeight = 768
            }

-}
orthographic : { viewpoint : Viewpoint, screenWidth : Float, screenHeight : Float, viewportHeight : Float, nearClipDistance : Float, farClipDistance : Float } -> Camera
orthographic { viewpoint, screenWidth, screenHeight, viewportHeight, nearClipDistance, farClipDistance } =
    let
        aspectRatio =
            screenWidth / screenHeight

        viewportWidth =
            aspectRatio * viewportHeight

        left =
            -viewportWidth / 2

        right =
            viewportWidth / 2

        bottom =
            -viewportHeight / 2

        top =
            viewportHeight / 2

        projectionMatrix =
            Matrix4.makeOrtho left right bottom top nearClipDistance farClipDistance
    in
    Camera
        { viewpoint = viewpoint
        , screenWidth = screenWidth
        , screenHeight = screenHeight
        , projectionMatrix = projectionMatrix
        }


{-| Get the viewpoint defining the position and orientation of a camera.
-}
viewpoint : Camera -> Viewpoint
viewpoint (Camera properties) =
    properties.viewpoint


{-| Get the width of the screen rendered to by a camera.
-}
screenWidth : Camera -> Float
screenWidth (Camera properties) =
    properties.screenWidth


{-| Get the height of the screen rendered to by a camera.
-}
screenHeight : Camera -> Float
screenHeight (Camera properties) =
    properties.screenHeight


{-| Get the [view matrix](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#the-view-matrix)
of a camera;

    Camera.viewMatrix camera

is shorthand for

    Viewpoint.viewMatrix (Camera.viewpoint camera)

-}
viewMatrix : Camera -> Mat4
viewMatrix camera =
    Viewpoint.viewMatrix (viewpoint camera)


{-| Construct a WebGL model-view matrix given a camera and a `Frame3d` that
defines the position and orientation of an object;

    Camera.modelViewMatrix camera modelFrame

is shorthand for

    Viewpoint.modelViewMatrix
        (Camera.viewpoint camera)
        modelFrame

-}
modelViewMatrix : Camera -> Frame3d -> Mat4
modelViewMatrix camera modelFrame =
    Viewpoint.modelViewMatrix (viewpoint camera) modelFrame


{-| Get the [projection matrix](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#the-projection-matrix)
of a camera. Multiplying by this matrix converts from eye coordinates to WebGL
normalized device coordinates.
-}
projectionMatrix : Camera -> Mat4
projectionMatrix (Camera properties) =
    properties.projectionMatrix


{-| Get the full model-view-projection matrix given a camera and a `Frame3d`
that defines the position and orientation of an object;

    Camera.modelViewProjectionMatrix camera modelFrame

is equivalent to

    Matrix4.mul (Camera.projectionMatrix camera)
        (Camera.modelViewMatrix camera modelFrame)

-}
modelViewProjectionMatrix : Camera -> Frame3d -> Mat4
modelViewProjectionMatrix camera modelFrame =
    Matrix4.mul (projectionMatrix camera) (modelViewMatrix camera modelFrame)
