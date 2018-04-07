module Teapot exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import Mouse
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.Camera as Camera exposing (Camera)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Interop.LinearAlgebra.Direction3d as Direction3d
import OpenSolid.Interop.LinearAlgebra.Frame3d as Frame3d
import OpenSolid.Interop.LinearAlgebra.Point3d as Point3d
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)
import OpenSolid.Viewpoint as Viewpoint
import SingleTouch
import Task
import Touch exposing (Touch, TouchEvent(..))
import WebGL exposing (Mesh)
import Window


-- Types


type Msg
    = StartRotatingAt Point2d
    | PointerMovedTo Point2d
    | StopRotating
    | SetWindowSize Window.Size
    | LoadModel (Result Http.Error (Mesh Attributes))


type alias Model =
    { placementFrame : Frame3d
    , mesh : () -> Maybe (Mesh Attributes)
    , dragPoint : Maybe Point2d
    , windowSize : Maybe Window.Size
    }


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }


type alias Uniforms =
    { modelMatrix : Mat4
    , viewMatrix : Mat4
    , projectionMatrix : Mat4
    , lightDirection : Vec3
    , faceColor : Vec3
    }


type alias Varyings =
    { interpolatedPosition : Vec3
    , interpolatedNormal : Vec3
    }



-- Constants


initialFrame : Frame3d
initialFrame =
    Frame3d.xyz
        |> Frame3d.rotateAround Axis3d.z (degrees -30)
        |> Frame3d.rotateAround Axis3d.y (degrees 20)


lightDirection : Direction3d
lightDirection =
    Vector3d.fromComponents ( -1, -1, -2 )
        |> Vector3d.direction
        |> Maybe.withDefault Direction3d.negativeZ


faceColor : Vec3
faceColor =
    vec3 0.2 0.3 0.9



-- Model loading


accumulateVertices : List Float -> List Point3d -> List Point3d
accumulateVertices coordinates accumulated =
    case coordinates of
        x :: y :: z :: rest ->
            accumulateVertices rest
                (Point3d.fromCoordinates ( x, y, z ) :: accumulated)

        _ ->
            List.reverse accumulated


accumulateNormals : List Float -> List Direction3d -> List Direction3d
accumulateNormals components accumulated =
    case components of
        x :: y :: z :: rest ->
            accumulateNormals rest
                (Direction3d.unsafe ( x, y, z ) :: accumulated)

        _ ->
            List.reverse accumulated


accumulateFaces : List Int -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
accumulateFaces indices accumulated =
    case indices of
        a :: b :: c :: d :: e :: f :: g :: h :: rest ->
            accumulateFaces rest (( b, c, d ) :: accumulated)

        _ ->
            List.reverse accumulated


meshDecoder : Decoder (Mesh Attributes)
meshDecoder =
    Decode.map3
        (\vertexData normalData faceData ->
            let
                frame =
                    Frame3d.xyz
                        |> Frame3d.rotateAround Axis3d.x (degrees 90)
                        |> Frame3d.translateBy
                            (Vector3d.fromComponents ( 0, 0, -1 ))

                vertices =
                    accumulateVertices vertexData []
                        |> List.map (Point3d.placeIn frame)

                normals =
                    accumulateNormals normalData []
                        |> List.map (Direction3d.placeIn frame)

                faces =
                    accumulateFaces faceData []

                attributes =
                    List.map2
                        (\vertex normal ->
                            { position = Point3d.toVec3 vertex
                            , normal = Direction3d.toVec3 normal
                            }
                        )
                        vertices
                        normals
            in
            WebGL.indexedTriangles attributes faces
        )
        (Decode.field "vertices" (Decode.list Decode.float))
        (Decode.field "normals" (Decode.list Decode.float))
        (Decode.field "faces" (Decode.list Decode.int))



-- Rendering


camera : Window.Size -> Camera
camera { width, height } =
    Camera.perspective
        { viewpoint =
            Viewpoint.lookAt
                { eyePoint = Point3d.fromCoordinates ( 15, 0, 0 )
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.z
                }
        , verticalFieldOfView = degrees 30
        , screenWidth = toFloat width
        , screenHeight = toFloat height
        , nearClipDistance = 0.1
        , farClipDistance = 100
        }


vertexShader : WebGL.Shader Attributes Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 viewMatrix;
        uniform mat4 modelMatrix;
        uniform mat4 projectionMatrix;
        varying vec3 interpolatedPosition;
        varying vec3 interpolatedNormal;

        void main () {
          gl_Position = projectionMatrix * viewMatrix * modelMatrix * vec4(position, 1.0);
          interpolatedPosition = (modelMatrix * vec4(position, 1.0)).xyz;
          interpolatedNormal = (modelMatrix * vec4(normal, 0.0)).xyz;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 lightDirection;
        uniform vec3 faceColor;
        varying vec3 interpolatedPosition;
        varying vec3 interpolatedNormal;

        void main () {
            vec3 normal = normalize(interpolatedNormal);
            float dotProduct = dot(-normal, lightDirection);
            float intensity = 0.4 + 0.6 * clamp(dotProduct, 0.0, 1.0);
            gl_FragColor = vec4(faceColor * intensity, 1.0);
        }
    |]


entity : Mesh Attributes -> Frame3d -> Window.Size -> WebGL.Entity
entity mesh placementFrame windowSize =
    let
        camera_ =
            camera windowSize

        uniforms =
            { projectionMatrix = Camera.projectionMatrix camera_
            , modelMatrix = Frame3d.toMat4 placementFrame
            , viewMatrix = Camera.viewMatrix camera_
            , lightDirection = Direction3d.toVec3 lightDirection
            , faceColor = faceColor
            }
    in
    WebGL.entity vertexShader fragmentShader mesh uniforms



-- Interactivity


mousePositionToPoint : Mouse.Position -> Point2d
mousePositionToPoint mousePosition =
    Point2d.fromCoordinates ( toFloat mousePosition.x, toFloat mousePosition.y )


touchToPoint : Touch -> Point2d
touchToPoint touch =
    Point2d.fromCoordinates ( touch.clientX, touch.clientY )


init : ( Model, Cmd Msg )
init =
    let
        model =
            { placementFrame = initialFrame
            , mesh = always Nothing
            , dragPoint = Nothing
            , windowSize = Nothing
            }

        cmds =
            Cmd.batch
                [ Task.perform SetWindowSize Window.size
                , Http.send LoadModel (Http.get "teapot.json" meshDecoder)
                ]
    in
    ( model, cmds )


dragAttributes : List (Attribute Msg)
dragAttributes =
    let
        onMouseDown pointToMsg =
            Events.on "mousedown" Mouse.position
                |> Attributes.map (mousePositionToPoint >> pointToMsg)

        onTouch touchEvent pointToMsg =
            SingleTouch.onSingleTouch touchEvent Touch.preventAndStop .touch
                |> Attributes.map (touchToPoint >> pointToMsg)
    in
    [ onMouseDown StartRotatingAt
    , onTouch TouchStart StartRotatingAt
    , onTouch TouchMove PointerMovedTo
    , onTouch TouchEnd (always StopRotating)
    , onTouch TouchCancel (always StopRotating)
    ]


view : Model -> Html Msg
view model =
    case ( model.windowSize, model.mesh () ) of
        ( Just windowSize, Just mesh ) ->
            let
                blockAttribute =
                    Attributes.style [ ( "display", "block" ) ]

                widthAttribute =
                    Attributes.width windowSize.width

                heightAttribute =
                    Attributes.height windowSize.height

                options =
                    [ WebGL.clearColor 0 0 0 1
                    , WebGL.depth 1
                    , WebGL.antialias
                    ]

                attributes =
                    blockAttribute :: widthAttribute :: heightAttribute :: dragAttributes

                entities =
                    [ entity mesh model.placementFrame windowSize ]
            in
            WebGL.toHtmlWith options attributes entities

        _ ->
            Html.text "Loading model..."


rotate : Frame3d -> Float -> Float -> Frame3d
rotate frame dx dy =
    let
        dragVector =
            Vector2d.fromComponents ( dx, dy )
    in
    case Vector2d.direction dragVector of
        Just direction2d ->
            let
                axialDirection =
                    Direction3d.on SketchPlane3d.yz <|
                        Direction2d.perpendicularTo direction2d

                rotationAxis =
                    Axis3d.with
                        { originPoint = Point3d.origin
                        , direction = axialDirection
                        }

                rotationAngle =
                    degrees 1 * Vector2d.length dragVector
            in
            frame |> Frame3d.rotateAround rotationAxis rotationAngle

        Nothing ->
            frame


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        StartRotatingAt startPoint ->
            ( { model | dragPoint = Just startPoint }, Cmd.none )

        StopRotating ->
            ( { model | dragPoint = Nothing }, Cmd.none )

        PointerMovedTo newPoint ->
            case model.dragPoint of
                Just lastPoint ->
                    let
                        ( dx, dy ) =
                            Vector2d.from lastPoint newPoint
                                |> Vector2d.components

                        rotatedFrame =
                            rotate model.placementFrame dx -dy

                        updatedModel =
                            { model
                                | placementFrame = rotatedFrame
                                , dragPoint = Just newPoint
                            }
                    in
                    ( updatedModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetWindowSize windowSize ->
            ( { model | windowSize = Just windowSize }, Cmd.none )

        LoadModel result ->
            case result of
                Ok mesh ->
                    ( { model | mesh = always (Just mesh) }, Cmd.none )

                Err _ ->
                    ( { model | mesh = always Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        dragEvents =
            case model.dragPoint of
                Just _ ->
                    Sub.batch
                        [ Mouse.moves (mousePositionToPoint >> PointerMovedTo)
                        , Mouse.ups (always StopRotating)
                        ]

                Nothing ->
                    Sub.none
    in
    Sub.batch [ dragEvents, Window.resizes SetWindowSize ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
