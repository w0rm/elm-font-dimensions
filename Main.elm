module Main exposing (main)

import Html exposing (Html)
import Html.Events as Events
import Svg exposing (Svg)
import Svg.Attributes as SvgAttributes
import Html.Attributes as HtmlAttributes
import OpenSolid.Svg as Svg
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Direction2d as Direction2d
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import Window
import Task
import Mouse
import Array exposing (Array)
import Structure exposing (Structure, Dimension)
import Json.Decode as Decode
import OpenSolid.Camera as Camera exposing (Camera)
import OpenSolid.Viewpoint as Viewpoint exposing (Viewpoint)
import OpenSolid.Camera.LineSegment3d as LineSegment3d
import OpenSolid.Camera.Point3d as Point3d


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { width : Float
    , height : Float
    , mouse : Maybe Point2d
    , sketchPlane : SketchPlane3d
    , structure : Structure
    , dimensionsCount : Float
    , dimensions : List Dimension
    , perspective : Float
    , text : String
    }


init : ( Model, Cmd Msg )
init =
    ( { width = 1
      , height = 1
      , mouse = Nothing
      , sketchPlane =
            SketchPlane3d.xy
                |> SketchPlane3d.rotateAroundOwn SketchPlane3d.xAxis (-pi / 8)
                |> SketchPlane3d.rotateAroundOwn SketchPlane3d.yAxis (-pi / 13)
      , structure = Structure.structure Structure.dimensions 0
      , dimensionsCount = 0
      , dimensions = Structure.dimensions
      , perspective = 0
      , text = "Afg"
      }
    , Task.perform Resize Window.size
    )


type Msg
    = Resize Window.Size
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | MouseMove Mouse.Position
    | ChangeDimensions Float
    | ChangeValue Int Float
    | ChangePerspective Float
    | ChangeText String
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ChangeValue number value ->
            let
                newDimensions =
                    List.map
                        (\dimension ->
                            if dimension.number == number then
                                { dimension | value = value }
                            else
                                dimension
                        )
                        model.dimensions
            in
                ( { model
                    | dimensions = newDimensions
                    , structure = Structure.structure newDimensions (round model.dimensionsCount)
                  }
                , Cmd.none
                )

        ChangeDimensions dimensionsCount ->
            ( { model
                | dimensionsCount = dimensionsCount
                , structure = Structure.structure model.dimensions (round dimensionsCount)
              }
            , Cmd.none
            )

        ChangePerspective perspective ->
            ( { model | perspective = perspective }, Cmd.none )

        ChangeText text ->
            ( { model | text = text }, Cmd.none )

        Resize { width, height } ->
            ( { model | width = toFloat width, height = toFloat height }, Cmd.none )

        MouseDown { x, y } ->
            ( { model | mouse = Just (Point2d.fromCoordinates ( toFloat x, toFloat y )) }, Cmd.none )

        MouseUp _ ->
            ( { model | mouse = Nothing }, Cmd.none )

        MouseMove { x, y } ->
            case model.mouse of
                Just source ->
                    let
                        deltaX =
                            toFloat x - Point2d.xCoordinate source

                        deltaY =
                            toFloat y - Point2d.yCoordinate source
                    in
                        ( { model
                            | sketchPlane =
                                model.sketchPlane
                                    |> SketchPlane3d.rotateAroundOwn SketchPlane3d.xAxis (-0.0001 * deltaY)
                                    |> SketchPlane3d.rotateAroundOwn SketchPlane3d.yAxis (-0.0001 * deltaX)
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Mouse.downs MouseDown
        , model.mouse
            |> Maybe.map (always (Mouse.moves MouseMove))
            |> Maybe.withDefault Sub.none
        , Mouse.ups MouseUp
        ]


viewStructure : Model -> List (Svg Msg)
viewStructure model =
    let
        distance =
            10 - model.perspective * 0.9

        eyePoint =
            model.sketchPlane
                |> SketchPlane3d.moveTo model.structure.focalPoint
                |> SketchPlane3d.offsetBy -distance
                |> SketchPlane3d.originPoint

        upDirection =
            SketchPlane3d.yDirection model.sketchPlane

        viewpoint =
            Viewpoint.lookAt
                { eyePoint = eyePoint
                , focalPoint = model.structure.focalPoint
                , upDirection = upDirection
                }

        camera =
            Camera.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = degrees 30
                , nearClipDistance = 0.1
                , farClipDistance = 1000
                , screenWidth = model.width
                , screenHeight = model.height
                }

        scale2d =
            distance * tan (degrees 15) * 2

        line3dToScreenSpace =
            LineSegment3d.toScreenSpace camera

        point3dToScreenSpace =
            Point3d.toScreenSpace camera

        screenCenter =
            Point2d.fromCoordinates ( model.width / 2, model.height / 2 )

        strokeWidth =
            SvgAttributes.strokeWidth (toString (max 1 (8 - model.dimensionsCount)))

        mapLine { number, line } =
            Svg.lineSegment2d [ SvgAttributes.stroke (color number), strokeWidth ]
                (line
                    |> line3dToScreenSpace
                    |> LineSegment2d.scaleAbout screenCenter scale2d
                )

        mapCoordinate { number, line } =
            Svg.lineSegment2d
                [ SvgAttributes.stroke (color number)
                , SvgAttributes.strokeOpacity "0.5"
                , SvgAttributes.strokeDasharray "10 10"
                , strokeWidth
                ]
                (line
                    |> line3dToScreenSpace
                    |> LineSegment2d.scaleAbout screenCenter scale2d
                )

        mapPoint { number, point } =
            Svg.point2dWith { radius = 10 - model.dimensionsCount }
                [ SvgAttributes.fill (color number) ]
                (point
                    |> point3dToScreenSpace
                    |> Point2d.scaleAbout screenCenter scale2d
                )

        mapValue point =
            Svg.point2dWith { radius = 10 - model.dimensionsCount }
                [ SvgAttributes.fill "#fff"
                , SvgAttributes.stroke "#000"
                , SvgAttributes.strokeWidth "2"
                ]
                (point
                    |> point3dToScreenSpace
                    |> Point2d.scaleAbout screenCenter scale2d
                )
    in
        List.concat
            [ model.structure.coordinates
                |> List.map mapCoordinate
            , model.structure.lines
                |> Structure.sortLinesByDistanceToPoint eyePoint
                |> List.map mapLine
            , List.map mapPoint model.structure.points
            , [ mapValue model.structure.value ]
            ]


view : Model -> Html Msg
view model =
    let
        centerFrame =
            Frame2d.with
                { originPoint = Point2d.fromCoordinates ( -model.width / 2, -model.height / 2 )
                , xDirection = Direction2d.positiveX
                }
    in
        Html.div
            [ HtmlAttributes.style
                [ ( "position", "absolute" )
                , ( "width", "100%" )
                , ( "height", "100%" )
                ]
            ]
            [ viewStyle
            , Svg.svg
                [ SvgAttributes.width (toString model.width)
                , SvgAttributes.height (toString model.height)
                , HtmlAttributes.style [ ( "display", "block" ) ]
                ]
                (viewStructure model)
            , viewLetter model.dimensions model.text
            , Html.div
                [ HtmlAttributes.style
                    [ ( "position", "absolute" )
                    , ( "right", "30px" )
                    , ( "width", "200px" )
                    , ( "top", "30px" )
                    ]
                , Events.onWithOptions "mousedown"
                    { stopPropagation = True, preventDefault = False }
                    (Decode.succeed Noop)
                ]
                [ Html.label
                    [ HtmlAttributes.for "dimensions"
                    , HtmlAttributes.style [ ( "display", "block" ) ]
                    ]
                    [ Html.text ("Dimensions: " ++ toString (round model.dimensionsCount))
                    ]
                , Html.input
                    [ HtmlAttributes.id "dimensions"
                    , HtmlAttributes.type_ "range"
                    , HtmlAttributes.value (toString model.dimensionsCount)
                    , Events.onInput (String.toFloat >> Result.withDefault 0 >> ChangeDimensions)
                    , HtmlAttributes.min "0"
                    , HtmlAttributes.max "8"
                    , HtmlAttributes.step "1"
                    ]
                    []
                , Html.label
                    [ HtmlAttributes.for "perspective"
                    , HtmlAttributes.style [ ( "display", "block" ) ]
                    ]
                    [ Html.text "Perspective"
                    ]
                , Html.input
                    [ HtmlAttributes.id "perspective"
                    , HtmlAttributes.type_ "range"
                    , HtmlAttributes.value (toString model.perspective)
                    , Events.onInput (String.toFloat >> Result.withDefault 0 >> ChangePerspective)
                    , HtmlAttributes.min "0"
                    , HtmlAttributes.max "10"
                    , HtmlAttributes.step "0.5"
                    ]
                    []
                , Html.div
                    []
                    (List.map
                        viewSlider
                        (List.take (round model.dimensionsCount) model.dimensions)
                    )
                ]
            ]


viewLetter : List Dimension -> String -> Html Msg
viewLetter valuesAndScales text =
    let
        fontVariationSettings =
            valuesAndScales
                -- only 3 dimensions are supported by 'Voto Serif GX'
                |> List.take 3
                |> List.map
                    (\{ value, name, min, max } ->
                        "'" ++ name ++ "' " ++ toString (round (min + (max - min) * value))
                    )
                |> String.join ", "
    in
        Html.div
            [ HtmlAttributes.style
                [ ( "position", "absolute" )
                , ( "bottom", "0" )
                , ( "left", "0" )
                , ( "max-width", "100%" )
                , ( "box-sizing", "border-box" )
                , ( "padding", "0 50px" )
                , ( "outline", "none" )
                , ( "font-family", "'Voto Serif GX'" )
                , ( "font-size", "200px" )
                , ( "line-height", "1.5" )
                , ( "text-rendering", "optimizeLegibility" )
                , ( "font-variation-settings", fontVariationSettings )
                ]
            , Events.onWithOptions "mousedown"
                { stopPropagation = True, preventDefault = False }
                (Decode.succeed Noop)
            , Events.onInput ChangeText
            , HtmlAttributes.spellcheck False
            , HtmlAttributes.autocomplete False
            , HtmlAttributes.contenteditable True
            , Events.onInput ChangeText
            ]
            [ Html.text text ]


viewSlider : Dimension -> Html Msg
viewSlider { number, value, title, min, max, step } =
    Html.div []
        [ Html.label
            [ HtmlAttributes.for title
            , HtmlAttributes.style [ ( "display", "block" ) ]
            ]
            [ Html.text ("Axis " ++ toString (number + 1) ++ " ")
            , Html.em [] [ Html.text title ]
            , Html.text (" = " ++ toString (round (min + (max - min) * value)))
            ]
        , Html.input
            [ HtmlAttributes.id title
            , HtmlAttributes.type_ "range"
            , HtmlAttributes.value (toString value)
            , HtmlAttributes.style [ ( "color", color number ) ]
            , Events.onInput (String.toFloat >> Result.withDefault 0 >> ChangeValue number)
            , HtmlAttributes.min "0"
            , HtmlAttributes.max "1"
            , HtmlAttributes.step (toString (step / (abs (max - min))))
            ]
            []
        ]


color : Int -> String
color number =
    Array.get number colors
        |> Maybe.withDefault "transparent"


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
        ]


viewStyle : Html Msg
viewStyle =
    Html.node "style"
        []
        [ Html.text """
@font-face {
  font-family: 'Voto Serif GX';
  src: url('https://www.axis-praxis.org/fonts/webfonts/VotoSerifGX.latin1.ttf') format('truetype');
}
input[type=range] {
  -webkit-appearance: none;
  margin: 18px 0;
  width: 100%;
}
input[type=range]:focus {
  outline: none;
}
input[type=range]::-webkit-slider-runnable-track {
  width: 100%;
  height: 2px;
  cursor: pointer;
  background: currentColor;
}
input[type=range]::-webkit-slider-thumb {
  height: 16px;
  width: 16px;
  border-radius: 8px;
  background: currentColor;
  cursor: pointer;
  -webkit-appearance: none;
  margin-top: -8px;
}
input[type=range]:focus::-webkit-slider-runnable-track {
  background: currentColor;
}
input[type=range]::-moz-range-track {
  width: 100%;
  height: 2px;
  cursor: pointer;
  background: currentColor;
}
input[type=range]::-moz-range-thumb {
  height: 16px;
  width: 16px;
  border-radius: 8px;
  background: currentColor;
  cursor: pointer;
}
input[type=range]::-ms-track {
  width: 100%;
  height: 2px;
  cursor: pointer;
  background: currentColor;
  border-color: transparent;
  border-width: 8px 0;
  color: transparent;
}
input[type=range]::-ms-fill-lower {
  background: transparent;
}
input[type=range]::-ms-fill-upper {
  background: transparent;
}
input[type=range]::-ms-thumb {
  height: 16px;
  width: 16px;
  border-radius: 8px;
  background: currentColor;
  cursor: pointer;
}
input[type=range]:focus::-ms-fill-lower {
  background: transparent;
}
input[type=range]:focus::-ms-fill-upper {
  background: transparent;
}
""" ]
