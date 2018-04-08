port module Main exposing (main)

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
import Structure exposing (Structure, Dimension, LoadedDimension)
import Json.Decode as Decode
import OpenSolid.Camera as Camera exposing (Camera)
import OpenSolid.Viewpoint as Viewpoint exposing (Viewpoint)
import OpenSolid.Camera.LineSegment3d as LineSegment3d
import OpenSolid.Camera.Point3d as Point3d


port loadFont :
    ({ url : String, dimensions : List LoadedDimension } -> msg)
    -> Sub msg


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
    , dimensionsCount : Int
    , dimensions : List Dimension
    , perspective : Float
    , url : String
    , text : String
    , tab : Tab
    }


type Tab
    = Values
    | Scales


tabs : List ( String, Tab )
tabs =
    [ ( "Values", Values )
    , ( "Scales", Scales )
    ]


initialDimensions : List Dimension
initialDimensions =
    Structure.dimensions
        [ { name = "wght"
          , title = "Weight"
          , min = 28
          , max = 194
          , value = 94
          }
        , { name = "wdth"
          , title = "Width"
          , min = 50
          , max = 130
          , value = 100
          }
        , { name = "opsz"
          , title = "Optical Size"
          , min = 12
          , max = 72
          , value = 12
          }
        ]


init : ( Model, Cmd Msg )
init =
    ( { width = 1
      , height = 1
      , mouse = Nothing
      , sketchPlane =
            SketchPlane3d.xy
                |> SketchPlane3d.rotateAroundOwn SketchPlane3d.xAxis (-pi / 8)
                |> SketchPlane3d.rotateAroundOwn SketchPlane3d.yAxis (-pi / 13)
      , structure = Structure.structure initialDimensions (List.length initialDimensions)
      , dimensionsCount = List.length initialDimensions
      , dimensions = initialDimensions
      , perspective = 0
      , text = "Afg"
      , tab = Values
      , url = "https://www.axis-praxis.org/fonts/webfonts/VotoSerifGX.latin1.ttf"
      }
    , Task.perform Resize Window.size
    )


type Msg
    = Resize Window.Size
    | MouseDown Mouse.Position
    | MouseUp Mouse.Position
    | MouseMove Mouse.Position
    | ChangeDimensions Int
    | ChangeValue Int Float
    | ChangeDistance Int Float
    | ChangePerspective Float
    | ChangeTab Tab
    | LoadFont { url : String, dimensions : List LoadedDimension }
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadFont font ->
            let
                dimensions =
                    Structure.dimensions font.dimensions
            in
                ( { model
                    | url = font.url
                    , dimensions = dimensions
                    , dimensionsCount = List.length dimensions
                    , structure = Structure.structure dimensions (List.length dimensions)
                  }
                , Cmd.none
                )

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
                    , structure = Structure.structure newDimensions model.dimensionsCount
                  }
                , Cmd.none
                )

        ChangeDistance number distance ->
            let
                newDimensions =
                    List.map
                        (\dimension ->
                            if dimension.number == number then
                                { dimension | distance = distance }
                            else
                                dimension
                        )
                        model.dimensions
            in
                ( { model
                    | dimensions = newDimensions
                    , structure = Structure.structure newDimensions model.dimensionsCount
                  }
                , Cmd.none
                )

        ChangeDimensions dimensionsCount ->
            ( { model
                | dimensionsCount = dimensionsCount
                , structure = Structure.structure model.dimensions dimensionsCount
              }
            , Cmd.none
            )

        ChangeTab tab ->
            ( { model | tab = tab }, Cmd.none )

        ChangePerspective perspective ->
            ( { model | perspective = perspective }, Cmd.none )

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
        , loadFont LoadFont
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
            SvgAttributes.strokeWidth (toString (max 1 (8 - toFloat model.dimensionsCount)))

        mapLine { color, line } =
            Svg.lineSegment2d [ SvgAttributes.stroke color, strokeWidth ]
                (line
                    |> line3dToScreenSpace
                    |> LineSegment2d.scaleAbout screenCenter scale2d
                )

        mapCoordinate { color, line } =
            Svg.lineSegment2d
                [ SvgAttributes.stroke color
                , SvgAttributes.strokeOpacity "0.5"
                , SvgAttributes.strokeDasharray "10 10"
                , strokeWidth
                ]
                (line
                    |> line3dToScreenSpace
                    |> LineSegment2d.scaleAbout screenCenter scale2d
                )

        mapPoint { color, point } =
            Svg.point2dWith { radius = 10 - toFloat model.dimensionsCount }
                [ SvgAttributes.fill color ]
                (point
                    |> point3dToScreenSpace
                    |> Point2d.scaleAbout screenCenter scale2d
                )

        mapValue point =
            Svg.point2dWith { radius = 10 - toFloat model.dimensionsCount }
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
            [ viewStyle model.url
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
                    [ Html.text ("Dimensions: " ++ toString model.dimensionsCount)
                    ]
                , Html.input
                    [ HtmlAttributes.id "dimensions"
                    , HtmlAttributes.type_ "range"
                    , HtmlAttributes.style [ ( "color", "#909090" ) ]
                    , Events.onInput (String.toInt >> Result.withDefault 0 >> ChangeDimensions)
                    , HtmlAttributes.min "0"
                    , HtmlAttributes.max (List.length model.dimensions |> toString)
                    , HtmlAttributes.step "1"
                    , HtmlAttributes.value (toString model.dimensionsCount)
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
                    , HtmlAttributes.style [ ( "color", "#909090" ) ]
                    , Events.onInput (String.toFloat >> Result.withDefault 0 >> ChangePerspective)
                    , HtmlAttributes.min "0"
                    , HtmlAttributes.max "10"
                    , HtmlAttributes.step "0.5"
                    , HtmlAttributes.value (toString model.perspective)
                    ]
                    []
                , Html.div
                    [ HtmlAttributes.style
                        [ ( "border-bottom", "1px solid #909090" )
                        , ( "height", "35px" )
                        , ( "line-height", "35px" )
                        , ( "margin", "10px 0" )
                        , ( "display"
                          , if model.dimensionsCount == 0 then
                                "none"
                            else
                                "block"
                          )
                        ]
                    ]
                    (List.map (viewTab model.tab) tabs)
                , Html.div
                    []
                    (List.map
                        (case model.tab of
                            Values ->
                                viewValueSlider

                            Scales ->
                                viewScaleSlider
                        )
                        (List.take model.dimensionsCount model.dimensions)
                    )
                ]
            ]


viewTab : Tab -> ( String, Tab ) -> Html Msg
viewTab activeTab ( title, tab ) =
    if activeTab == tab then
        Html.span
            [ HtmlAttributes.style
                [ ( "border", "1px solid #909090" )
                , ( "border-bottom", "none" )
                , ( "font", "inherit" )
                , ( "display", "inline-block" )
                , ( "padding", "0 10px" )
                , ( "background", "#fff" )
                ]
            ]
            [ Html.text title ]
    else
        Html.button
            [ HtmlAttributes.style
                [ ( "border", "1px solid transparent" )
                , ( "border-bottom", "none" )
                , ( "background", "transparent" )
                , ( "display", "inline-block" )
                , ( "color", "#909090" )
                , ( "outline", "none" )
                , ( "font", "inherit" )
                , ( "padding", "0 10px" )
                , ( "cursor", "pointer" )
                ]
            , Events.onClick (ChangeTab tab)
            ]
            [ Html.text title ]


viewLetter : List Dimension -> String -> Html Msg
viewLetter valuesAndScales text =
    let
        fontVariationSettings =
            valuesAndScales
                |> List.map
                    (\{ value, name } ->
                        "'" ++ name ++ "' " ++ toString value
                    )
                |> String.join ", "
    in
        Html.div
            [ HtmlAttributes.style
                [ ( "position", "absolute" )
                , ( "bottom", "0" )
                , ( "left", "0" )
                , ( "max-width", "100%" )
                , ( "max-height", "100%" )
                , ( "box-sizing", "border-box" )
                , ( "padding", "0 50px" )
                , ( "outline", "none" )
                , ( "font-family", "'Voto Serif GX'" )
                , ( "font-size", "200px" )
                , ( "line-height", "1.5" )
                , ( "text-rendering", "optimizeLegibility" )
                , ( "caret-color", "#909090" )
                , ( "font-variation-settings", fontVariationSettings )
                ]
            , Events.onWithOptions "mousedown"
                { stopPropagation = True
                , preventDefault = False
                }
                (Decode.succeed Noop)
            , HtmlAttributes.spellcheck False
            , HtmlAttributes.autocomplete False
            , HtmlAttributes.contenteditable True
            ]
            [ Html.text text ]


viewValueSlider : Dimension -> Html Msg
viewValueSlider { number, color, value, title, min, max, step } =
    Html.div []
        [ Html.label
            [ HtmlAttributes.for title
            , HtmlAttributes.style [ ( "display", "block" ) ]
            ]
            [ Html.text ("Axis " ++ toString (number + 1) ++ " ")
            , Html.em [] [ Html.text title ]
            , Html.text (" = " ++ toString (round value))
            ]
        , Html.input
            [ HtmlAttributes.id title
            , HtmlAttributes.type_ "range"
            , HtmlAttributes.style [ ( "color", color ) ]
            , Events.onInput (String.toFloat >> Result.withDefault 0 >> ChangeValue number)
            , HtmlAttributes.min (toString min)
            , HtmlAttributes.max (toString max)
            , HtmlAttributes.step "1"
            , HtmlAttributes.value (toString value)
            ]
            []
        ]


viewScaleSlider : Dimension -> Html Msg
viewScaleSlider { number, color, distance, title } =
    Html.div []
        [ Html.label
            [ HtmlAttributes.for title
            , HtmlAttributes.style [ ( "display", "block" ) ]
            ]
            [ Html.text ("Axis " ++ toString (number + 1) ++ " ")
            , Html.em [] [ Html.text title ]
            , Html.text (" = " ++ toString (toFloat (round (distance * 1000)) / 1000))
            ]
        , Html.input
            [ HtmlAttributes.id title
            , HtmlAttributes.type_ "range"
            , HtmlAttributes.style [ ( "color", color ) ]
            , Events.onInput (String.toFloat >> Result.withDefault 0 >> ChangeDistance number)
            , HtmlAttributes.min "0"
            , HtmlAttributes.max "50"
            , HtmlAttributes.step "0.1"
            , HtmlAttributes.value (toString distance)
            ]
            []
        ]


viewStyle : String -> Html Msg
viewStyle url =
    Html.node "style"
        []
        [ Html.text ("""
@font-face {
  font-family: 'Voto Serif GX';
  src: url('""" ++ url ++ """') format('truetype');
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
""") ]
