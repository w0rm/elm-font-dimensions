port module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onMouseDown, onMouseMove, onMouseUp, onResize)
import Camera3d exposing (Camera3d)
import Circle2d
import Direction2d
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder, Value)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d.Projection as LineSegment3d
import Point2d exposing (Point2d)
import Point3d.Projection as Point3d
import SketchPlane3d exposing (SketchPlane3d)
import Structure exposing (Dimension, LoadedDimension, Structure)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttributes
import Task
import Viewpoint3d exposing (Viewpoint3d)


port loadFont :
    ({ title : String, url : String, dimensions : List LoadedDimension } -> msg)
    -> Sub msg


main : Program Value Model Msg
main =
    Browser.element
        { init = always init
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
    , title : String
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
      , url = "assets/VotoSerifGX.latin1.ttf"
      , title = "VotoSerifGX.latin1.ttf"
      }
    , Task.perform (\{ viewport } -> Resize viewport.width viewport.height) getViewport
    )


type Msg
    = Resize Float Float
    | MouseDown Float Float
    | MouseUp
    | MouseMove Float Float
    | ChangeDimensions Int
    | ChangeValue Int Float
    | ChangeDistance Int Float
    | ChangePerspective Float
    | ChangeTab Tab
    | LoadFont { url : String, dimensions : List LoadedDimension, title : String }
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
                , title = font.title
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

        Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )

        MouseDown x y ->
            ( { model | mouse = Just (Point2d.fromCoordinates ( x, y )) }, Cmd.none )

        MouseUp ->
            ( { model | mouse = Nothing }, Cmd.none )

        MouseMove x y ->
            case model.mouse of
                Just source ->
                    let
                        deltaX =
                            x - Point2d.xCoordinate source

                        deltaY =
                            y - Point2d.yCoordinate source
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
        [ onResize (\w h -> Resize (toFloat w) (toFloat h))
        , onMouseDown (mousePosition MouseDown)
        , model.mouse
            |> Maybe.map (always (onMouseMove (mousePosition MouseMove)))
            |> Maybe.withDefault Sub.none
        , onMouseUp (Decode.succeed MouseUp)
        , loadFont LoadFont
        ]


mousePosition : (Float -> Float -> Msg) -> Decoder Msg
mousePosition coordsToMsg =
    Decode.map2 coordsToMsg
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


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
            Viewpoint3d.lookAt
                { eyePoint = eyePoint
                , focalPoint = model.structure.focalPoint
                , upDirection = upDirection
                }

        camera =
            Camera3d.perspective
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
            SvgAttributes.strokeWidth (String.fromFloat (max 1 (8 - toFloat model.dimensionsCount)))

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
            Svg.circle2d
                [ SvgAttributes.fill color ]
                (point
                    |> point3dToScreenSpace
                    |> Point2d.scaleAbout screenCenter scale2d
                    |> Circle2d.withRadius (10 - toFloat model.dimensionsCount)
                )

        mapValue point =
            Svg.circle2d
                [ SvgAttributes.fill "#fff"
                , SvgAttributes.stroke "#000"
                , SvgAttributes.strokeWidth "2"
                ]
                (point
                    |> point3dToScreenSpace
                    |> Point2d.scaleAbout screenCenter scale2d
                    |> Circle2d.withRadius (10 - toFloat model.dimensionsCount)
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
            Frame2d.withXDirection
                Direction2d.positiveX
                (Point2d.fromCoordinates ( -model.width / 2, -model.height / 2 ))
    in
    Html.div
        [ HtmlAttributes.style "position" "absolute"
        , HtmlAttributes.style "width" "100%"
        , HtmlAttributes.style "height" "100%"
        ]
        [ viewStyle model.url
        , Svg.svg
            [ SvgAttributes.width (String.fromFloat model.width)
            , SvgAttributes.height (String.fromFloat model.height)
            , HtmlAttributes.style "display" "block"
            ]
            (viewStructure model)
        , viewLetter model.dimensions model.text
        , viewInfo
        , Html.div
            [ HtmlAttributes.style "position" "absolute"
            , HtmlAttributes.style "right" "30px"
            , HtmlAttributes.style "width" "200px"
            , HtmlAttributes.style "top" "30px"
            , Events.stopPropagationOn "mousedown" (Decode.succeed ( Noop, True ))
            ]
            [ Html.label
                [ HtmlAttributes.for "file"
                , HtmlAttributes.style "display" "block"
                , HtmlAttributes.style "margin" "0 0 50px"
                ]
                [ Html.text "Font: "
                , Html.em [] [ Html.text model.title ]
                ]
            , Html.label
                [ HtmlAttributes.for "dimensions"
                , HtmlAttributes.style "display" "block"
                ]
                [ Html.text ("Dimensions: " ++ String.fromInt model.dimensionsCount)
                ]
            , Html.input
                [ HtmlAttributes.id "dimensions"
                , HtmlAttributes.type_ "range"
                , HtmlAttributes.style "color" "#909090"
                , Events.onInput (String.toInt >> Maybe.withDefault 0 >> ChangeDimensions)
                , HtmlAttributes.min "0"
                , HtmlAttributes.max (List.length model.dimensions |> String.fromInt)
                , HtmlAttributes.step "1"
                , HtmlAttributes.value (String.fromInt model.dimensionsCount)
                ]
                []
            , Html.label
                [ HtmlAttributes.for "perspective"
                , HtmlAttributes.style "display" "block"
                ]
                [ Html.text "Perspective"
                ]
            , Html.input
                [ HtmlAttributes.id "perspective"
                , HtmlAttributes.type_ "range"
                , HtmlAttributes.style "color" "#909090"
                , Events.onInput (String.toFloat >> Maybe.withDefault 0 >> ChangePerspective)
                , HtmlAttributes.min "0"
                , HtmlAttributes.max "10"
                , HtmlAttributes.step "0.5"
                , HtmlAttributes.value (String.fromFloat model.perspective)
                ]
                []
            , Html.div
                [ HtmlAttributes.style "border-bottom" "1px solid #909090"
                , HtmlAttributes.style "height" "35px"
                , HtmlAttributes.style "line-height" "35px"
                , HtmlAttributes.style "margin" "10px 0"
                , HtmlAttributes.style "display"
                    (if model.dimensionsCount == 0 then
                        "none"

                     else
                        "block"
                    )
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
            [ HtmlAttributes.style "border" "1px solid #909090"
            , HtmlAttributes.style "border-bottom" "none"
            , HtmlAttributes.style "font" "inherit"
            , HtmlAttributes.style "display" "inline-block"
            , HtmlAttributes.style "padding" "0 10px"
            , HtmlAttributes.style "background" "#fff"
            ]
            [ Html.text title ]

    else
        Html.button
            [ HtmlAttributes.style "border" "1px solid transparent"
            , HtmlAttributes.style "border-bottom" "none"
            , HtmlAttributes.style "background" "transparent"
            , HtmlAttributes.style "display" "inline-block"
            , HtmlAttributes.style "color" "#909090"
            , HtmlAttributes.style "outline" "none"
            , HtmlAttributes.style "font" "inherit"
            , HtmlAttributes.style "padding" "0 10px"
            , HtmlAttributes.style "cursor" "pointer"
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
                        "'" ++ name ++ "' " ++ String.fromFloat value
                    )
                |> String.join ", "
    in
    Html.div
        [ HtmlAttributes.style "position" "absolute"
        , HtmlAttributes.style "bottom" "0"
        , HtmlAttributes.style "left" "0"
        , HtmlAttributes.style "max-width" "100%"
        , HtmlAttributes.style "max-height" "100%"
        , HtmlAttributes.style "box-sizing" "border-box"
        , HtmlAttributes.style "padding" "0 50px"
        , HtmlAttributes.style "outline" "none"
        , HtmlAttributes.style "font-family" "'Voto Serif GX'"
        , HtmlAttributes.style "font-size" "200px"
        , HtmlAttributes.style "line-height" "1.5"
        , HtmlAttributes.style "text-rendering" "optimizeLegibility"
        , HtmlAttributes.style "caret-color" "#909090"
        , HtmlAttributes.style "font-variation-settings" fontVariationSettings
        , Events.stopPropagationOn "mousedown" (Decode.succeed ( Noop, True ))
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
            , HtmlAttributes.style "display" "block"
            ]
            [ Html.text ("Axis " ++ String.fromInt (number + 1) ++ " ")
            , Html.em [] [ Html.text title ]
            , Html.text (" = " ++ String.fromInt (round value))
            ]
        , Html.input
            [ HtmlAttributes.id title
            , HtmlAttributes.type_ "range"
            , HtmlAttributes.style "color" color
            , Events.onInput (String.toFloat >> Maybe.withDefault 0 >> ChangeValue number)
            , HtmlAttributes.min (String.fromFloat min)
            , HtmlAttributes.max (String.fromFloat max)
            , HtmlAttributes.step "1"
            , HtmlAttributes.value (String.fromFloat value)
            ]
            []
        ]


viewScaleSlider : Dimension -> Html Msg
viewScaleSlider { number, color, distance, title } =
    Html.div []
        [ Html.label
            [ HtmlAttributes.for title
            , HtmlAttributes.style "display" "block"
            ]
            [ Html.text ("Axis " ++ String.fromInt (number + 1) ++ " ")
            , Html.em [] [ Html.text title ]
            , Html.text (" = " ++ String.fromFloat (toFloat (round (distance * 1000)) / 1000))
            ]
        , Html.input
            [ HtmlAttributes.id title
            , HtmlAttributes.type_ "range"
            , HtmlAttributes.style "color" color
            , Events.onInput (String.toFloat >> Maybe.withDefault 0 >> ChangeDistance number)
            , HtmlAttributes.min "0"
            , HtmlAttributes.max "50"
            , HtmlAttributes.step "0.1"
            , HtmlAttributes.value (String.fromFloat distance)
            ]
            []
        ]


viewInfo : Html Msg
viewInfo =
    Html.div
        [ HtmlAttributes.style "position" "absolute"
        , HtmlAttributes.style "left" "30px"
        , HtmlAttributes.style "top" "20px"
        ]
        [ Html.h1
            [ HtmlAttributes.style "font-size" "120%"
            ]
            [ Html.text "Multidimensional"
            , Html.br [] []
            , Html.text "Axis Visualizer"
            ]
        , Html.p []
            [ Html.text "Concept: Luc(as) de Groot, "
            , Html.br [] []
            , Html.a [ HtmlAttributes.href "http://lucasfonts.com" ] [ Html.text "LucasFonts.com" ]
            ]
        , Html.p []
            [ Html.text "Code: Andrey Kuzmin, "
            , Html.br [] []
            , Html.a
                [ HtmlAttributes.href "https://github.com/w0rm/elm-font-dimensions" ]
                [ Html.text "open source on GitHub" ]
            ]
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

        body,
        html {
            margin: 0;
            padding: 0
        }

        input[type=file] {
            position: absolute;
            right: 30px;
            top: 60px;
            margin: 0;
            width: 200px;
            z-index: 1;
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
