module Sheet

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type Model =
    { Wire: BusWire.Model
      Zoom: float
      SelectedPort: CommonTypes.PortId option
      SelectedComponents: CommonTypes.ComponentId list
      SelectedWires: CommonTypes.ConnectionId list
      SelectingMultiple: bool
      DragStartPos: XYPos
      DraggingPos: XYPos }

type KeyboardMsg =
    | CtrlS
    | AltC
    | AltV
    | AltZ
    | AltShiftZ
    | Del
    | AltA

type Msg =
    | Wire of BusWire.Msg
    | Symbol of Symbol.Msg
    | KeyPress of KeyboardMsg
    | SelectPort of CommonTypes.PortId
    | SelectComponents of CommonTypes.ComponentId list
    | SelectWires of CommonTypes.ConnectionId list
    | SelectMultiple of XYPos
    | SelectDragStart of XYPos
    | SelectDragEnd of XYPos
    | SelectDragging of XYPos

type MouseOps =
    | MouseDown
    | MouseUp
    | MouseMove

let origin = { X = 0.; Y = 0. }

let inBoundingBox point box =
    match box with
    | _, topL, botR ->
        let leftX = topL.X
        let rightX = botR.X
        let topY = topL.Y
        let botY = botR.Y

        point.X >= leftX
        && point.X <= rightX
        && point.Y >= topY
        && point.Y <= botY

let getID tuple =
    let id, _, _ = tuple
    id

let getIDList predicate mousePos lst =
    lst
    |> List.filter (predicate mousePos)
    |> List.map getID


// TODO: CHANGE THIS TO USE DRAGGINGPOS INSTEAD? CHECK MESSAGE UPDATES TO SEE IF POSSIBLE
let selectElements (model: Model) (mousePos: XYPos) (dispatch: Dispatch<Msg>) =
    let symbolIDList =
        Symbol.getBoundingBoxes model.Wire.Symbol mousePos
        |> getIDList inBoundingBox mousePos

    let wireIDList =
        BusWire.getBoundingBoxes model.Wire mousePos
        |> getIDList inBoundingBox mousePos

    dispatch <| SelectComponents symbolIDList
    dispatch <| Symbol(Symbol.Highlight symbolIDList)
    dispatch <| SelectWires wireIDList
    dispatch <| Wire(BusWire.HighlightWires wireIDList)
    dispatch <| SelectDragStart mousePos

//CHANGE TO CALL INBOUNDINGBOX FOR EACH CORNER
let boxInSelectedArea startPos endPos box =
    let minX = min startPos.X endPos.X
    let minY = min startPos.Y endPos.Y
    let maxX = max startPos.X endPos.X
    let maxY = max startPos.Y endPos.Y

    match box with
    | _, topL, botR ->
        let leftX = topL.X
        let rightX = botR.X
        let topY = topL.Y
        let botY = botR.Y

        minX <= leftX
        && maxX >= rightX
        && minY <= topY
        && maxY >= botY


//TODO: REFACTOR THIS TO USE SINGLE FUNCTION IN ALL CODE
let dragSelectElements (model: Model) (dispatch: Dispatch<Msg>) =
    let symbolIDList =
        Symbol.getBoundingBoxes model.Wire.Symbol model.DraggingPos
        |> List.filter (boxInSelectedArea model.DragStartPos model.DraggingPos)
        |> List.map getID

    let wireIDList =
        BusWire.getBoundingBoxes model.Wire model.DraggingPos
        |> List.filter (boxInSelectedArea model.DragStartPos model.DraggingPos)
        |> List.map getID

    dispatch <| SelectComponents symbolIDList
    dispatch <| Symbol(Symbol.Highlight symbolIDList)
    dispatch <| SelectWires wireIDList
    dispatch <| Wire(BusWire.HighlightWires wireIDList)

let cornersToString startCoord endCoord =
    sprintf
        "%f,%f %f,%f %f,%f %f,%f"
        startCoord.X startCoord.Y
        endCoord.X startCoord.Y
        endCoord.X endCoord.Y
        startCoord.X endCoord.Y

let drawSelectionBox model =
    polygon [ SVGAttr.Points(cornersToString model.DragStartPos model.DraggingPos)
              SVGAttr.StrokeWidth "1px"
              SVGAttr.Stroke "lightblue"
              SVGAttr.StrokeDasharray "5,5"
              SVGAttr.FillOpacity 0.1
              SVGAttr.Fill "grey" ] []

let drawPortConnectionLine model =
    line [ X1 model.DragStartPos.X
           Y1 model.DragStartPos.Y
           X2 model.DraggingPos.X
           Y2 model.DraggingPos.Y
           Style [ match Symbol.isPort model.Wire.Symbol model.DraggingPos with
                   | Some _ ->
                       Stroke "darkblue"
                       StrokeWidth "5px"
                       StrokeDasharray "10,10"
                   | None ->
                       Stroke "green"
                       StrokeWidth "1px"
                       StrokeDasharray "5,5"
                   FillOpacity 0.1 ] ] []

let mouseDown model mousePos dispatch = 
    match Symbol.isPort model.Wire.Symbol mousePos with
    | Some (_, portId) -> dispatch <| SelectPort portId
    | None ->
        if boxInSelectedArea model.DragStartPos model.DraggingPos ((), mousePos, mousePos) then
            ()
        else
            selectElements model mousePos dispatch

    dispatch <| SelectDragStart mousePos

let mouseUp model mousePos dispatch = 
    dispatch <| SelectDragging mousePos

    match Symbol.isPort model.Wire.Symbol model.DraggingPos with
    | Some (_, endPoint) ->
        let startPort =
            match model.SelectedPort with
            | Some a -> a
            | None -> failwithf "Error: tried to create port connection without starting port"

        dispatch <| Wire(BusWire.AddWire(startPort, endPoint))
    | None -> ()

    if model.SelectingMultiple then
        dragSelectElements model dispatch

    dispatch <| SelectDragEnd mousePos

let mouseMove model mousePos dispatch mDown = 
    let symbolIDList =
        Symbol.getBoundingBoxes model.Wire.Symbol mousePos
        |> List.filter (inBoundingBox mousePos)
        |> List.map getID

    dispatch <| Symbol(Symbol.HighlightPorts symbolIDList)

    if mDown then // Drag
        dispatch <| SelectDragging mousePos

        match model.SelectedPort with
        | Some _ -> ()
        | None ->
            let transVector =
                { X = mousePos.X - model.DraggingPos.X
                  Y = mousePos.Y - model.DraggingPos.Y }

            if not (List.isEmpty model.SelectedComponents) then
                dispatch <| Symbol(Symbol.Move(model.SelectedComponents, transVector))
            else if not (List.isEmpty model.SelectedWires) then
                dispatch <| Wire(BusWire.MoveWires(model.SelectedWires, transVector))
            else
                dispatch <| SelectMultiple mousePos

let mDown (ev: Types.MouseEvent) = ev.buttons <> 0.

let handleMouseOps (mouseOp: MouseOps) (model: Model) (ev: Types.MouseEvent) (dispatch: Dispatch<Msg>) =
    let coordX = ev.clientX / model.Zoom
    let coordY = ev.clientY / model.Zoom
    let mousePos = { X = coordX; Y = coordY }
    match mouseOp with
    | MouseDown -> mouseDown model mousePos dispatch
    | MouseUp -> mouseUp model mousePos dispatch
    | MouseMove -> 
        mouseMove model mousePos dispatch (mDown ev)

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (model: Model) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =
    let sizeInPixels = sprintf "%.2fpx" ((1000. * model.Zoom))
    /// Is the mouse button currently down?

    div [ Style [ Height "100vh"
                  MaxWidth "100vw"
                  CSSProp.OverflowX OverflowOptions.Auto
                  CSSProp.OverflowY OverflowOptions.Auto ]
          OnMouseDown
              (fun ev ->
                  handleMouseOps MouseDown model ev dispatch)

          OnMouseUp
              (fun ev ->
                  handleMouseOps MouseUp model ev dispatch)
          OnMouseMove
              (fun ev ->
                  handleMouseOps MouseMove model ev dispatch)] [

        svg [ Style [ Border "3px solid green"
                      Height sizeInPixels
                      Width sizeInPixels ] ] [
            g [ Style [ Transform(sprintf "scale(%f)" model.Zoom) ] ] [  // top-level transform style attribute for zoom
                svgReact // the application code
                match model.SelectedPort with
                | Some _ -> drawPortConnectionLine model
                | None -> ()

                if model.SelectingMultiple then
                    drawSelectionBox model
            ]
        ] // top-level transform style attribute for zoom
    ] // top-level transform style attribute for zoom


let view (model: Model) (dispatch: Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom model wireSvg dispatch


let deleteWires model =
    let connectedWires =
        model.SelectedComponents
        |> List.map (Symbol.getPortIds model.Wire.Symbol)
        |> List.collect (BusWire.getWireIdsFromPortIds model.Wire)

    let wiresToDelete =
        connectedWires
        |> List.append model.SelectedWires
        |> List.distinct

    BusWire.update (BusWire.DeleteWires wiresToDelete) model.Wire

let deleteSymbols model wModel =
    BusWire.update (BusWire.Symbol(Symbol.Delete model.SelectedComponents)) wModel


let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Wire wMsg ->
        let wModel, wCmd = BusWire.update wMsg model.Wire
        { model with Wire = wModel }, Cmd.map Wire wCmd
    | Symbol sMsg ->
        let sModel, sCmd = BusWire.update (BusWire.Symbol sMsg) model.Wire
        { model with Wire = sModel }, Cmd.map Wire sCmd
    | KeyPress AltShiftZ ->
        printStats () // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress Del ->
        let wModel, wCmd = deleteWires model
        let sModel, sCmd = deleteSymbols model wModel

        { model with
              Wire = sModel
              SelectedPort = None
              SelectedComponents = []
              SelectedWires = [] },
        Cmd.batch [ Cmd.map Wire wCmd;
                    Cmd.map Wire sCmd ]
    | KeyPress AltA ->
        let sModel, sCmd =
            BusWire.update
                (BusWire.Symbol(Symbol.Add(CommonTypes.ComponentType.Mux2, model.DraggingPos, 2, 1)))
                model.Wire

        { model with Wire = sModel }, Cmd.map Wire sCmd
    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey

        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)
    | SelectPort spMsg -> { model with SelectedPort = Some spMsg }, Cmd.none
    | SelectComponents scMsg ->
        { model with SelectedComponents = scMsg }, Cmd.none
    | SelectWires swMsg -> { model with SelectedWires = swMsg }, Cmd.none
    | SelectMultiple multMsg ->
        { model with
              DraggingPos = multMsg;
              SelectingMultiple = true },
        Cmd.none
    | SelectDragStart dragMsg ->
        { model with
              DragStartPos = dragMsg;
              DraggingPos = dragMsg },
        Cmd.none
    | SelectDragEnd dragMsg ->
        { model with
              SelectedPort = None;
              SelectingMultiple = false },
        Cmd.none
    | SelectDragging dragMsg -> { model with DraggingPos = dragMsg }, Cmd.none


let init () =
    let model, cmds = (BusWire.init 400) ()

    { Wire = model
      Zoom = 1.0
      SelectedPort = None
      SelectedComponents = []
      SelectedWires = []
      SelectingMultiple = false
      DragStartPos = origin
      DraggingPos = origin },
    Cmd.map Wire cmds
