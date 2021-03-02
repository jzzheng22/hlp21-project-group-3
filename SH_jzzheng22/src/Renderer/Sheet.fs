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

type Predicate<'a> = 
    | A of (XYPos -> 'a * XYPos * XYPos)
    | B of (XYPos * XYPos -> 'a * XYPos * XYPos)

let origin = { X = 0.; Y = 0. }

/// Tests if a point is inside a bounding box
let inBoundingBox point box =
    // match box with
    // | _, topL, botR ->
    //     point.X >= topL.X && point.X <= botR.X && point.Y >= topL.Y && point.Y <= botR.Y
    // match box with
    // | _, topL, botR ->
    point.X >= (fst box).X && point.X <= (snd box).X && point.Y >= (fst box).Y && point.Y <= (snd box).Y
/// Takes in two coordinates and a bounding box
/// Tests to see if box is between those two coordinates
let boxInSelectedArea outerBox innerBox = 
    let outerTopL = {X = min (fst outerBox).X (snd outerBox).X; Y = min (fst outerBox).Y (snd outerBox).Y}
    let outerBotR = {X = max (fst outerBox).X (snd outerBox).X; Y = max (fst outerBox).Y (snd outerBox).Y}
    let outerBox = outerTopL, outerBotR
    // match innerBox with
    // | _, topL, botR ->
    //     inBoundingBox topL outerBox && inBoundingBox botR outerBox
    inBoundingBox (fst innerBox) outerBox && inBoundingBox (snd innerBox) outerBox

let getID tuple =
    let id, _, _ = tuple
    id

let getIDList filter lst =
    lst
    |> filter
    |> List.map getID

// let getIDList predicate coords lst =
//     lst
//     |> List.filter (predicate coords)
//     |> List.map getID

let dispatchSelection symbolIDList wireIDList dispatch = 
    dispatch <| SelectComponents symbolIDList
    dispatch <| Symbol(Symbol.Highlight symbolIDList)
    dispatch <| SelectWires wireIDList
    dispatch <| Wire(BusWire.HighlightWires wireIDList)

//TODO: REFACTOR THIS TO USE SINGLE FUNCTION IN ALL CODE
/// Selects elements inside the outer box
// let dragSelectElements (model: Model) (predicate: Predicate<'a>) coords (dispatch: Dispatch<Msg>) =
    /// 
        /// let dragSelectElements (model: Model) predicate coords (dispatch: Dispatch<Msg>) =
    // let symbolIDList =
    //     Symbol.getBoundingBoxes model.Wire.Symbol model.DraggingPos
    //     |> getIDList (List.filter (predicate coords))
    // let wireIDList =
    //     BusWire.getBoundingBoxes model.Wire model.DraggingPos
    //     |> getIDList (List.filter (predicate coords))
    // dispatchSelection symbolIDList wireIDList dispatch
// let dragSelectElements (model: Model) predicate coords (dispatch: Dispatch<Msg>) =
//     let symbolIDList =
//         Symbol.getBoundingBoxes model.Wire.Symbol model.DraggingPos
//         |> getIDList predicate coords
//     let wireIDList =
//         BusWire.getBoundingBoxes model.Wire model.DraggingPos
//         |> getIDList predicate coords
//     dispatchSelection symbolIDList wireIDList dispatch
let filterFunc predicate coords tuple = 
    match tuple with
    | _, a, b -> predicate coords (a, b)

let dragSelectElements (model: Model) predicate coords (dispatch: Dispatch<Msg>) =
    // let outerBoxCoords = (model.DragStartPos, model.DraggingPos)
    let symbolIDList =
        Symbol.getBoundingBoxes model.Wire.Symbol model.DraggingPos
        |> getIDList (List.filter (filterFunc predicate coords))
        // |> getIDList (List.filter (fun (_, a, b) -> boxInSelectedArea outerBoxCoords (a, b)))

    let wireIDList =
        BusWire.getBoundingBoxes model.Wire model.DraggingPos
        // |> getIDList boxInSelectedArea outerBoxCoords
        // |> getIDList (List.filter (fun (_, a, b) -> boxInSelectedArea outerBoxCoords (a, b)))
        |> getIDList (List.filter (filterFunc predicate coords))

    dispatchSelection symbolIDList wireIDList dispatch

// TODO: CHANGE THIS TO USE DRAGGINGPOS INSTEAD? CHECK MESSAGE UPDATES TO SEE IF POSSIBLE
/// Selects elements where mousePos is inside bounding box
let selectElements (model: Model) (mousePos: XYPos) (dispatch: Dispatch<Msg>) =
    dragSelectElements model inBoundingBox mousePos dispatch
    dispatch <| SelectDragStart mousePos

// let selectElements (model: Model) (mousePos: XYPos) (dispatch: Dispatch<Msg>) =
//     let symbolIDList =
//         Symbol.getBoundingBoxes model.Wire.Symbol mousePos
//         // |> getIDList inBoundingBox mousePos
//         // |> getIDList (List.filter (fun (_, a, b) -> inBoundingBox mousePos (a, b)))
//         |> getIDList (List.filter (filterFunc predicate coords))


//     let wireIDList =
//         BusWire.getBoundingBoxes model.Wire mousePos
//         // |> getIDList inBoundingBox mousePos
//         |> getIDList (List.filter (filterFunc predicate coords))

//         // |> getIDList (List.filter (fun (_, a, b) -> inBoundingBox mousePos (a, b)))

//     dispatchSelection symbolIDList wireIDList dispatch
//     dispatch <| SelectDragStart mousePos


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
        let outerBoxCoords = (model.DragStartPos, model.DraggingPos)
        if not (boxInSelectedArea outerBoxCoords (mousePos, mousePos)) then
            selectElements model mousePos dispatch
    dispatch <| SelectDragStart mousePos

let mouseUp model mousePos dispatch = 
    dispatch <| SelectDragging mousePos

    match Symbol.isPort model.Wire.Symbol model.DraggingPos with
    | Some (_, endPoint) ->
        // let startPort =
        match model.SelectedPort with
        | Some a -> 
            dispatch <| Wire(BusWire.AddWire(a, endPoint))
        | None -> () //failwithf "Error: tried to create port connection without starting port"

        
    | None -> ()

    if model.SelectingMultiple then
        let outerBoxCoords = (model.DragStartPos, model.DraggingPos)

        dragSelectElements model boxInSelectedArea outerBoxCoords dispatch
        // dragSelectElements model dispatch


    dispatch <| SelectDragEnd mousePos

let mouseMove model mousePos dispatch mDown = 
    let symbolIDList =
        Symbol.getBoundingBoxes model.Wire.Symbol mousePos
        // |> getIDList inBoundingBox mousePos
        |> getIDList (List.filter (fun (_, a, b) -> inBoundingBox mousePos (a, b)))
    // let symbolIDList =
    //     Symbol.getBoundingBoxes model.Wire.Symbol mousePos
    //     |> List.filter (inBoundingBox mousePos)
    //     |> List.map getID

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
    | MouseMove -> mouseMove model mousePos dispatch (mDown ev)

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
          OnMouseDown (fun ev -> handleMouseOps MouseDown model ev dispatch)
          OnMouseUp (fun ev -> handleMouseOps MouseUp model ev dispatch)
          OnMouseMove (fun ev -> handleMouseOps MouseMove model ev dispatch)
        ] [
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
