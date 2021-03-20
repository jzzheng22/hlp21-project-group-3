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
      SelectedPort: CommonTypes.PortId option * CommonTypes.PortType
      SelectedComponents: CommonTypes.ComponentId list
      SelectedWireSegments: (CommonTypes.ConnectionId * int) list
      SelectingMultiple: bool
      EditSizeOf: CommonTypes.ComponentId option
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
    | ZoomCanvasIn 
    | ZoomCanvasOut
    | SymbolClockwise
    | SymbolAntiClock
    | SymbolMagnify
    | SymbolShrink 

type Msg =
    | Wire of BusWire.Msg
    | Symbol of Symbol.Msg
    | KeyPress of KeyboardMsg
    | SelectPort of CommonTypes.PortId * CommonTypes.PortType
    | SelectComponents of CommonTypes.ComponentId list
    | SelectWireSegments of (CommonTypes.ConnectionId * int) list
    | SelectDragStart of XYPos
    | SelectDragging of XYPos
    | SelectDragEnd
    | DispatchMove of XYPos
    | BeginSizeEdit of CommonTypes.ComponentId
    | EditSize of (CommonTypes.ComponentId * XYPos)
    | ErrorMsg of string

type MouseOps =
    | MouseDown
    | MouseUp
    | MouseMove


let origin = { X = 0.; Y = 0. }

/// The length of one side of the square canvas in pixels without zoom.
let unzoomedCanvas = 1000.0
let unzoomedGrid = unzoomedCanvas / 100.0

/// Tests if a point is inside a bounding box
let inBoundingBox point box =
    point.X >= (fst box).X && point.X <= (snd box).X && point.Y >= (fst box).Y && point.Y <= (snd box).Y

/// Tests if a point is within a specified threshold distance from any corner of a bounding box (assuming zoom = 1.0). 
let pointNearBoxCorner point box =
    let threshold = 5.0
    let xClose p1 p2 = (abs (p1.X - p2.X)) < threshold
    let yClose p1 p2 = (abs (p1.Y - p2.Y)) < threshold
    ( (xClose point (fst box)) || (xClose point (snd box)) ) && ( (yClose point (fst box)) || (yClose point (snd box)) )

/// Takes in two coordinates and a bounding box
/// Tests to see if box is between those two coordinates
let boxInSelectedArea outerBox innerBox = 
    let outerTopL = {X = min (fst outerBox).X (snd outerBox).X; Y = min (fst outerBox).Y (snd outerBox).Y}
    let outerBotR = {X = max (fst outerBox).X (snd outerBox).X; Y = max (fst outerBox).Y (snd outerBox).Y}
    let outerBox = outerTopL, outerBotR
    inBoundingBox (fst innerBox) outerBox && inBoundingBox (snd innerBox) outerBox

/// Takes in a point and returns translation vector 
/// required to move to nearest square
let snapGridVector (point: XYPos) = 
    let newXSquare = round (point.X / unzoomedGrid)
    let newYSquare = round (point.Y / unzoomedGrid)
    {X = (unzoomedGrid * newXSquare - point.X); Y = (unzoomedGrid * newYSquare - point.Y)}

let getSymbolID (id, _, _) = id

let getWireSegmentID (id, index, _, _) = (id, index)

let getSymbolIDList filter lst =
    lst
    |> filter
    |> List.map getSymbolID

let getWireSegmentIDList filter lst =
    lst
    |> filter
    |> List.map getWireSegmentID

let dispatchSelection symbolIDList wireSegmentIDList dispatch = 
    dispatch <| SelectComponents symbolIDList
    dispatch <| Symbol(Symbol.Highlight symbolIDList)
    dispatch <| SelectWireSegments wireSegmentIDList
    dispatch <| Wire(BusWire.HighlightWires (List.map fst wireSegmentIDList))

/// Removes ID from Symbol tuple. Used for testing where IDs are not necessary 
let removeSymbolID predicate coords (_, a, b) = predicate coords (a, b)
/// Remove wire ID from tuple.
let removeWireSegmentID predicate coords (_, _, a, b) = predicate coords (a, b)

let filterBySelectingMultiple predicate lst =
    if not predicate then
        List.chunkBySize 1 lst
        |> List.last
    else
        id lst

/// Selects elements inside the outer box
let dragSelectElements (model: Model) predicate coords (dispatch: Dispatch<Msg>) =
    let symbolIDList =
        Symbol.getBoundingBoxes model.Wire.Symbol model.DraggingPos
        |> getSymbolIDList (List.filter (removeSymbolID predicate coords)) 
        |> filterBySelectingMultiple model.SelectingMultiple

    let wireSegmentIDList =
        BusWire.getBoundingBoxes model.Wire model.DraggingPos
        |> getWireSegmentIDList (List.filter (removeWireSegmentID predicate coords))
        |> filterBySelectingMultiple model.SelectingMultiple

    dispatchSelection symbolIDList wireSegmentIDList dispatch

/// Selects elements where mousePos is inside bounding box
let selectElements (model: Model) (mousePos: XYPos) (dispatch: Dispatch<Msg>) =
    dragSelectElements model inBoundingBox mousePos dispatch

let nearCorner model point = 
    let nearSymbols = 
        Symbol.getBoundingBoxes model.Wire.Symbol point 
        |> List.filter (removeSymbolID pointNearBoxCorner point)
    
    if List.isEmpty nearSymbols then 
        None 
    else 
        Some (List.last nearSymbols)

let validConnection model =
    match Symbol.isPort model.Wire.Symbol model.DraggingPos with
    | Some (_, portID) when Symbol.getPortType model.Wire.Symbol portID <> snd model.SelectedPort ->
        Some portID
    | _ -> None

let cornersToString startCoord endCoord =
    sprintf
        "%f,%f %f,%f %f,%f %f,%f"
        startCoord.X startCoord.Y
        endCoord.X startCoord.Y
        endCoord.X endCoord.Y
        startCoord.X endCoord.Y

///Converts the model into an Issie canvas state = (components, connections), and feeds this into buswidthinferer
let inferWidth (model : Model) = 
    let comps = Symbol.extractComponents model.Wire.Symbol
    let conns = BusWire.extractWires model.Wire 
    let canvas = (comps, conns)
    BusWidthInferer.inferConnectionsWidth canvas

/// Checks that the widths are the same for connections that could not be inferred from width inferer
let checkWidth (model : Model) (connect : CommonTypes.ConnectionId) =
    let (port1, port2) = (BusWire.connectToPort model.Wire connect)
    let (w1, w2) = (Symbol.getPortWidth model.Wire.Symbol port1, Symbol.getPortWidth model.Wire.Symbol port2)
    if w1 = w2 then w1 else -1

/// Sends a highlight error to buswire and symbol for the connection given
let dispatchError (model : Model) (connect : CommonTypes.ConnectionId) dispatch =
    dispatch <| BusWire.HighlightError [connect]
    dispatch <| BusWire.Symbol(Symbol.HighlightError (BusWire.connectToSym model.Wire connect))

let updateWidth (model : Model) dispatch =
    inferWidth model
    |> function
    | Ok x -> //this is a Map<ConnectionId, int Option>
        x 
        |> Map.toList 
        |> List.map (
            fun (k, v) -> 
            match v with 
            | Some a -> 
                dispatch <| BusWire.UpdateWidth (k, a)
            | None -> 
                let w = checkWidth model k
                if w = -1 then dispatchError model k
                else dispatch <| BusWire.UpdateWidth (k, w))
    | Error e -> //this is a {Message : string; Connections : ConnectionId list}
        e.ConnectionsAffected
        |> List.map (dispatchError model)
        //dispatch <| displayErrorMessage e.Msg


/// This function generates the background grid for the canvas by drawing spaced out lines
let backgroundGrid zoom  = 
    let canvasSize = unzoomedCanvas * zoom
    let step = unzoomedGrid * zoom

    let vertLineMap n = 
        line [
            SVGAttr.X1 (string n)
            SVGAttr.X2 (string n) 
            SVGAttr.Y1 "0"
            SVGAttr.Y2 (string canvasSize)
            SVGAttr.StrokeWidth "0.75px"
            SVGAttr.Stroke "gainsboro"
            SVGAttr.FillOpacity 0.1
        ] []

    let horizontalLineMap n = 
        line [
            SVGAttr.X1 "0"
            SVGAttr.X2 (string canvasSize) 
            SVGAttr.Y1 (string n)
            SVGAttr.Y2 (string n)
            SVGAttr.StrokeWidth "0.75px"
            SVGAttr.Stroke "gainsboro"
            SVGAttr.FillOpacity 0.1
        ] []

    [
        g[] (List.map vertLineMap [0. .. step .. canvasSize])
        g[] (List.map horizontalLineMap [0. .. step .. canvasSize])
    ]

let multiplyPosByZoom pos zoom = {pos with X = pos.X * zoom; Y = pos.Y * zoom}

let multiplyValByZoom value zoom = value * zoom

let drawSelectionBox model =
    polygon [ SVGAttr.Points(cornersToString (multiplyPosByZoom model.DragStartPos model.Zoom) (multiplyPosByZoom model.DraggingPos model.Zoom))
              SVGAttr.StrokeWidth "1px"
              SVGAttr.Stroke "lightblue"
              SVGAttr.StrokeDasharray "5,5"
              SVGAttr.FillOpacity 0.1
              SVGAttr.Fill "grey" ] []

let drawPortConnectionLine model =
    line [ X1 (multiplyValByZoom model.DragStartPos.X model.Zoom)
           Y1 (multiplyValByZoom model.DragStartPos.Y model.Zoom)
           X2 (multiplyValByZoom model.DraggingPos.X model.Zoom)
           Y2 (multiplyValByZoom model.DraggingPos.Y model.Zoom)
           Style [ match validConnection model with
                   | Some _ ->
                       Stroke "darkblue"
                       StrokeWidth "5px"
                       StrokeDasharray "10,10"
                   | None ->
                       Stroke "green"
                       StrokeWidth "1px"
                       StrokeDasharray "5,5"
                   FillOpacity 0.1 ] ] []

let highlightCorners model box = 
    let circleGen centre =  
        circle [
            Cx (centre.X)
            Cy (centre.Y)
            R 5
            Style [
                Stroke "black"
                Fill "red"
            ]
        ] []

    g[][
        circleGen (multiplyPosByZoom (fst box) model.Zoom)
        circleGen (multiplyPosByZoom (snd box) model.Zoom)
        circleGen (multiplyPosByZoom {X = (fst box).X; Y = (snd box).Y} model.Zoom)
        circleGen (multiplyPosByZoom {X = (snd box).X; Y = (fst box).Y} model.Zoom)
    ]

/// This will be set when the canvas is first created and then provide info about how the canvas is scrolled.
let mutable getSvgClientRect: (unit -> Types.ClientRect option) = (fun () -> None) // svgClientRect() will contain the canvas bounding box

let mouseDown model mousePos dispatch = 
    match Symbol.isPort model.Wire.Symbol mousePos with
    | Some (_, portId) -> 
        dispatch <| SelectPort (portId, Symbol.getPortType model.Wire.Symbol portId)
    | None ->
        match nearCorner model mousePos with 
        | Some (id, _, _) -> 
            dispatch <| BeginSizeEdit id 
        | None ->  
            let overlappingComponentList = 
                model.SelectedComponents
                |> List.map (Symbol.getBoundingBox model.Wire.Symbol)
                |> List.filter (inBoundingBox mousePos) 
            if List.isEmpty overlappingComponentList then
                selectElements model mousePos dispatch
    dispatch <| SelectDragStart mousePos
    

let mouseUp model mousePos dispatch = 
    match validConnection model with
    | Some endPort ->
        match model.SelectedPort with
        | (Some startPort, _) when Symbol.getPortType model.Wire.Symbol endPort <> snd model.SelectedPort 
        /// --------------------------------------- ///
        /// Here buswidth inferer should be called? ///
        /// --------------------------------------- ///
            -> dispatch <| Wire(BusWire.AddWire(startPort, endPort))
               updateWidth model
        | _ -> ()
    | None -> ()
    if model.SelectingMultiple then
        let outerBoxCoords = (model.DragStartPos, model.DraggingPos)
        dragSelectElements model boxInSelectedArea outerBoxCoords dispatch
    dispatch <| SelectDragEnd

let increaseBoundingBox (a, topL, botR) = 
    a, {X = topL.X - 10.; Y = topL.Y - 10.}, {X = botR.X + 10.; Y = botR.Y + 10.}

let mouseMove model mousePos dispatch mDown = 
    let symbolIDList =
        Symbol.getBoundingBoxes model.Wire.Symbol mousePos
        |> List.map increaseBoundingBox
        |> getSymbolIDList (List.filter (removeSymbolID inBoundingBox mousePos))

    dispatch <| Symbol(Symbol.HighlightPorts symbolIDList)

    if mDown then // Drag
        match model.SelectedPort with
        | (Some _, _) -> ()
        | _ ->
            match model.EditSizeOf with 
            | Some id -> 
                dispatch <| EditSize (id, mousePos)
            | _ -> 
                dispatch <| DispatchMove mousePos

        dispatch <| SelectDragging mousePos


let mDown (ev: Types.MouseEvent) = ev.buttons <> 0.

let handleMouseOps (mouseOp: MouseOps) (model: Model) (ev: Types.MouseEvent) (dispatch: Dispatch<Msg>) =
    let coordX = ev.clientX 
    let coordY = ev.clientY 
    let offsetX, offsetY =
        match getSvgClientRect () with
        | Some a ->
            a.left, a.top
        | None ->
            0., 0.
    let mousePos = { X = (coordX - offsetX) / model.Zoom; Y = (coordY - offsetY) / model.Zoom }
    match mouseOp with
    | MouseDown -> mouseDown model mousePos dispatch
    | MouseUp -> mouseUp model mousePos dispatch
    | MouseMove -> mouseMove model mousePos dispatch (mDown ev)

let displaySvgWithZoom (model: Model) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =
    let sizeInPixels = sprintf "%.2fpx" ((unzoomedCanvas * model.Zoom))
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
                      Width sizeInPixels ] 
                      
              Ref (fun html -> 
                        getSvgClientRect <- fun () -> (Some (html.getBoundingClientRect()))) ] 
            [
                g [] (backgroundGrid model.Zoom) //Background grid
                g [ Style [ Transform(sprintf "scale(%f)" model.Zoom) ] ] [  // top-level transform style attribute for zoom
                    svgReact
                  ]
                g [] [
                        match model.SelectedPort with
                        | (Some _, _) -> drawPortConnectionLine model
                        | _ -> 
                            match model.EditSizeOf with 
                            | Some id -> highlightCorners model (Symbol.getBoundingBox model.Wire.Symbol id)
                            | _ ->
                                if model.SelectingMultiple then
                                    drawSelectionBox model]
        ] 
    ] 


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
        |> List.append (List.map fst model.SelectedWireSegments)
        |> List.distinct

    BusWire.update (BusWire.DeleteWires wiresToDelete) model.Wire

let deleteSymbols model wModel =
    BusWire.update (BusWire.Symbol(Symbol.Delete model.SelectedComponents)) wModel

let moveElements model mousePos =
    let transVector =
        { X = (mousePos.X - model.DraggingPos.X)
          Y = (mousePos.Y - model.DraggingPos.Y) }
    let newModel = {model with DraggingPos = mousePos}
    if not (List.isEmpty model.SelectedComponents) then
        let sModel, sCmd = BusWire.update (BusWire.Symbol (Symbol.Move(model.SelectedComponents, transVector))) newModel.Wire
        {model with Wire = sModel}, Cmd.map Wire sCmd

    else if not (List.isEmpty model.SelectedWireSegments) then
        let wireSegment = List.head model.SelectedWireSegments
        let wModel, wCmd = BusWire.update (BusWire.MoveWires(fst wireSegment, snd wireSegment, transVector)) newModel.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd

    else
        { model with SelectingMultiple = true}, Cmd.none


let changeSymbolSize model id mousePos =
    let topL, botR = Symbol.getBoundingBox model.Wire.Symbol id
    let mouseXOffset = max (abs (mousePos.X - topL.X)) (abs (mousePos.X - botR.X))
    let mouseYOffset = max (abs (mousePos.Y - topL.Y)) (abs (mousePos.Y  - botR.Y))

    let sizeToGrid = snapGridVector {X = mouseXOffset; Y = mouseYOffset}

    let snapX = mouseXOffset + sizeToGrid.X 
    let snapY = mouseYOffset + sizeToGrid.Y

    let currentBoxWidth = abs (topL.X - botR.X)
    let currentBoxHeight = abs (topL.Y - botR.Y)

    let scaleMsg = id, {X = snapX / currentBoxWidth; Y = snapY / currentBoxHeight}
    let sModel, sCmd = BusWire.update (BusWire.Symbol (Symbol.Scale scaleMsg)) model.Wire
    {model with Wire = sModel}, Cmd.map Wire sCmd


let snapSymbolToGrid model =
    let transVector =
        Symbol.getBoundingBox model.Wire.Symbol (List.head model.SelectedComponents)
        |> function
        | (topL, _) ->
            snapGridVector topL
    let sModel, sCmd = BusWire.update (BusWire.Symbol (Symbol.Move(model.SelectedComponents, transVector))) model.Wire 
    { model with 
        Wire = sModel;
        SelectedPort = None, CommonTypes.PortType.Input;
        SelectingMultiple = false }, Cmd.map Wire sCmd

let snapWireSegmentToGrid model =
    // A wire segment can only be dragged and snapped horizontally or vertically
    let transVector =
        if abs (model.DraggingPos.X - model.DragStartPos.X) > abs (model.DraggingPos.Y - model.DragStartPos.Y) then 
            snapGridVector {X = model.DraggingPos.X; Y = 0.0}
        else 
            snapGridVector {X = 0.0; Y = model.DraggingPos.Y}
    let wireSegment = List.head model.SelectedWireSegments
    let wModel, wCmd = BusWire.update (BusWire.MoveWires(fst wireSegment, snd wireSegment, transVector)) model.Wire 
    { model with 
        Wire = wModel;
        SelectedPort = None, CommonTypes.PortType.Input;
        SelectingMultiple = false }, Cmd.map Wire wCmd

let topleftCorners model =
    model.SelectedComponents
    |> List.map (Symbol.getBoundingBox model.Wire.Symbol >> fst)

let foldFunction (model: Model, cmd: Cmd<Msg>) (id: CommonTypes.ComponentId, vector: XYPos) = 
    let sModel, sCmd = BusWire.update(BusWire.Symbol(Symbol.Move ([id], vector))) model.Wire
    {model with Wire = sModel}, Cmd.batch [ Cmd.map Wire sCmd; cmd ]

let alignComponents model vector =
    vector
    |> List.zip model.SelectedComponents
    |> List.fold foldFunction (model, Cmd.none)
    
/// Finds the highest index for a Symbol type and increments that number
let getNewSymbolIndex (model : Model) (compType : CommonTypes.ComponentType) : int = 
    let symbolList = 
        model.Wire.Symbol 
        |> List.filter (fun x -> x.Type = compType)

    if List.isEmpty symbolList then 1 
    else symbolList
        |> List.map (fun x -> x.Index)
        |> List.max
        |> (+) 1

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
    | KeyPress SymbolClockwise | KeyPress SymbolAntiClock | KeyPress SymbolMagnify | KeyPress SymbolShrink -> 
        if List.isEmpty model.SelectedComponents then 
            model, Cmd.none
        else 
            let symid = List.head model.SelectedComponents
            let sModel, sCmd = 
                match msg with 
                | KeyPress SymbolClockwise -> 
                    BusWire.update(BusWire.Symbol(Symbol.Rotate (symid, 90))) model.Wire
                | KeyPress SymbolAntiClock ->
                    BusWire.update(BusWire.Symbol(Symbol.Rotate (symid, 270))) model.Wire
                | KeyPress SymbolMagnify ->
                    BusWire.update(BusWire.Symbol(Symbol.Scale (symid, {X = 1.25; Y = 1.25}))) model.Wire
                | KeyPress SymbolShrink -> 
                    BusWire.update(BusWire.Symbol(Symbol.Scale (symid, {X = 0.8; Y = 0.8}))) model.Wire
                | _ -> 
                    failwithf "Unexpected input in symbol transformation."
            { model with Wire = sModel }, Cmd.map Wire sCmd
    | KeyPress ZoomCanvasIn -> 
        ({model with Zoom = model.Zoom * 1.25}, Cmd.none)
    | KeyPress ZoomCanvasOut -> 
        ({model with Zoom = model.Zoom/1.25}, Cmd.none)
    | KeyPress Del ->
        let wModel, wCmd = deleteWires model
        let sModel, sCmd = deleteSymbols model wModel

        { model with
              Wire = sModel
              SelectedPort = None, CommonTypes.PortType.Input
              SelectedComponents = []
              SelectedWireSegments = [] },
        Cmd.batch [ Cmd.map Wire wCmd;
                    Cmd.map Wire sCmd ]
    | KeyPress AltA ->
        let sModel, sCmd =
            BusWire.update
                (BusWire.Symbol(Symbol.Add(CommonTypes.ComponentType.Mux2, model.DraggingPos, 2, 1, (getNewSymbolIndex model CommonTypes.ComponentType.Mux2))))
                model.Wire

        { model with Wire = sModel }, Cmd.map Wire sCmd 
    /// Align along x-axis
    | KeyPress AltC ->
        let leftMost =  List.minBy (fun a -> a.X) (topleftCorners model)
        List.map (fun a ->
            if a.X <> leftMost.X then 
                {X = 0.0; Y = leftMost.Y - a.Y}
            else 
                {X = 0.0; Y = 0.0}) (topleftCorners model)
        |> alignComponents model
    | KeyPress AltV ->
        let downMost = List.maxBy (fun a -> a.Y) (topleftCorners model)
        List.map (fun a ->
            if a.Y <> downMost.Y then 
                {X = downMost.X - a.X; Y = 0.0}
            else 
                {X = 0.0; Y = 0.0}) (topleftCorners model)
        |> alignComponents model

    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey

        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)
    | SelectPort (portID, portType) -> { model with SelectedPort = Some portID, portType }, Cmd.none
    | SelectComponents scMsg ->
        { model with SelectedComponents = scMsg }, Cmd.none
    | SelectWireSegments swMsg -> { model with SelectedWireSegments = swMsg }, Cmd.none
    | SelectDragStart dragMsg ->
        { model with
              DragStartPos = dragMsg;
              DraggingPos = dragMsg },
        Cmd.none
    | SelectDragEnd ->
        if not (List.isEmpty model.SelectedComponents) then
            snapSymbolToGrid model
        elif not (List.isEmpty model.SelectedWireSegments) then
                snapWireSegmentToGrid model
        else
            { model with 
                SelectedPort = None, CommonTypes.PortType.Input;
                SelectingMultiple = false 
                EditSizeOf = None}, Cmd.none
    | SelectDragging dragMsg ->
        { model with DraggingPos = dragMsg }, Cmd.none
    | DispatchMove mousePos ->
        moveElements model mousePos
    | BeginSizeEdit id -> 
        {model with EditSizeOf = Some id}, Cmd.none
    | EditSize (id, mousePos) -> 
        changeSymbolSize model id mousePos

let init () =
    let model, cmds = (BusWire.init 400) ()

    { Wire = model
      Zoom = 1.0
      SelectedPort = None, CommonTypes.PortType.Input
      SelectedComponents = []
      SelectedWireSegments = []
      SelectingMultiple = false
      EditSizeOf = None
      DragStartPos = origin
      DraggingPos = origin },
    Cmd.map Wire cmds
