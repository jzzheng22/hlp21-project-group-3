﻿module Sheet

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
      AddingSymbol: bool
      DragStartPos: XYPos
      DraggingPos: XYPos }

type KeyboardMsg =
    | AltX
    | AltY
    | AltShiftZ
    | Del
    | SymbolAddBegin
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
    | MoveElements of XYPos
    | BeginSizeEdit of CommonTypes.ComponentId
    | EditSize of (CommonTypes.ComponentId * XYPos)
    | ErrorMsg of string
    | UpdateWidths 
    | SymbolAddFinish

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

let dispatchSelection symbolIDList wireSegmentIDList (dispatch: Dispatch<Msg>) = 
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

/// Converts the model into an Issie canvas state = (components, connections), and feeds this into buswidthinferer
let inferWidth (model : Model) = 
    let comps = Symbol.extractComponents model.Wire.Symbol
    printf"hello\n inferWidth comps:\n %A" comps
    let conns = BusWire.extractWires model.Wire 
    printf"hello\n inferWidth conns:\n %A" conns
    let canvas = (comps, conns)
    BusWidthInferer.inferConnectionsWidth canvas

let getWires (model : Model) =
    inferWidth model
    |> function
    | Ok x -> //this is a Map<ConnectionId, int Option>
        let conList = Map.toList x
        let conns = 
            conList 
            |> List.map (fun (x, _) -> x) 
            |> BusWire.getWires model.Wire
        printf "hello UpdateWidth %A" conList
        let wModel, wCmd = BusWire.update (BusWire.UpdateWidth conList) model.Wire
        (conns, wModel, wCmd)
    | Error e -> //this is a {Msg : string; ConnectionsAffected : ConnectionId list}
        let wires = e.ConnectionsAffected
        printf "hello UpdateWidth Error \n Message: %s \n Connections: \n %A" e.Msg wires
        let wModel, wCmd = BusWire.update (BusWire.HighlightError wires) model.Wire
        (wires, wModel, wCmd)

/// Sends a highlight error to buswire and symbol for the connection list given
let dispatchError (model : Model) (wires : CommonTypes.ConnectionId list)  =
    let wModel, wCmd = BusWire.update (BusWire.HighlightError wires) model.Wire
    let sModel, sCmd = BusWire.update (BusWire.Symbol (Symbol.HighlightError (List.collect (fun wire -> BusWire.connectedSymbols model.Wire wire) wires))) wModel
    {model with Wire = sModel}, Cmd.batch [ Cmd.map Wire wCmd; Cmd.map Wire sCmd ]

let drawError (msg : string) : ReactElement list = 
    let drawBox = 
        rect[
            X 0
            Y 0
            SVGAttr.Height 30.
            SVGAttr.Width (String.length msg * 7)
            SVGAttr.Fill "red"
            SVGAttr.Stroke "red"
            SVGAttr.Opacity 0.5
            SVGAttr.StrokeWidth 1.][]

    let drawText = 
        text[
            X 0
            Y 0
            Style[
                TextAnchor "middle"
                DominantBaseline "middle"
                FontSize 6.
                FontWeight "bold"
                Fill "Black"
                UserSelect UserSelectOptions.None]][str <| msg] //Prevent highlighting text
        
    [drawText; drawBox]

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
    if model.AddingSymbol then
        dispatch <| SymbolAddFinish
    else
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
            -> dispatch <| Wire(BusWire.AddWire(startPort, endPort))
               dispatch <| UpdateWidths
        | _ -> ()
    | None -> ()
    if model.SelectingMultiple then
        let outerBoxCoords = (model.DragStartPos, model.DraggingPos)
        dragSelectElements model boxInSelectedArea outerBoxCoords dispatch
    dispatch <| SelectDragEnd

let increaseBoundingBox (a, topL, botR) = 
    a, {X = topL.X - 10.; Y = topL.Y - 10.}, {X = botR.X + 10.; Y = botR.Y + 10.}

let mouseMove model mousePos dispatch mDown = 
    if model.AddingSymbol then 
        dispatch <| MoveElements mousePos
        dispatch <| SelectDragging mousePos
    else 
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
                    dispatch <| MoveElements mousePos

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

/// Scales a symbol according to the mouse position.
let changeSymbolSize model id mousePos =
    let topL, botR = Symbol.getBoundingBox model.Wire.Symbol id
    let mouseXOffset = max (abs (mousePos.X - topL.X)) (abs (mousePos.X - botR.X))
    let mouseYOffset = max (abs (mousePos.Y - topL.Y)) (abs (mousePos.Y  - botR.Y))

    let sizeToGrid = snapGridVector {X = mouseXOffset; Y = mouseYOffset}

    let snapX = mouseXOffset + sizeToGrid.X 
    let snapY = mouseYOffset + sizeToGrid.Y

    let currentBoxWidth = abs (topL.X - botR.X)
    let currentBoxHeight = abs (topL.Y - botR.Y)

    let condition (boxWidth: float) (adjustment: float) = boxWidth <= unzoomedGrid && adjustment < boxWidth
    if ((condition currentBoxWidth snapX) || (condition currentBoxHeight snapY)) then
        model, Cmd.none
    else 
        let scaleMsg = [id], {X = snapX / currentBoxWidth; Y = snapY / currentBoxHeight}
        let sModel, sCmd = BusWire.update (BusWire.Symbol (Symbol.Scale scaleMsg)) model.Wire
        {model with Wire = sModel}, Cmd.map Wire sCmd

/// Moves a symbol so that it snaps to grid.
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
    
/// Deletes selected symbols and wires and updates the widthInferrer.
let deleteElements model = 
    let wModel1, wCmd1 = deleteWires model
    let sModel1, sCmd1 = deleteSymbols model wModel1

    let deleteModel = 
        { model with
              Wire = sModel1
              SelectedPort = None, CommonTypes.PortType.Input
              SelectedComponents = []
              SelectedWireSegments = [] 
        }

    let (wires, newModel, wCmd2) = getWires deleteModel
    let sModel2, sCmd2 = BusWire.update (BusWire.Symbol (Symbol.HighlightError (List.collect (fun wire -> BusWire.connectedSymbols deleteModel.Wire wire) wires))) newModel
    {deleteModel with Wire = sModel2}, Cmd.batch [Cmd.map Wire wCmd1; Cmd.map Wire sCmd1; Cmd.map Wire wCmd2; Cmd.map Wire sCmd2]

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

let addSymbol model =
    let sModel, sCmd =
        BusWire.update
            (BusWire.Symbol(Symbol.Add(CommonTypes.ComponentType.Mux2, model.DraggingPos, 2, 1, (getNewSymbolIndex model CommonTypes.ComponentType.Mux2))))
            model.Wire
    { model with 
        Wire = sModel;
        SelectedComponents = [(List.head sModel.Symbol).Id];
        AddingSymbol = true;
        SelectedPort = None, CommonTypes.PortType.Input;
        SelectingMultiple = false; 
        EditSizeOf = None}, Cmd.map Wire sCmd 

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
            let symid = model.SelectedComponents
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
            let snapModel, snapCmd = snapSymbolToGrid {model with Wire = sModel}
            snapModel, Cmd.batch [ Cmd.map Wire sCmd; snapCmd]
    | KeyPress ZoomCanvasIn -> 
        ({model with Zoom = model.Zoom * 1.25}, Cmd.none)
    | KeyPress ZoomCanvasOut -> 
        ({model with Zoom = model.Zoom/1.25}, Cmd.none)
    | KeyPress Del ->
        deleteElements model

    | KeyPress SymbolAddBegin ->
        addSymbol model
    /// Align along x-axis
    | KeyPress AltX ->
        let leftMost =  List.minBy (fun a -> a.X) (topleftCorners model)
        List.map (fun a ->
            if a.X <> leftMost.X then 
                {X = 0.0; Y = leftMost.Y - a.Y}
            else 
                {X = 0.0; Y = 0.0}) (topleftCorners model)
        |> alignComponents model
    /// Align along y-axis
    | KeyPress AltY ->
        let downMost = List.maxBy (fun a -> a.Y) (topleftCorners model)
        List.map (fun a ->
            if a.Y <> downMost.Y then 
                {X = downMost.X - a.X; Y = 0.0}
            else 
                {X = 0.0; Y = 0.0}) (topleftCorners model)
        |> alignComponents model

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
        else
            match model.EditSizeOf with 
            | Some id ->
                let previousComponents = model.SelectedComponents
                let snapModel, cmd = snapSymbolToGrid {model with SelectedComponents = [id]}
                { snapModel with 
                    SelectedComponents = previousComponents
                    EditSizeOf = None}, cmd
            | None -> 
                { model with 
                    SelectedPort = None, CommonTypes.PortType.Input;
                    SelectingMultiple = false 
                    EditSizeOf = None}, Cmd.none
    | SelectDragging dragMsg ->
        { model with DraggingPos = dragMsg }, Cmd.none
    | MoveElements mousePos ->
        moveElements model mousePos
    | BeginSizeEdit id -> 
        {model with EditSizeOf = Some id}, Cmd.none
    | EditSize (id, mousePos) -> 
        changeSymbolSize model id mousePos
    | UpdateWidths -> 
        let wires, newModel, wCmd = getWires model
        let sModel, sCmd = BusWire.update (BusWire.Symbol (Symbol.HighlightError (List.collect (fun wire -> BusWire.connectedSymbols model.Wire wire) wires))) newModel
        {model with Wire = sModel}, Cmd.batch [Cmd.map Wire wCmd; Cmd.map Wire sCmd]
    | ErrorMsg msg -> model, Cmd.none
    | SymbolAddFinish ->
        let snapModel, cmd = snapSymbolToGrid model
        {snapModel with SelectedComponents = []; AddingSymbol = false}, cmd

let init () =
    let model, cmds = (BusWire.init 400) ()

    { Wire = model
      Zoom = 1.0
      SelectedPort = None, CommonTypes.PortType.Input
      SelectedComponents = []
      SelectedWireSegments = []
      SelectingMultiple = false
      EditSizeOf = None
      AddingSymbol = false
      DragStartPos = origin
      DraggingPos = origin },
    Cmd.map Wire cmds
