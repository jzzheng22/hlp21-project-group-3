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
    | ZoomCanvasIn 
    | ZoomCanvasOut 

type Msg =
    | Wire of BusWire.Msg
    | Symbol of Symbol.Msg
    | KeyPress of KeyboardMsg
    | SelectPort of CommonTypes.PortId * CommonTypes.PortType
    | SelectComponents of CommonTypes.ComponentId list
    | SelectWires of CommonTypes.ConnectionId list
    | SelectDragStart of XYPos
    | SelectDragging of XYPos
    | SelectDragEnd
    | DispatchMove of XYPos

type MouseOps =
    | MouseDown
    | MouseUp
    | MouseMove


//IMPORTANT CONSTANTS-------------------------------------------------------------------

///The origin (0,0) as an XYPos
let origin = { X = 0.; Y = 0. }

///The length of one side of the square canvas in pixels without zoom.
let UNZOOMEDCANVAS = 1000.0

///Side-length of a grid square (1/100 of canvas).
let UNZOOMEDGRID = UNZOOMEDCANVAS/100.0

//HELPER FUNCTIONS-------------------------------------------------------------------------

/// Tests if a point is inside a bounding box
let inBoundingBox point box =
    point.X >= (fst box).X && point.X <= (snd box).X && point.Y >= (fst box).Y && point.Y <= (snd box).Y

/// Takes in two coordinates and a bounding box
/// Tests to see if box is between those two coordinates
let boxInSelectedArea outerBox innerBox = 
    let outerTopL = {X = min (fst outerBox).X (snd outerBox).X; Y = min (fst outerBox).Y (snd outerBox).Y}
    let outerBotR = {X = max (fst outerBox).X (snd outerBox).X; Y = max (fst outerBox).Y (snd outerBox).Y}
    let outerBox = outerTopL, outerBotR
    inBoundingBox (fst innerBox) outerBox && inBoundingBox (snd innerBox) outerBox

///Snap-to-grid. Takes in a point (top-left coord of bounding box) and returns a 
///translation vector indicating where the point should move to be on a grid intersection point.
let snapgrid (point: XYPos) = 
    let newXSquare = round (point.X/UNZOOMEDGRID)
    let newYSquare = round (point.Y/UNZOOMEDGRID)
    //return translation vector
    {X = (UNZOOMEDGRID*newXSquare - point.X); Y = (UNZOOMEDGRID*newYSquare - point.Y)}

///Finds the top left corner point of the bounding box for a symbol, given the id. Currently searches through all the
///bounding boxes for the symbol ID. Return as option.
let findTopLeft (model: Model) (point: XYPos) (symId: CommonTypes.ComponentId) = 
    Symbol.getBoundingBoxes (model.Wire.Symbol) point 
    //We try to find a box with the symbol id.
    |> List.tryFind (fun (id, tl, br) -> id = symId)
    //Return the XYPos top-left corner as an option
    |> Option.map (fun (id,tl,br) -> tl)

///Obtain first element (will be ID) from a 3-tuple (representation of bounding box).
let getID (id, _, _) = id

let getIDList filter lst =
    lst
    |> filter
    |> List.map getID

let dispatchSelection symbolIDList wireIDList dispatch = 
    dispatch <| SelectComponents symbolIDList
    dispatch <| Symbol(Symbol.Highlight symbolIDList)
    dispatch <| SelectWires wireIDList
    dispatch <| Wire(BusWire.HighlightWires wireIDList)

/// Removes ID from tuple. Used for testing where IDs are not necessary 
let removeID predicate coords (_, a, b) = predicate coords (a, b)

/// Selects elements inside the outer box
let dragSelectElements (model: Model) predicate coords (dispatch: Dispatch<Msg>) =
    let symbolIDList =
        Symbol.getBoundingBoxes model.Wire.Symbol model.DraggingPos
        |> getIDList (List.filter (removeID predicate coords)) 
        //If we are not doing a drag-select then a list with > 1 element implies overlap - we only select last element in the list 
        //as this renders on top of the others.   
        |> if (not model.SelectingMultiple) then List.chunkBySize 1 >> List.last else id 

    let wireIDList =
        BusWire.getBoundingBoxes model.Wire model.DraggingPos
        |> getIDList (List.filter (removeID predicate coords))
        //If we are not doing a drag-select then a list with > 1 element implies overlap - we only select last element in the list 
        //as this renders on top of the others.   
        |> if (not model.SelectingMultiple) then List.chunkBySize 1 >> List.last else id 

    dispatchSelection symbolIDList wireIDList dispatch

/// Selects elements where mousePos is inside bounding box
let selectElements (model: Model) (mousePos: XYPos) (dispatch: Dispatch<Msg>) =
    dragSelectElements model inBoundingBox mousePos dispatch

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

/// This function generates the background grid for the canvas by drawing spaced out lines
let backgroundGrid zoom  = 
    let canvasSize = UNZOOMEDCANVAS*zoom
    let step = UNZOOMEDGRID*zoom

    let vertLineMap n = 
        line [
            SVGAttr.X1 (string n)
            SVGAttr.X2 (string n) 
            SVGAttr.Y1 "0"
            SVGAttr.Y2 (string canvasSize)
            SVGAttr.StrokeWidth "1px"
            SVGAttr.Stroke "gainsboro"
            SVGAttr.FillOpacity 0.1
        ] []

    let horizontalLineMap n = 
        line [
            SVGAttr.X1 "0"
            SVGAttr.X2 (string canvasSize) 
            SVGAttr.Y1 (string n)
            SVGAttr.Y2 (string n)
            SVGAttr.StrokeWidth "1px"
            SVGAttr.Stroke "gainsboro"
            SVGAttr.FillOpacity 0.1
        ] []

    [
        g[] (List.map vertLineMap [0.0..step..canvasSize])
        g[] (List.map horizontalLineMap [0.0..step..canvasSize])
    ]

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

/// This will be set when the canvas is first created and then provide info about how the canvas is scrolled.
let mutable getSvgClientRect: (unit -> Types.ClientRect option) = (fun () -> None) // svgClientRect() will contain the canvas bounding box

let mouseDown model mousePos dispatch = 
    match Symbol.isPort model.Wire.Symbol mousePos with
    | Some (_, portId) -> dispatch <| SelectPort (portId, Symbol.getPortType model.Wire.Symbol portId)
    | None ->
        let componentList = 
            //Check whether we have clicked on an element already selected
            model.SelectedComponents
            |> List.map (Symbol.getBoundingBox model.Wire.Symbol)
            |> List.filter (inBoundingBox mousePos) 
        //If not, select new elements based on the mouse position
        if List.isEmpty componentList then
            selectElements model mousePos dispatch
    dispatch <| SelectDragStart mousePos
    

let mouseUp model mousePos dispatch = 
    match validConnection model with
    | Some endPort ->
        match model.SelectedPort with
        | (Some startPort, _) when Symbol.getPortType model.Wire.Symbol endPort <> snd model.SelectedPort 
            -> dispatch <| Wire(BusWire.AddWire(startPort, endPort))
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
        |> getIDList (List.filter (removeID inBoundingBox mousePos))

    dispatch <| Symbol(Symbol.HighlightPorts symbolIDList)

    if mDown then // Drag
        match model.SelectedPort with
        | (Some _, _) -> ()
        | _ ->
            dispatch <| DispatchMove mousePos

        dispatch <| SelectDragging mousePos


let mDown (ev: Types.MouseEvent) = ev.buttons <> 0.

let handleMouseOps (mouseOp: MouseOps) (model: Model) (ev: Types.MouseEvent) (dispatch: Dispatch<Msg>) =
    let coordX = ev.clientX / model.Zoom
    let coordY = ev.clientY / model.Zoom
    let offsetX, offsetY =
        match getSvgClientRect () with
        | Some a ->
            a.left, a.top
        | None ->
            0., 0.
    let mousePos = { X = coordX - offsetX; Y = coordY - offsetY}
    match mouseOp with
    | MouseDown -> mouseDown model mousePos dispatch
    | MouseUp -> mouseUp model mousePos dispatch
    | MouseMove -> mouseMove model mousePos dispatch (mDown ev)

/// Generates the SVG canvas, adjusted for zoom and scrolling.
let displaySvgWithZoom (model: Model) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =
    let sizeInPixels = sprintf "%.2fpx" ((UNZOOMEDCANVAS * model.Zoom))
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
                        getSvgClientRect <- fun () -> (Some (html.getBoundingClientRect()))) ] [
            g [ Style [ Transform(sprintf "scale(%f)" model.Zoom) ] ] [  // top-level transform style attribute for zoom
              
                g[] (backgroundGrid model.Zoom) //Background grid

                svgReact // the application code
                match model.SelectedPort with
                | (Some _, _) -> drawPortConnectionLine model //Port selection line
                | _ -> ()

                if model.SelectingMultiple then //Drag select box
                    drawSelectionBox model
            ]
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
        |> List.append model.SelectedWires
        |> List.distinct

    BusWire.update (BusWire.DeleteWires wiresToDelete) model.Wire

let deleteSymbols model wModel =
    BusWire.update (BusWire.Symbol(Symbol.Delete model.SelectedComponents)) wModel

///Moves symbols/wires based on what is currently selected.
let moveElements model mousePos =
    let transVector =
        { X = (mousePos.X - model.DraggingPos.X) / model.Zoom
          Y = (mousePos.Y - model.DraggingPos.Y) / model.Zoom }
    let newModel = {model with DraggingPos = mousePos}
    if not (List.isEmpty model.SelectedComponents) then
        let sModel, sCmd = BusWire.update (BusWire.Symbol (Symbol.Move(model.SelectedComponents, transVector))) newModel.Wire
        {model with Wire = sModel}, Cmd.map Wire sCmd

    else if not (List.isEmpty model.SelectedWires) then
        printf "Demo: Send BusWire.MoveWires"
        printf "%A" model.SelectedWires
        printf "%A" transVector
        let wModel, wCmd = BusWire.update (BusWire.MoveWires(model.SelectedWires, transVector)) newModel.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd

    else
        { model with SelectingMultiple = true}, Cmd.none

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
    //Zooming in
    | KeyPress ZoomCanvasIn -> 
        ({model with Zoom = model.Zoom * 1.25}, Cmd.none)
    //Zooming out
    | KeyPress ZoomCanvasOut -> 
        ({model with Zoom = model.Zoom/1.25}, Cmd.none)
    | KeyPress Del ->
        let wModel, wCmd = deleteWires model
        let sModel, sCmd = deleteSymbols model wModel

        { model with
              Wire = sModel
              SelectedPort = None, CommonTypes.PortType.Input
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
    | SelectPort (portID, portType) -> { model with SelectedPort = Some portID, portType }, Cmd.none
    | SelectComponents scMsg ->
        { model with SelectedComponents = scMsg }, Cmd.none
    | SelectWires swMsg -> { model with SelectedWires = swMsg }, Cmd.none
    | SelectDragStart dragMsg ->
        { model with
              DragStartPos = dragMsg;
              DraggingPos = dragMsg },
        Cmd.none
    | SelectDragEnd ->
        //Find translation vector for snap-to-grid on selected components if any.
        let transVector = 
            //origin implies no translation necessary
            if List.isEmpty model.SelectedComponents then origin
            else  
                let pointToShift = 
                        //Find the top-left corner of bounding box of first symbol in the model list.
                        let pointOption = findTopLeft model origin (List.head (model.SelectedComponents))
                        match pointOption with
                        | Some a -> a 
                        | None -> failwithf "ERROR: Symbol ID is not logged in the model and cannot be searched."
                
                snapgrid pointToShift

        let interimModel = { model with
                                SelectedPort = None, CommonTypes.PortType.Input;
                                SelectingMultiple = false }
        //Move symbols if movement is required
        if transVector <> origin then 
           let sModel, sCmd = BusWire.update (BusWire.Symbol (Symbol.Move(model.SelectedComponents, transVector))) model.Wire 
           {interimModel with Wire = sModel}, Cmd.map Wire sCmd
        //else reset model
        else 
            interimModel, Cmd.none
    | SelectDragging dragMsg ->
        { model with DraggingPos = dragMsg }, Cmd.none
    | DispatchMove mousePos ->
        moveElements model mousePos

let init () =
    let model, cmds = (BusWire.init 400) ()

    { Wire = model
      Zoom = 1.0
      SelectedPort = None, CommonTypes.PortType.Input
      SelectedComponents = []
      SelectedWires = []
      SelectingMultiple = false
      DragStartPos = origin
      DraggingPos = origin },
    Cmd.map Wire cmds
