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

let origin = { X = 0.; Y = 0. }

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

    let wireIDList =
        BusWire.getBoundingBoxes model.Wire model.DraggingPos
        |> getIDList (List.filter (removeID predicate coords))

    dispatchSelection symbolIDList wireIDList dispatch

/// Selects elements where mousePos is inside bounding box
let selectElements (model: Model) (mousePos: XYPos) (dispatch: Dispatch<Msg>) =
    dragSelectElements model inBoundingBox mousePos dispatch
    dispatch <| SelectDragStart mousePos


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
                   | Some (_, portID) when Symbol.getPortType model.Wire.Symbol portID <> snd model.SelectedPort ->
                       Stroke "darkblue"
                       StrokeWidth "5px"
                       StrokeDasharray "10,10"
                   | _ ->
                       Stroke "green"
                       StrokeWidth "1px"
                       StrokeDasharray "5,5"
                   FillOpacity 0.1 ] ] []

/// this will be set when the canvas is first created and then provide info about how the canvas is scrolled.
let mutable getSvgClientRect: (unit -> Types.ClientRect option) = (fun () -> None) // svgClientRect() will contain the canvas bounding box

let mouseDown model mousePos dispatch = 
    dispatch <| SelectDragStart mousePos
    match Symbol.isPort model.Wire.Symbol mousePos with
    | Some (_, portId) -> dispatch <| SelectPort (portId, Symbol.getPortType model.Wire.Symbol portId)
    | None ->
        let outerBoxCoords = (model.DragStartPos, model.DraggingPos)
        if not (boxInSelectedArea outerBoxCoords (mousePos, mousePos)) then
            selectElements model mousePos dispatch
    printf "mousedown: %A" mousePos

let mouseUp model mousePos dispatch = 
    // dispatch <| SelectDragging mousePos
    match Symbol.isPort model.Wire.Symbol model.DraggingPos with
    | Some (_, endPort) ->
        match model.SelectedPort with
        | (Some startPort, _) when Symbol.getPortType model.Wire.Symbol endPort <> snd model.SelectedPort 
            -> dispatch <| Wire(BusWire.AddWire(startPort, endPort))
        | _ -> ()
    | None -> ()

    if model.SelectingMultiple then
        let outerBoxCoords = (model.DragStartPos, model.DraggingPos)
        dragSelectElements model boxInSelectedArea outerBoxCoords dispatch
    printf "mouseup: %A" mousePos
    dispatch <| SelectDragEnd

let increaseBoundingBox (a, topL, botR) = 
    a, {X = topL.X - 10.; Y = topL.Y - 10.}, {X = botR.X + 10.; Y = botR.Y + 10.}

let mouseMove model mousePos dispatch mDown = 
    let symbolIDList =
        Symbol.getBoundingBoxes model.Wire.Symbol mousePos
        |> List.map increaseBoundingBox
        |> getIDList (List.filter (removeID inBoundingBox mousePos))

    dispatch <| Symbol(Symbol.HighlightPorts symbolIDList)
    printf "inMouseMove: %A" model.DraggingPos

    if mDown then // Drag
        // dispatch <| SelectDragging mousePos
        match model.SelectedPort with
        | (Some _, _) -> ()
        | _ ->
            dispatch <| DispatchMove mousePos
        printf "%A" model.Wire.Symbol.[8]

        dispatch <| SelectDragging mousePos

// let scrollOffset (ev: Types.Window) = {X = ev.pageXOffset; Y = ev.pageYOffset}

let mDown (ev: Types.MouseEvent) = ev.buttons <> 0.

let handleMouseOps (mouseOp: MouseOps) (model: Model) (ev: Types.MouseEvent) (dispatch: Dispatch<Msg>) =
    let coordX = ev.clientX / model.Zoom
    let coordY = ev.clientY / model.Zoom
    // printf "%A" (ev.pageX / model.Zoom)
    // printf "%A" (ev.pageY / model.Zoom)
    printf "%A" (ev.clientX / model.Zoom)
    printf "%A" (ev.clientY / model.Zoom)
    // printf "%A" (ev.offsetX / model.Zoom)
    // printf "%A" (ev.offsetY / model.Zoom)

    let offsetX, offsetY =
        match getSvgClientRect () with
        | Some a ->
            a.left, a.top
        | None ->
            0., 0.
    // let mousePos = { X = coordX; Y = coordY}
    printf "%A %A" offsetX offsetY
    // let offsetX = ev.offsetX
    let mousePos = { X = coordX - offsetX; Y = coordY - offsetY}
    printf "%A" mousePos
    match mouseOp with
    | MouseDown -> mouseDown model mousePos dispatch
    | MouseUp -> mouseUp model mousePos dispatch
    | MouseMove -> mouseMove model mousePos dispatch (mDown ev)

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (model: Model) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =
    let sizeInPixels = sprintf "%.2fpx" ((1000. * model.Zoom))
    div [ Style [ Height "100vh"
                  MaxWidth "100vw"
                  CSSProp.OverflowX OverflowOptions.Auto
                  CSSProp.OverflowY OverflowOptions.Auto ]
        //   let scrollEvent (ev: Types.UIEvent) =
        //     let element = ref None
        //     match !element  with
        //     | Some e ->
        //         printf "%A" e.scrollWidth
        //         printf "%A" e.clientWidth
        //         printf "%A" e.scrollLeft
            // let scrollOffset = 
            // printf "%A" ev.pageX
            // printf "%A" ev.pageY
            // printf "%A" ev.scrollWidth
            // dispatch <| Scroll scrollOffset
        //   OnScroll (fun ev -> scrollEvent ev)
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
                svgReact // the application code
                match model.SelectedPort with
                | (Some _, _) -> drawPortConnectionLine model
                | _ -> ()

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

let moveElements model mousePos =
    let transVector =
        { X = (mousePos.X - model.DraggingPos.X) / model.Zoom
          Y = (mousePos.Y - model.DraggingPos.Y) / model.Zoom }
    let newModel = {model with DraggingPos = mousePos}

    if not (List.isEmpty model.SelectedComponents) then
        let sModel, sCmd = BusWire.update (BusWire.Symbol (Symbol.Move(model.SelectedComponents, transVector))) newModel.Wire
        {model with Wire = sModel}, Cmd.map Wire sCmd

    else if not (List.isEmpty model.SelectedWires) then
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
        { model with
              SelectedPort = None, CommonTypes.PortType.Input;
              SelectingMultiple = false },
        Cmd.none
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
