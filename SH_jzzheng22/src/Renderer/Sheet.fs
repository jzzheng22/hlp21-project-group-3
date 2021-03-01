module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type Model = {
    Wire: BusWire.Model
    Zoom: float 
    SelectedPorts: CommonTypes.PortId list
    SelectedComponents: CommonTypes.ComponentId list
    SelectedWires: CommonTypes.ConnectionId list
    DragStartPos: XYPos
    // DragEndPos: XYPos
    DraggingPos: bool * XYPos
    MousePos: XYPos
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | Del

type Msg =
    | Wire of BusWire.Msg
    | Symbol of Symbol.Msg
    | KeyPress of KeyboardMsg
    | SelectPort of (XYPos * CommonTypes.PortId)
    | SelectComponents of (XYPos * CommonTypes.ComponentId list)
    | SelectWires of (XYPos * CommonTypes.ConnectionId list)
    | SelectDragStart of XYPos
    | SelectDragEnd of (bool * XYPos)
    | SelectDragging of (bool * XYPos)
    | MouseMove of XYPos

let origin = {X = 0.; Y = 0.}

let inBoundingBox point box =
    match box with
    | _, topL, botR ->
        let leftX = topL.X
        let rightX = botR.X
        let topY = topL.Y
        let botY = botR.Y
        // printf "%A" (point.X >= leftX && point.X <= rightX && point.Y >= botY && point.Y <= topY)
        point.X >= leftX && point.X <= rightX && point.Y <= botY && point.Y >= topY

let getID tuple =
    let id, _, _ = tuple
    id

let selectElements (model: Model) (mousePos: XYPos) (dispatch: Dispatch<Msg>) =
    printf "%A" "SELECT ELEMENTS"
    printf "%A" (Symbol.getBoundingBoxes model.Wire.Symbol mousePos)
    let symbolIDList = 
        Symbol.getBoundingBoxes model.Wire.Symbol mousePos
        |> List.filter (inBoundingBox mousePos)
        |> List.map getID
    let wireIDList = 
        BusWire.getBoundingBoxes model.Wire mousePos
        |> List.filter (inBoundingBox mousePos)
        |> List.map getID
    printf "%A" symbolIDList
    // if not (List.isEmpty symbolIDList) then
    dispatch <| SelectComponents (mousePos, symbolIDList)
    dispatch <| Symbol (Symbol.Highlight symbolIDList)


    // if not (List.isEmpty wireIDList) then
    dispatch <| SelectWires (mousePos, wireIDList)
    // else
    dispatch <| SelectDragStart mousePos

let selectDragging (model: Model) (mousePos: XYPos) (dispatch: Dispatch<Msg>) =
    failwithf "Not implemented"

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (model: Model) (svgReact: ReactElement) (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" ((1000. * model.Zoom))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = ev.buttons <> 0.
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev:Types.MouseEvent) = 
        dispatch <| Wire (BusWire.MouseMsg {Op = op ; Pos = { X = ev.clientX / model.Zoom ; Y = ev.clientY / model.Zoom}})
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ] 
          OnMouseDown (fun ev -> 
            let coordX = ev.clientX / model.Zoom
            let coordY = ev.clientY / model.Zoom
            let mousePos = {X = coordX; Y = coordY}
            match Symbol.isPort model.Wire.Symbol mousePos with
            | Some (portCoords, portId) -> dispatch <| SelectPort (portCoords, portId)
            | None -> selectElements model mousePos dispatch
          )
            // (mouseOp Down ev))
          OnMouseUp (fun ev -> 
            let coordX = ev.clientX / model.Zoom
            let coordY = ev.clientY / model.Zoom
            let mousePos = {X = coordX; Y = coordY}
            dispatch <| SelectDragEnd (false, mousePos)
          )
            // (mouseOp Up ev))
          OnMouseMove (fun ev -> 
            let coordX = ev.clientX / model.Zoom
            let coordY = ev.clientY / model.Zoom
            let mousePos = {X = coordX; Y = coordY}
            let symbolIDList = 
                Symbol.getBoundingBoxes model.Wire.Symbol mousePos
                |> List.filter (inBoundingBox mousePos)
                |> List.map getID
            // if not (List.isEmpty symbolIDList) then
            dispatch <| Symbol (Symbol.HighlightPorts symbolIDList)
            if mDown ev then // Drag
                dispatch <| SelectDragging (true, mousePos)
            )
        ]
        [ svg
            [ Style 
                [
                    Border "3px solid green"
                    Height sizeInPixels
                    Width sizeInPixels           
                ]
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" model.Zoom)]] // top-level transform style attribute for zoom
                [ 
                    text [ // a demo text svg element
                        X 500; 
                        Y 50; 
                        Style [
                            TextAnchor "middle" // horizontal algnment vs (X,Y)
                            DominantBaseline "middle" // vertical alignment vs (X,Y)
                            FontSize "40px"
                            FontWeight "Bold"
                            Fill "Green" // font color
                        ]
                    ] [str "sample text"]

                    svgReact // the application code


                ]
            ]
        ]



/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) =
    printf "%A" model.Wire.Symbol.[6]
    printf "%A" "Selected Components:"
    printf "%A" model.SelectedComponents
    // dispatch <| Symbol (Symbol.Highlight model.SelectedComponents)
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom model wireSvg dispatch
       

let update (msg : Msg) (model : Model): Model * Cmd<Msg> =
    match msg with
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    | Symbol sMsg ->
        let wModel, wCmd = BusWire.update (BusWire.Symbol sMsg) model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey
        printfn "Key:%A" c
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)
    | SelectPort spMsg -> 
        let _, portID = spMsg
        {model with SelectedPorts = [portID]}, Cmd.none
    | SelectComponents scMsg -> 
        let _, componentIDList = scMsg
        {model with SelectedComponents = componentIDList}, Cmd.none
    | SelectWires swMsg -> 
        let _, wireIDList = swMsg
        {model with SelectedWires = wireIDList}, Cmd.none
    | SelectDragStart dragMsg -> 
        {model with DragStartPos = dragMsg}, Cmd.none
    | SelectDragEnd dragMsg ->
        {model with DraggingPos = dragMsg}, Cmd.none
    | SelectDragging dragMsg ->
        {model with DraggingPos = dragMsg}, Cmd.none
    | MouseMove moveMsg ->
        {model with MousePos = moveMsg}, Cmd.none




let init() = 
    let model,cmds = (BusWire.init 400)()
    {
        Wire = model
        Zoom = 1.0
        SelectedPorts = []
        SelectedComponents = []
        SelectedWires = []
        DragStartPos = origin
        // DragEndPos = origin
        DraggingPos = false, origin
        MousePos = origin
    }, Cmd.map Wire cmds
    

