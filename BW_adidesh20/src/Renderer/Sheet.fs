module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

//FIRST DEFINE THE NECESSARY TYPES FOR MESSAGES AND MODEL:

//Discriminated union to indicate the context of a mouse click.
type Selection = 
    | Port 
    | MultiBox 
    | ComponentSymbol
    | ComponentWire
    | Nothing

//The model of the MVU.
type Model = {
    Wire: BusWire.Model
    SelectBoxStart: XYPos
    SelectBoxCurrent: XYPos
    SelectionType: Selection
    SelectedComponentIDs: CommonTypes.ComponentId list
    SelectedWireIDs: CommonTypes.ConnectionId list
    SelectedPort: CommonTypes.PortId option
    Zoom: float
    }

//Keyboard messages (see Renderer.fs)
type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL | ZoomCanvasIn | ZoomCanvasOut
    
//The message type
type Msg =
    | WireMsg of BusWire.Msg
    | KeyPress of KeyboardMsg
    | SelectSymbols of (XYPos * CommonTypes.ComponentId list)
    | SelectWires of (XYPos * CommonTypes.ConnectionId list) 
    | SelectPort of (XYPos * CommonTypes.PortId)
    | SelectMulti of XYPos 
    | SelectGrow of XYPos
    | SelectEnd of XYPos
    | UpdateMouse of XYPos


//NEXT DEFINE CONSTANTS THAT ARE USED IN THE CODE: 

//Define the origin point for future reference
let origin = {X = 0.0; Y = 0.0}



//DEFINE HELPER FUNCTIONS:

//We work with data structures of the form (ComponentID, topleft corner, bottomright corner) when representing bounding boxes.
//This obtains the component ID
let obtainID (data: (string * XYPos * XYPos)) = 
    let id, topLeft, bottomRight = data 
    id    

//This obtains the top left and bottom right corners as a tuple
let obtainCorners (data: (string * XYPos * XYPos)) =
    let id, topLeft, bottomRight = data 
    (topLeft, bottomRight)

//Bool function that determines whether a point is within a bounding box. The bounding box is a tuple of 
//(topleft corner, bottomright corner)
let inBox (point: XYPos) (box: (XYPos * XYPos)) =
    //Obtain top-left and bottom-right coordinates of bounding box
    let topLeft, bottomRight = box 
    //See if the point is inside or not
    match point with 
    | a when a.X < topLeft.X  ->
        false
    | a when a.X > bottomRight.X ->
        false 
    | a when a.Y < topLeft.Y ->
        false 
    | a when a.Y > bottomRight.Y ->
        false
    | _ -> true

//This allows us to take in the data format (3-tuple) specified above and work with it to check if a point is 
//inside the specified bounding box.
let pointInBoundingBox (point: XYPos) (box: (string * XYPos * XYPos)) = 
    let corners = obtainCorners box 
    inBox point corners

//Higher level - a function that takes in a list of bounding boxes and a point and returns a list of component IDs
//for those boxes which contain the point 
let filterBoxes (point: XYPos) (boxList: (string * XYPos * XYPos) list)  =
    boxList
    |> List.filter (pointInBoundingBox point)
    |> List.map obtainID


//Bool function to check if one box (id: string * top-left corner: XYPos * bottom-right corner: XYPos) is fully inside another box
//(corner: XYPos * corner: XYPos). We assume that we don't know which of the corners are top-left and bottom-right for the encloser box.
let boxEncloses (encloser: (XYPos * XYPos)) (enclosed: (string * XYPos * XYPos)) =
    
    //Figure out topleft and bottomright corners of encloser and create a tuple (topleft: XYPos, botright: XYPos)
    let corner1, corner2 = encloser 
    let topleft = {X = (min corner1.X corner2.X) ; Y = (min corner1.Y corner2.Y)}
    let botright = {X = (max corner1.X corner2.X) ; Y = (max corner1.Y corner2.Y)}
    let enclosingBox = (topleft, botright)
    //Check if corner points of enclosed box are inside the enclosingBox 
    let id, a, b = enclosed
    (inBox a enclosingBox) && (inBox b enclosingBox) 
    

//Function to check if two lists have elements in common 
let hasCommon ls1 ls2 = 
    Set.isEmpty (Set.intersect (Set.ofList ls1) (Set.ofList ls2)) |> not   


    
//Functions to generate string of corner points of rectangle

//For a single pair of x-y coordinates - can be generally used anywhere
let XYPostoString (p:XYPos) = 
    string (p.X) + "," + string (p.Y)

//Putting the coordinates together for the rectangle
let rectCornerString (topLeft: XYPos) (botRight: XYPos) =
    let width = botRight.X - topLeft.X 
    let height = botRight.Y - topLeft.Y 
    //We have different scenarios depending on whether height and width are positive or negative
    match (width, height) with
    | (w,h) when w > 0.0 && h > 0.0 ->
        XYPostoString topLeft + " " + XYPostoString {topLeft with X = topLeft.X + width} + " " + XYPostoString botRight + " " + XYPostoString {botRight with X = botRight.X - width}
    | (w,h) when w > 0.0 && h < 0.0 ->
        XYPostoString {botRight with X = botRight.X - width} + " " + XYPostoString botRight + " " + XYPostoString {topLeft with X = topLeft.X + width} + " " + XYPostoString topLeft
    | (w,h) when w < 0.0 && h > 0.0 ->
        XYPostoString {topLeft with X = topLeft.X + width} + " " + XYPostoString topLeft + " " + XYPostoString {botRight with X = botRight.X - width} + " " + XYPostoString botRight
    | _ ->
        XYPostoString botRight + " " + XYPostoString {botRight with X = botRight.X - width} + " " + XYPostoString topLeft + " " + XYPostoString {topLeft with X = topLeft.X + width}




//Returns the relevant graphic that forms when you drag mouse 
let selectGraphic model = 
    //This condition avoids unnecessary processing when the mouse is down without being dragged.
    if model.SelectBoxStart <> model.SelectBoxCurrent then
        match model.SelectionType with
        //Green selection box that forms when we don't start from a component/port.
        | MultiBox ->
            Some (polygon [
                                SVGAttr.Points (rectCornerString model.SelectBoxStart model.SelectBoxCurrent)
                                SVGAttr.StrokeWidth "1px"
                                SVGAttr.Stroke "Black"
                                SVGAttr.FillOpacity 0.1
                                SVGAttr.Fill "Green"
                            ] [])
        //Dashed line that forms when we start from a port.
        | Port ->
            let dashwidth = 5.0
            Some (line [
                                SVGAttr.StrokeDasharray (string dashwidth + ", " + string dashwidth)
                                SVGAttr.X1 (string (model.SelectBoxStart.X))
                                SVGAttr.X2 (string (model.SelectBoxCurrent.X))
                                SVGAttr.Y1 (string (model.SelectBoxStart.Y))
                                SVGAttr.Y2 (string (model.SelectBoxCurrent.Y))
                                SVGAttr.StrokeWidth "3px"
                                SVGAttr.Stroke "Gray"
                                SVGAttr.FillOpacity 0.1
                            ] [])
        //No graphic necessary for the other selects.
        | _ -> 
            None
    else 
        None



// This function generates the background grid for the canvas by drawing spaced out lines
let backgroundGrid zoom  = 
    let canvasSize = 1000.0*zoom
    let step = 10.0*zoom

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



 /// this will be set when the canvas is first created and then provide info about how the canvas is scrolled.
let mutable getSvgClientRect: (unit -> Types.ClientRect option) = (fun () -> None)

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (model:Model) (svgReact: ReactElement) (selectGraphic: ReactElement option) (dispatch: Dispatch<Msg>)=
    
    let zoom = model.Zoom

    let sizeInPixels = sprintf "%.2fpx" (1000. * zoom)
    
    let rightscroll, downscroll = 
        match getSvgClientRect () with 
        | Some a ->
            a.right, a.bottom 
        | None -> 
            0.0, 0.0 

    //Finding the scroll position
    let rightScrollOffset = 1000.0*zoom - rightscroll
    let downScrollOffset = 1000.0*zoom - downscroll

    printfn "Scrolls: %A, %A" rightScrollOffset downScrollOffset

    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        ev.buttons <> 0.

     //Obtain the grid in the background as a reactElement list
    let grid = backgroundGrid zoom

    let selectlist =
        match selectGraphic with 
        | Some a -> [a]
        | None -> []

    //Define variables to scale with zoom
    let rotation=0 

    //The react element we return
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ] 

          //Based on different mouse events, update the model with coordinates scaled for zoom
          OnMouseDown ( fun ev ->
                            let mouseX = ev.pageX 
                            let mouseY = ev.pageY 
                            //mousePos contains the mouse coordinates adjusted for scrolling
                            let mousePos = {X = mouseX + rightScrollOffset ; Y = mouseY + downScrollOffset}

                            //CASE 1: Click at Port
                            match (Symbol.isPort (model.Wire.Symbol) mousePos) with 
                            | Some (a: (XYPos * CommonTypes.PortId)) -> 
                                let portcoord, portid = a
                                SelectPort (mousePos, portid) |> dispatch
                            | None -> 
                                //CASE 2: Mouse Click within a symbol 
                                //Get symbol bounding boxes and filter based on whether they contain the click point
                                let filteredSymbolList = Symbol.getBoundingBoxes (model.Wire.Symbol) mousePos
                                                         //Convert IDs to strings to use in generic helper functions
                                                         |> List.map (fun (a,b,c) -> (string a, b, c))
                                                         |> filterBoxes mousePos
                                                         //Convert back to component IDs when done. 
                                                         |> List.map CommonTypes.ComponentId
                                //Check if we have something
                                if not (List.isEmpty filteredSymbolList) then 
                                    //If we select an ID that is already highlighted we only update the current mouse coordinates
                                    if hasCommon filteredSymbolList model.SelectedComponentIDs then
                                        UpdateMouse mousePos |> dispatch
                                    //Else we initiate a new selection
                                    else 
                                        SelectSymbols (mousePos, filteredSymbolList) |> dispatch
                                else 
                                    //CASE 3: Mouse Click on a wire
                                    //Filter wire bounding boxes on whether they contain the click point
                                    let filteredWireList = BusWire.getBoundingBoxes (model.Wire) mousePos 
                                                           //Convert IDs to strings to use in generic helper functions
                                                           |> List.map (fun (a,b,c) -> (string a, b, c))
                                                           |> filterBoxes mousePos
                                                           //Convert back to component IDs when done. 
                                                           |> List.map CommonTypes.ConnectionId
                                    //Check if we have something.
                                    if not (List.isEmpty filteredWireList) then
                                        //If we select an ID that is already highlighted we don't change anything
                                        if hasCommon filteredWireList model.SelectedWireIDs then
                                            UpdateMouse mousePos |> dispatch
                                        //Else we initiate a new selection
                                        else 
                                            SelectWires (mousePos, filteredWireList) |> dispatch
                                    else 
                                        //CASE 4: Beginning of a drag-select box        
                                        SelectMulti mousePos |> dispatch )


          OnMouseUp (fun ev ->
                        let mouseX = ev.pageX 
                        let mouseY = ev.pageY 
                        //mousePos contains the mouse coordinates adjusted for scrolling
                        let mousePos = {X = mouseX + rightScrollOffset ; Y = mouseY + downScrollOffset}
                        //In response to lifing mouse
                        SelectEnd mousePos |> dispatch)


          OnMouseMove (fun ev -> 
                            let mouseX = ev.pageX 
                            let mouseY = ev.pageY 
                            //mousePos contains the mouse coordinates adjusted for scrolling
                            let mousePos = {X = mouseX + rightScrollOffset ; Y = mouseY + downScrollOffset}
                            //In response to mouse dragging send message.

                            //In ALL cases, we need to highlight the ports of a symbol if our mouse enters the bounding box

                            //See if we can find a symbol for which the mouse is within bounding box. We will just take the first such symbol we find 
                            //and ignore any overlapping ones.
                            let stringIdBoxes = Symbol.getBoundingBoxes (model.Wire.Symbol) mousePos 
                                                |> List.map (fun (a,b,c) -> (string a, b, c))
                            match List.tryFind (pointInBoundingBox mousePos) stringIdBoxes with 
                            //We get something then highlight the symbol ports.
                            | Some a ->
                                printfn "Gotcha!"
                                let symid, tl, br = a 
                                WireMsg (BusWire.Symbol (Symbol.HighlightPorts (CommonTypes.ComponentId symid))) |> dispatch
                            //Else just return unit.
                            | None ->
                                printfn "Nothing" 
                                WireMsg (BusWire.Symbol (Symbol.HighlightPorts (CommonTypes.ComponentId ""))) |> dispatch

                            if mDown ev 
                            then SelectGrow mousePos |> dispatch
                            )
        ]
        [ svg
            [ Style 
                [
                    Border "3px solid green"
                    Height sizeInPixels
                    Width sizeInPixels
                ]

              Ref (fun html -> 
                        getSvgClientRect <- fun () -> (Some (html.getBoundingClientRect())))
            ]
            [
                //The background grid.
                g [] grid
                //The components - must be scaled with the zoom.
                g [Style [Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " 0.0 0.0 rotation zoom )]] 
                    [svgReact]
                //The select graphics e.g. dotted line, green box.
                g [] selectlist
            ]
        ]


//View function
let view (model:Model) (dispatch : Msg -> unit) =

    WireMsg (BusWire.Symbol (Symbol.Highlight model.SelectedComponentIDs)) |> dispatch
    WireMsg (BusWire.HighlightWires model.SelectedWireIDs) |> dispatch           
     
    let wDispatch wMsg = dispatch (WireMsg wMsg)
    //Get components from wire and symbol
    let svgComponents = BusWire.view model.Wire wDispatch
    //Display the canvas
    displaySvgWithZoom model svgComponents (selectGraphic model) dispatch
       



let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    //A wire message - pass on to BusWire
    | WireMsg wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map WireMsg wCmd
    //Zooming in
    | KeyPress ZoomCanvasIn -> 
        ({model with Zoom = model.Zoom * 1.25}, Cmd.none)
    //Zooming out
    | KeyPress ZoomCanvasOut -> 
        ({model with Zoom = model.Zoom/1.25}, Cmd.none)
    //Printing performace stats
    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    // all other keys are turned into SetColor commands
    | KeyPress s -> 
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey
        printfn "Key:%A" c
        model, Cmd.ofMsg (WireMsg <| BusWire.SetColor c)
        
    | UpdateMouse pos -> 
        {model with SelectBoxStart = pos; SelectBoxCurrent = pos}, Cmd.none

    //Begin Selection from a port i.e pull a dotted line
    | SelectPort data -> 
        let startPoint, port = data
        {model with 
            SelectBoxStart = startPoint;
            SelectBoxCurrent = startPoint;
            SelectionType = Port;
            SelectedComponentIDs = [];
            SelectedWireIDs = [];
            SelectedPort = Some port}, Cmd.none

    //Begin selection of symbols
    | SelectSymbols data ->
        let startPoint, symbols = data
        {model with 
            SelectBoxStart = startPoint;
            SelectBoxCurrent = startPoint;
            SelectionType = ComponentSymbol;
            SelectedComponentIDs = symbols;
            SelectedWireIDs = [];
            SelectedPort = None}, Cmd.none

    //Begin selection of wires 
    | SelectWires data -> 
         let startPoint, wires = data
         {model with 
            SelectBoxStart = startPoint;
            SelectBoxCurrent = startPoint;
            SelectionType = ComponentWire;
            SelectedComponentIDs = [];
            SelectedWireIDs = wires;
            SelectedPort = None}, Cmd.none

    //Begin dragging of a multi-select
    | SelectMulti startPoint ->
        {model with 
            SelectBoxStart = startPoint;
            SelectBoxCurrent = startPoint;
            SelectionType = MultiBox;
            SelectedComponentIDs = [];
            SelectedWireIDs = [];
            SelectedPort = None}, Cmd.none

    //Continue a selection - generated by mouse drag   
    | SelectGrow point ->
        //Obtain a translation vector indicating how the mouse position has moved
        let startPoint = model.SelectBoxCurrent
        let vector = {X = point.X - startPoint.X; Y = point.Y - startPoint.Y}
        //The selection type will not change until a mouse lift i.e SelectEnd is sent.
        match model.SelectionType with
        //This corresponds to a symbol being moved
        | ComponentSymbol -> 
            //Movement must notify bus wire so that rerouting can be done.           
            let wmodel, wCmd = BusWire.update (BusWire.Symbol (Symbol.Move (model.SelectedComponentIDs, vector))) model.Wire 
            {model with Wire = wmodel; SelectBoxCurrent = point}, Cmd.map WireMsg wCmd
        //This corresponds to a wire being moved
        | ComponentWire ->
            let wModel, wCmd = BusWire.update (BusWire.MoveWires (model.SelectedWireIDs, vector)) model.Wire
            {model with Wire = wModel; SelectBoxCurrent = point}, Cmd.map WireMsg wCmd
        //Otherwise we don't need to dispatch any messages to Symbol/BusWire components
        | _ ->
            {model with SelectBoxCurrent = point}, Cmd.none

    //Finish a selection operation - generated by a mouse click being released.    
    | SelectEnd endPoint ->
        match model.SelectionType with 
        //If we're dragging from a port
        | Port ->
            match (Symbol.isPort (model.Wire.Symbol) endPoint) with 
            //If we get a port, create a wire between the ports.
            | Some a ->
                let endportcoord, endportid = a
                let startportid = 
                    match model.SelectedPort with 
                    | Some port -> 
                        port 
                    | None -> failwithf "Unexpected case in selection type port - unable to create connection."
                let wModel, wCmd = BusWire.update (BusWire.AddWire (startportid, endportid)) model.Wire
                {model with
                     Wire = wModel;
                     SelectBoxStart = origin;
                     SelectBoxCurrent = origin;
                     SelectionType = Nothing;
                     SelectedComponentIDs = [];
                     SelectedWireIDs = [];
                     SelectedPort = None}, Cmd.none
            //Else revert to default.
            | None ->
                {model with
                    SelectBoxStart = origin;
                    SelectBoxCurrent = origin;
                    SelectionType = Nothing;
                    SelectedComponentIDs = [];
                    SelectedWireIDs = [];
                    SelectedPort = None}, Cmd.none
        //If we're selecting multiple components in a box         
        | MultiBox ->
            //Prepare this tuple so that we can find the symbols inside the box.
            let draggedBox = (model.SelectBoxStart, endPoint)
            //Find all symbols inside the dragged box and select them. Selection mode is now ComponentSymbol. 
            let selectedSymbols = 
                Symbol.getBoundingBoxes (model.Wire.Symbol) endPoint
                |> List.map (fun (a,b,c) -> (string a, b, c))
                |> List.filter (boxEncloses draggedBox)
                |> List.map (obtainID >> CommonTypes.ComponentId)
            //If we get something then we move to ComponentSymbol state with the selected symbols.
            if not (List.isEmpty selectedSymbols) then 
                {model with
                    SelectBoxStart = origin;
                    SelectBoxCurrent = origin;
                    SelectionType = ComponentSymbol;
                    SelectedComponentIDs = selectedSymbols;
                    SelectedWireIDs = [];
                    SelectedPort = None}, Cmd.none 
            //Otherwise we have nothing
            else 
                {model with
                    SelectBoxStart = origin;
                    SelectBoxCurrent = origin;
                    SelectionType = Nothing;
                    SelectedComponentIDs = [];
                    SelectedWireIDs = [];
                    SelectedPort = None}, Cmd.none 
        //Otherwise for ComponentSymbol and ComponentWire, we just keep the selected and set selection coordinates to default.
        | _ -> 
            {model with SelectBoxStart = origin; SelectBoxCurrent = origin}, Cmd.none           


let init() = 
    let wiremodel,cmds = (BusWire.init 400)()
    {
        Wire = wiremodel
        SelectBoxStart = origin
        SelectBoxCurrent = origin
        SelectionType = Nothing
        SelectedComponentIDs = []
        SelectedWireIDs = []
        SelectedPort = None
        Zoom = 1.0
    }, Cmd.map WireMsg cmds