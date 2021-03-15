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
    SelectedWireIDs: (int * CommonTypes.ConnectionId) list
    SelectedPort: CommonTypes.PortId option
    AddKey: int
    Zoom: float
    }

//Keyboard messages (see Renderer.fs)
type KeyboardMsg =
    | CtrlS 
    | AltC 
    | AltV 
    | AltZ 
    | AltShiftZ 
    | RotateSymbol 
    | ScaleUpSymbol
    | ScaleDownSymbol
    | AltA
    | DEL 
    | ZoomCanvasIn 
    | ZoomCanvasOut 
    
    
//The message type
type Msg =
    | WireMsg of BusWire.Msg
    | KeyPress of KeyboardMsg
    | SelectSymbols of (XYPos * CommonTypes.ComponentId list)
    | SelectWires of (XYPos * (int * CommonTypes.ConnectionId) list) 
    | SelectPort of (XYPos * CommonTypes.PortId)
    | SelectMulti of XYPos 
    | SelectGrow of XYPos
    | SelectEnd of XYPos
    | UpdateMouse of XYPos


//NEXT DEFINE CONSTANTS THAT ARE USED IN THE CODE: 

//Define the origin point for future reference
let origin = {X = 0.0; Y = 0.0}

//The canvas is square - we define length of one side in pixels without zoom
let UNZOOMEDCANVAS = 1000.0

//We define side-length of a grid square (1/100 of canvas)
let UNZOOMEDGRID = UNZOOMEDCANVAS/100.0

//DEFINE HELPER FUNCTIONS:


//NOTE: Functions from here onwards use the coordinate system of symbol and buswire as they are concerned with bounding box calculations -
//these cocrdinates are not scaled with the zoom i.e. they are the coordinates when zoom = 1.0.



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

let obtainID2 (data: (int * string * XYPos * XYPos)) = 
    let index,id, topLeft, bottomRight = data 
    (index,id)    
let obtainCorners2 (data: (int * string * XYPos * XYPos)) =
    let index,id, topLeft, bottomRight = data 
    (topLeft, bottomRight)

let pointInBoundingBox2 (point: XYPos) (box: (int * string * XYPos * XYPos)) = 
    let corners = obtainCorners2 box 
    inBox point corners

let filterBoxes2 (point: XYPos) (boxList: (int * string * XYPos * XYPos) list)  =
    boxList
    |> List.filter (pointInBoundingBox2 point)
    |> List.map obtainID2

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


//Function that translates the addKey value of model to an initial position for adding a symbol
let iniPos zoom addkey =
    let yOffset = float (addkey%10)
    let xOffset = float (addkey/10)
    //define x-y gap for no zoom between each new symbol
    let gap = 9.0*UNZOOMEDGRID
    {X = (UNZOOMEDGRID + gap*xOffset)*zoom; Y = (UNZOOMEDGRID + gap*yOffset)*zoom}

//Snap-to-grid. Takes in a point (will be top-left coord of bounding box) and returns a 
//translation vector indicating where the point should move to be one a grid intersection point.
let snapgrid (point: XYPos) = 
    let newXSquare = round (point.X/UNZOOMEDGRID)
    let newYSquare = round (point.Y/UNZOOMEDGRID)
    //return translation vector
    {X = (UNZOOMEDGRID*newXSquare - point.X); Y = (UNZOOMEDGRID*newYSquare - point.Y)}




//NOTE: Functions from here onwards use the coordinate system of sheet as they are concerned with display - 
//these cocrdinates can be scaled with the zoom.



//It is not efficient to transfer zooming to coordinates held in symbol and busWire - instead we define 
//a function to scale down the coordinates in sheet so that an interface with other modules in the presence
//of zooming is possible.
let zoomScale (zoom: float) (pt: XYPos) = 
    {X = pt.X/zoom; Y = pt.Y/zoom}

    
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


 /// this will be set when the canvas is first created and then provide info about how the canvas is scrolled.
let mutable getSvgClientRect: (unit -> Types.ClientRect option) = (fun () -> None)

/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point
let displaySvgWithZoom (model:Model) (svgReact: ReactElement) (selectGraphic: ReactElement option) (dispatch: Dispatch<Msg>)=
    
    let zoom = model.Zoom

    let sizeInPixels = sprintf "%.2fpx" (UNZOOMEDCANVAS * zoom)
    
    let rightscroll, downscroll = 
        match getSvgClientRect () with 
        | Some a ->
            a.right, a.bottom 
        | None -> 
            0.0, 0.0 

    //Finding the scroll position
    let rightScrollOffset = UNZOOMEDCANVAS*zoom - rightscroll
    let downScrollOffset = UNZOOMEDCANVAS*zoom - downscroll

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

                            //"Scaled down" version of coordinates to interface with other modules for zoom
                            let scaledMousePos = zoomScale model.Zoom mousePos

                            //CASE 1: Click at Port
                            match (Symbol.isPort (model.Wire.Symbol) scaledMousePos) with 
                            | Some (a: (XYPos * CommonTypes.PortId)) -> 
                                let portcoord, portid = a
                                SelectPort (mousePos, portid) |> dispatch
                            | None -> 
                                //CASE 2: Mouse Click within a symbol 
                                //Get symbol bounding boxes and filter based on whether they contain the click point
                                let filteredSymbolList = Symbol.getBoundingBoxes (model.Wire.Symbol) scaledMousePos
                                                         //Convert IDs to strings to use in generic helper functions
                                                         |> List.map (fun (a,b,c) -> (string a, b, c))
                                                         |> filterBoxes scaledMousePos
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
                                    let filteredWireList = BusWire.getBoundingBoxes (model.Wire) scaledMousePos 
                                                           //Convert IDs to strings to use in generic helper functions
                                                           |> List.map (fun (index, id ,b,c) -> (index,string id, b, c))
                                                           |> filterBoxes2 scaledMousePos
                                                           //Convert back to component IDs when done. 
                                                           |> List.map (fun (a,b)->(a, CommonTypes.ConnectionId b))
                                    //Check if we have something.
                                    if not (List.isEmpty filteredWireList) then
                                        //If we select a wire ID that is already highlighted we don't change anything
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

                            //"Scaled down" version of coordinates to interface with other modules for zoom
                            let scaledMousePos = zoomScale model.Zoom mousePos

                            //In response to mouse dragging send message.

                            //In ALL cases, we need to highlight the ports of a symbol if our mouse enters the bounding box

                            //Find all symbol IDs for which the mouse is within bounding box. We want to highlight ports on all of these symbols.
                            let symIds = Symbol.getBoundingBoxes (model.Wire.Symbol) scaledMousePos 
                                            |> List.map (fun (a,b,c) -> (string a, b, c))
                                            |> List.filter (pointInBoundingBox scaledMousePos)
                                            |> List.map (fun (strid, tl, br) -> CommonTypes.ComponentId strid)
                            //Highlight the list of symbol ports.
                            WireMsg (BusWire.Symbol (Symbol.HighlightPorts symIds)) |> dispatch
                            

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

// END OF UBIQUITOUS HELPER FUNCTIONS


//View function
let view (model:Model) (dispatch : Msg -> unit) =

    WireMsg (BusWire.Symbol (Symbol.Highlight model.SelectedComponentIDs)) |> dispatch
    WireMsg (BusWire.HighlightWires (List.map snd model.SelectedWireIDs)) |> dispatch           
     
    let wDispatch wMsg = dispatch (WireMsg wMsg)
    //Get components from wire and symbol
    let svgComponents = BusWire.view model.Wire wDispatch
    //Display the canvas
    displaySvgWithZoom model svgComponents (selectGraphic model) dispatch
       

//DEFINE SOME HELPER FUNCTIONS FOR UPDATE TO SEND MESSAGES

//Send message to BusWire
let sendBusWireMsg (model: Model) (msg: BusWire.Msg) =
    BusWire.update msg model.Wire

//Send message to Symbol
let sendSymbolMsg (model: Model) (msg: Symbol.Msg) = 
    BusWire.update (BusWire.Symbol msg) model.Wire

//Send BusWire message and update model
let updateBusWireModel (model: Model) (msg: BusWire.Msg) =
    let wModel, wCmd = sendBusWireMsg model msg
    {model with Wire = wModel}, Cmd.map WireMsg wCmd

//Send BusWire message and update model
let updateSymbolModel (model: Model) (msg: Symbol.Msg) =
    let wModel, wCmd = sendSymbolMsg model msg
    {model with Wire = wModel}, Cmd.map WireMsg wCmd


//Update function begins here
let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    //A wire message - pass on to BusWire
    | WireMsg wMsg -> 
        updateBusWireModel model wMsg

    //Zooming in
    | KeyPress ZoomCanvasIn -> 
        ({model with Zoom = model.Zoom * 1.25}, Cmd.none)

    //Zooming out
    | KeyPress ZoomCanvasOut -> 
        ({model with Zoom = model.Zoom/1.25}, Cmd.none)

    //NOTE: The following case is part of Joanna's demo. It implements:
    //          - Rotating symbol utility. Rotates symbol clockwise by 90 degrees.
    //          - Scaling up symbol utility. Magnifies symbol by 1.25.
    //          - Scaling down symbol utility. Shrinks symbol to scale 0.8 = 1/1.25.
    // Stretching and clockwise rotation NOT implemented.
    | KeyPress k when (k = RotateSymbol || k = ScaleUpSymbol || k = ScaleDownSymbol) -> 
        if List.isEmpty model.SelectedComponentIDs then 
            model, Cmd.none
        else 
            let symid = List.head model.SelectedComponentIDs
            match k with 
            //Rotation
            | RotateSymbol -> 
                updateSymbolModel model  (Symbol.Rotate (symid, 90))
            //Magnification
            | ScaleUpSymbol -> 
                let scaleamt = 1.25
                updateSymbolModel model (Symbol.Scale (symid, {X = scaleamt; Y = scaleamt}))
            //Shrinking
            | ScaleDownSymbol -> 
                let scaleamt = 0.8 
                updateSymbolModel model (Symbol.Scale (symid, {X = scaleamt; Y = scaleamt}))
            | _ -> 
                failwithf "Unexpected input in symbol transformation."

    //Deleting a selected symbol(s)/wire(s).
    | KeyPress DEL -> 
        match model.SelectionType with 
        //Delete symbol(s)
        | ComponentSymbol -> 
            //We first need to find and delete all connected wires of the symbol(s)
            let connectedWires = model.SelectedComponentIDs
                                 //Obtain portID lists for each selected component
                                 |> List.map (Symbol.getPortIds model.Wire.Symbol) 
                                 //Obtain the wires corresponding to all these ports into a single list
                                 |> List.collect (BusWire.getWireIdsFromPortIds model.Wire)
                                 //Remove repeated elements 
                                 |> List.distinct
            //Delete the wires first.                  
            let wModel1, wCmd1 = sendBusWireMsg model (BusWire.DeleteWires connectedWires)
            //Then delete the symbols.
            let wModel2, wCmd2 = BusWire.update (BusWire.Symbol (Symbol.Delete model.SelectedComponentIDs)) wModel1
            //return updated model
            {model with Wire = wModel2}, Cmd.batch [Cmd.map WireMsg wCmd2; Cmd.map WireMsg wCmd1]
        //delete wire(s)
        | ComponentWire -> 
            updateBusWireModel model (BusWire.DeleteWires (List.map snd model.SelectedWireIDs))
        | _ -> 
            model, Cmd.none


    //To test adding an AND symbol
    | KeyPress AltA -> 
        let wModel, wCmd = sendSymbolMsg model (Symbol.Add (CommonTypes.ComponentType.And, (iniPos model.Zoom model.AddKey), 2, 1))
        {model with Wire = wModel; AddKey = (model.AddKey + 1)%100}, Cmd.map WireMsg wCmd

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
            SelectedWireIDs = wires ;
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
        let scaledVector = zoomScale model.Zoom vector
        //The selection type will not change until a mouse lift i.e SelectEnd is sent.
        match model.SelectionType with
        //This corresponds to a symbol being moved
        | ComponentSymbol -> 
            //Movement must notify bus wire so that rerouting can be done.           
            let wmodel, wCmd = sendSymbolMsg model (Symbol.Move (model.SelectedComponentIDs, scaledVector))
            {model with Wire = wmodel; SelectBoxCurrent = point}, Cmd.map WireMsg wCmd
        //This corresponds to a wire being moved
        | ComponentWire ->
            let wModel, wCmd = sendBusWireMsg model (BusWire.MoveWires ((List.map snd model.SelectedWireIDs), scaledVector))
            {model with Wire = wModel; SelectBoxCurrent = point}, Cmd.map WireMsg wCmd
        //Otherwise we don't need to dispatch any messages to Symbol/BusWire components
        | _ ->
            {model with SelectBoxCurrent = point}, Cmd.none


    //Finish a selection operation - generated by a mouse click being released.    
    | SelectEnd endPoint ->

        let scale = zoomScale model.Zoom
        let scaledEndPoint = scale endPoint
        match model.SelectionType with 
        //If we're dragging from a port
        | Port ->
            match (Symbol.isPort (model.Wire.Symbol) scaledEndPoint) with 
            //If we get a port, create a wire between the ports.
            | Some a ->
                //Find the start and end port IDs
                let endportcoord, endportid = a
                let startportid = 
                    match model.SelectedPort with 
                    | Some port -> 
                        port 
                    | None -> failwithf "Unexpected case in selection type port - unable to create connection."
                //Create new wire and update model
                let wModel, wCmd = sendBusWireMsg model (BusWire.AddWire (startportid, endportid))
                {model with
                     Wire = wModel;
                     SelectBoxStart = origin;
                     SelectBoxCurrent = origin;
                     SelectionType = Nothing;
                     SelectedComponentIDs = [];
                     SelectedWireIDs = [];
                     SelectedPort = None}, Cmd.none
            //Else revert to default model state.
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
            //Prepare this tuple so that we can find the symbols inside the box. Must be scaled as we are interfacing 
            //with coordinates from other modules.
            let draggedBox = ((scale model.SelectBoxStart), (scale endPoint))
            //Find all symbols inside the dragged box and select them. Selection mode is now ComponentSymbol. 
            let selectedSymbols = 
                Symbol.getBoundingBoxes (model.Wire.Symbol) scaledEndPoint
                |> List.map (fun (a,b,c) -> (string a, b, c))
                |> List.filter (boxEncloses draggedBox)
                |> List.map (obtainID >> CommonTypes.ComponentId)
           
            //If we get something then we move to ComponentSymbol state with the selected symbols and associated wires.
            if not (List.isEmpty selectedSymbols) then 

                //We are finding all wires that are connected to at least 2 of the selected symbols.
                let selectedWires =
                    selectedSymbols
                    //Obtain portID lists for each selected component
                    |> List.map (Symbol.getPortIds model.Wire.Symbol) 
                    //Obtain the wires corresponding to all these ports
                    |> List.collect (BusWire.getWireIdsFromPortIds model.Wire)
                    //Count how many of each ID there are (note: id below is identity mapping) 
                    |> List.countBy id
                    //Filter out wireIDs that only appear once 
                    |> List.filter (fun (id, count) -> count > 1)
                    //Only interested in the first elements of tuples i.e. ids
                    |> List.map (fun a-> (int 1, fst a))
                    
                {model with
                    SelectBoxStart = origin;
                    SelectBoxCurrent = origin;
                    SelectionType = ComponentSymbol;
                    SelectedComponentIDs = selectedSymbols;
                    SelectedWireIDs = selectedWires;
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
        //For a symbol we will implement snap-to-grid. We move the box according to the returned translation vector. 
        | ComponentSymbol ->
            let pointToShift = 
                //Find the top-left corner of symbol bounding box based on id - we only search for id of the first selected symbol in the list 
                //and perform the translation based on this symbol.
                Symbol.getBoundingBoxes (model.Wire.Symbol) scaledEndPoint 
                //We are sure that we will find a box with the searched id. We return this box.
                |> List.find (fun (id, tl, br) -> id = List.head (model.SelectedComponentIDs))
                |> (fun (id,tl,br) -> tl)
            
            //The vector is already scaled to zoom = 1.0 as the point we pass it is from the coordinate system of 
            //symbol and busWire.
            let scaledVector = snapgrid pointToShift
            //Send move message based on translation vector
            let wmodel, wCmd = sendSymbolMsg model (Symbol.Move (model.SelectedComponentIDs, scaledVector))
            //We set selection coordinates to default.
            {model with Wire = wmodel; SelectBoxStart = origin; SelectBoxCurrent = origin}, Cmd.map WireMsg wCmd

        //Otherwise for ComponentWire, we just keep the selected and set selection coordinates to default.
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
        AddKey = 0
        Zoom = 1.0
    }, Cmd.map WireMsg cmds