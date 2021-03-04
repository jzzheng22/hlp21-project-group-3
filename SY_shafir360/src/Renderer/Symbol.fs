module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers


//interface functions are at the very bottom 

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//
type ExtendedPortType =
    |In 
    |Out 
    |En

type PortLocal =
    {

        Port: CommonTypes.Port
        Pos: XYPos
        Height: float
        Width: float
        Inverted: bool
        Name: string
        Highlight: bool
        Wirewidth : int
        ExtendedType : ExtendedPortType
    }

//global types
let portGap = float 30
let portVerticalOffset = int 40
let symbolwidth =  float 200
let portHeight = float 15
let portWidth = float 15

type EnablePort =
    {
        Pos: XYPos
        Height:float
        Width:float
        Name:string
        Highlight : bool
        HostId :string
        Id :string
    }


    
/// Model to generate one symbol (skeleton). Id is a unique Id 
/// for the symbol shared with Issie Component type.
/// The real type will obviously be much larger.
/// Complex information that never changes (other than Id) should 
/// probably not be here, but looked up via some function
/// from a more compact form, so that comparison of two Symbols to
/// determine are they the same is fast.
type Symbol =
    {
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        H: float
        W: float
        Inputport: PortLocal list
        Outputport : PortLocal list
        Name: string
        Highlight: bool
        HighlightAllPorts : bool
        SymbolType: CommonTypes.ComponentType
    }


type Model = Symbol list

//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    /// coords not adjusted for top-level zoom
    | StartDragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    /// coords not adjusted for top-level zoom
    | Dragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | EndDragging of sId : CommonTypes.ComponentId
    | AddCircle of XYPos // used by demo code to add a circle
    | DeleteSymbol of sId:CommonTypes.ComponentId 
    | UpdateSymbolModelWithComponent of CommonTypes.Component 
    |Add of symbolInfo: CommonTypes.ComponentType * pos : XYPos * inputLength : int * outputLength : int
    |Delete of sIdList : CommonTypes.ComponentId list
    |Move of sIDListAndPos : CommonTypes.ComponentId list * Pos : XYPos
    | Highlight of symbolIdList: CommonTypes.ComponentId list
    | HighlightPorts of symbolIdList : CommonTypes.ComponentId list
    | HighlightSinglePort of portId : string
  //  | AddSymbol (SymbolType, Pos, input, output) ->
    //    (CreateNewSymbol SYmbolType Pos inputLength outputLength) :: model, Cmd.none// Issie interface


//---------------------------------helper types and functions----------------//



let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let addPos (pos:XYPos) (translation:XYPos) : XYPos =
    {
        X = pos.X + translation.X;
        Y = pos.Y + translation.Y
    }

//-----------------------------Skeleton Model Type for symbols----------------//


//interface helper function
let isPortinSymbol (portId:CommonTypes.PortId) (symbol: Symbol) = 
    List.append symbol.Inputport symbol.Outputport
    |> List.tryFind (fun port -> port.Port.Id = string(portId))
    |> function
    | Some p -> true
    | None ->  false

let findPortFromModel (model:Model) (portId:CommonTypes.PortId) :PortLocal =
    model
    |> List.tryFind (fun (symbol: Symbol) -> isPortinSymbol portId symbol)
    |> function
    |Some symbol -> List.append symbol.Inputport symbol.Outputport
                    |> List.find (fun port -> port.Port.Id = string portId)              
    |None -> failwithf "error: no port with ID: %s found" (string portId)

let isWithinBox (topLeftCoord:XYPos) (H:float) (W:float) (searchCoord:XYPos) =
    let cond1 = searchCoord.X > topLeftCoord.X
    let cond2 = searchCoord.X < topLeftCoord.X + W
    let cond3 = searchCoord.Y >  topLeftCoord.Y
    let cond4 = searchCoord.Y <  topLeftCoord.Y + H
    cond1 && cond2 && cond3 && cond4


//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (symbolType:CommonTypes.ComponentType) (pos:XYPos) (inputLength: int) (outputLength: int) : Symbol =

    let symboliD = CommonTypes.ComponentId (Helpers.uuid())
    let name = 
        match symbolType with
        |CommonTypes.ComponentType.And -> "And"
        |CommonTypes.ComponentType.Not -> "Not"
        |CommonTypes.ComponentType.Mux2 -> "Mux2"
        |CommonTypes.ComponentType.AsyncROM a -> "AsyncRom"
        |CommonTypes.ComponentType.BusSelection (a,b) ->"BusSelection"
       // |CommonTypes.ComponentType.Circle -> "Circle"
        |CommonTypes.ComponentType.Constant (a,b) ->"Constant"
        |CommonTypes.ComponentType.Decode4 -> "Decode4"
        |CommonTypes.ComponentType.Demux2 -> "Demux2"
        |CommonTypes.ComponentType.DFF -> "DFF"
        |CommonTypes.ComponentType.DFFE -> "DFFE"
        |CommonTypes.ComponentType.Input i ->"Input"
        |CommonTypes.ComponentType.IOLabel -> "IO Label" //what is this
        |CommonTypes.ComponentType.MergeWires -> "Merge Wires"
        |CommonTypes.ComponentType.Nand -> "Nand"
        |CommonTypes.ComponentType.NbitsAdder i -> "%i Bit Adder" 
        |CommonTypes.ComponentType.Nor -> "Nor"
        |CommonTypes.ComponentType.Or -> "Or"
        |CommonTypes.ComponentType.Output i ->"Output"
        |CommonTypes.ComponentType.RAM m -> "RAM"
        |CommonTypes.ComponentType.Register i -> "Register"
        |CommonTypes.ComponentType.RegisterE i -> "RegisterE"
        |CommonTypes.ComponentType.ROM m -> "ROM"
        |CommonTypes.ComponentType.SplitWire i -> "SplitWire"
        |CommonTypes.ComponentType.Xnor -> "XNOR"
        |CommonTypes.ComponentType.Xor -> "XOR"
        |CommonTypes.ComponentType.Custom c-> "Custom"
        
    
    let ifInverter =  
        match symbolType with   
        |CommonTypes.ComponentType.Nand -> true
        |CommonTypes.ComponentType.Nor -> true
        |CommonTypes.ComponentType.Not -> true
        |CommonTypes.ComponentType.Xnor -> true
        |_ -> false

    
       
    let createPort  (portT: CommonTypes.PortType) (extendedport: ExtendedPortType) (symbolHeight:float option) (index:int) : PortLocal =

        let px =match extendedport with
                |In ->pos.X
                |Out ->pos.X + symbolwidth - portWidth
                |En ->pos.X + (symbolwidth/2.) - (portWidth/2.)
                    
        let py =match extendedport with
                |In -> pos.Y + 40.0 + (float(index) * portGap)
                |Out ->pos.Y + 40.0 + (float(index) * portGap)
                |En -> pos.Y - portHeight + (match symbolHeight with
                                            |Some i -> i
                                            |None -> failwithf "Error when making enable port.No symbol height found")

        let invert =if portT = CommonTypes.PortType.Input then 
                        false
                    else
                        ifInverter
        
        {
            Height = portHeight
            Width = portWidth
            Inverted = invert
            Pos = {X=px ;Y= py}
            Name = match extendedport with
                    |In -> sprintf "In %i" index
                    |Out -> sprintf "Out %i" index
                    |En -> sprintf "En" 
                    
                    
            Port = {
                Id = Helpers.uuid();
                PortNumber = Some index
                PortType = portT
                HostId = string(symboliD)
            }
            Highlight = false
            Wirewidth = 1
            ExtendedType = extendedport
        }
    
    
    let createPortList (portSize:int) (portType:CommonTypes.PortType) (extendedport:ExtendedPortType) : PortLocal list = 
        [0..portSize-1]
        |>List.map (createPort portType extendedport None )
    
    let outputListIntermediate =  createPortList outputLength CommonTypes.PortType.Output ExtendedPortType.Out
    let inputList =createPortList inputLength CommonTypes.PortType.Input ExtendedPortType.In
    let createHightFromPortLength = (portVerticalOffset) + ( int portGap * List.max [inputList.Length; outputListIntermediate.Length] )
    let height = float createHightFromPortLength
    let makeEnablePort = createPort CommonTypes.PortType.Input ExtendedPortType.En (Some(height)) outputListIntermediate.Length
    let outputList = match symbolType with
                     |CommonTypes.ComponentType.DFFE ->  makeEnablePort :: outputListIntermediate
                     |CommonTypes.ComponentType.RegisterE w->  makeEnablePort :: outputListIntermediate
                     |_ -> outputListIntermediate

    let createEnablePort: EnablePort =
        {
            Pos = {X=pos.X + (symbolwidth/2.0) ;Y= pos.Y + height }
            Height = portHeight
            Width = portWidth
            Name = "En"
            Highlight = false
            HostId = string(symboliD)
            Id = Helpers.uuid();

        }        
    

    let ifEnable =
            match symbolType with
            |CommonTypes.ComponentType.DFFE -> Some (createEnablePort)
            |CommonTypes.ComponentType.RegisterE i -> Some (createEnablePort)
            |_ -> None

    match symbolType with 
    |CommonTypes.ComponentType.MergeWires ->
        {
            Pos = pos 
            LastDragPos = {X=0. ; Y=0.} // initial value can always be this
            IsDragging = false // initial value can always be this
            Id = symboliD// create a unique id for this symbol
            H = height
            W = symbolwidth
            Inputport = [createPort CommonTypes.PortType.Input ExtendedPortType.In None 0;
                         createPort CommonTypes.PortType.Input ExtendedPortType.In None 2]
            Outputport = [createPort CommonTypes.PortType.Output ExtendedPortType.Out None 1]              
            Name = name
            Highlight = false
            HighlightAllPorts = false
            SymbolType =symbolType
        }
    |CommonTypes.ComponentType.SplitWire i -> 
        {
            Pos = pos 
            LastDragPos = {X=0. ; Y=0.} // initial value can always be this
            IsDragging = false // initial value can always be this
            Id = symboliD// create a unique id for this symbol
            H = height
            W = symbolwidth
            Inputport = [createPort CommonTypes.PortType.Input ExtendedPortType.In None 1]
            Outputport = [createPort CommonTypes.PortType.Output ExtendedPortType.Out None 0;
                          createPort CommonTypes.PortType.Output ExtendedPortType.Out None 2]              
            Name = name
            Highlight = false
            HighlightAllPorts = false
            SymbolType =symbolType
        }
    |_->{
            Pos = pos 
            LastDragPos = {X=0. ; Y=0.} // initial value can always be this
            IsDragging = false // initial value can always be this
            Id = symboliD// create a unique id for this symbol
            H = height
            W = symbolwidth
            Inputport = inputList
            Outputport = outputList
            Name = name
            Highlight = false
            HighlightAllPorts = false
            SymbolType =symbolType
        }
    
        
    


/// Dummy function for test. The real init would probably have no symbols.
let init () =
    
    [(createNewSymbol CommonTypes.ComponentType.Nand {X=0.;Y=0.} 2 1 );
      (createNewSymbol CommonTypes.ComponentType.And {X=100.;Y=100.} 3 4 );
      (createNewSymbol CommonTypes.ComponentType.DFFE {X=200.;Y=200.} 0 4 );
      (createNewSymbol CommonTypes.ComponentType.MergeWires {X=300.;Y=300.} 0 4 )
      //(createNewSymbol (CommonTypes.ComponentType.SplitWire 2 )  {X=400.;Y=400.} 0 4 )
      ]

    , Cmd.none
    
/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | Add  (symbolType, pos,inputLength ,outputLength)  -> 
        createNewSymbol symbolType pos inputLength outputLength :: model, Cmd.none
    | Delete sIdList -> 
        List.filter (fun symbol -> List.contains symbol.Id sIdList = false) model, Cmd.none
    | StartDragging (sId, pagePos) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                { sym with
                    LastDragPos = pagePos
                    IsDragging = true
                }
        )
        , Cmd.none

    | Dragging (rank, pagePos) ->
        model
        |> List.map (fun sym ->
            if rank <> sym.Id then
                sym
            else
                let diff = posDiff pagePos sym.LastDragPos
                { sym with
                    Pos = posAdd sym.Pos diff
                    LastDragPos = pagePos
                }
        )
        , Cmd.none

    | EndDragging sId ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then 
                sym
            else
                { sym with
                    IsDragging = false 
                }
        )
        , Cmd.none

    |Move (sIDList, translation) ->
        model
        |> List.map (fun symbol ->
            if List.contains symbol.Id sIDList then
                { symbol with
                    Pos = addPos symbol.Pos translation
                    
                    Inputport = List.map (fun port -> {port with Pos = (addPos port.Pos translation) } ) symbol.Inputport
                    Outputport = List.map (fun port -> {port with Pos = (addPos port.Pos translation) } ) symbol.Outputport
                    //printfn "moveX %A movey " Pos
                }
            else
                symbol
        )
        , Cmd.none

    | Highlight symbolIdList ->
        model
        |> List.map (fun symbol ->
            if List.contains symbol.Id symbolIdList then 
                { symbol with
                    Highlight = true
                }
            else
                { symbol with
                    Highlight = false 
                }
        )
        , Cmd.none

    | HighlightPorts symbolIdList ->
        model
        |> List.map (fun symbol ->
            if List.contains symbol.Id  symbolIdList then 
                { symbol with
                    HighlightAllPorts = true
                }
            else
                { symbol with
                    HighlightAllPorts= false 
                }
        )
        , Cmd.none
(*
    | HighlightSinglePort portID ->
        model
        |> List.map (fun symbol ->
            let result =List.append symbol.Inputport symbol.Outputport
                        |> List.map (fun port -> port.Port.Id )
            if List.contains portID result  then 
                { symbol with
                    if 
                }
            else
                { symbol with
                    HighlightAllPorts= false 
                }
        )
        , Cmd.none
*)


   // | MouseMsg _ -> model, Cmd.none // allow unused mouse messags
    //| _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderCircleProps =
    {
        Circle : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

type private RenderSymbolProps =
    {
        Symbol: Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }


/// View for one symbol with caching for efficient execution when input does not change
let private renderCircle =
    FunctionComponent.Of(
        fun (props : RenderCircleProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Circle.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )

            let color =
                if props.Circle.IsDragging then
                    "lightblue"
                else
                    "grey"

            circle
                [ 
                    OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Circle.Id
                        |> props.Dispatch
                    )
                    OnMouseDown (fun ev -> 
                        // See note above re coords wrong if zoom <> 1.0
                        StartDragging (props.Circle.Id, posOf ev.pageX ev.pageY)
                        |> props.Dispatch
                        document.addEventListener("mousemove", handleMouseMove.current)
                    )
                    Cx props.Circle.Pos.X
                    Cy props.Circle.Pos.Y
                    R 0.
                    SVGAttr.Fill color
                    SVGAttr.Stroke color
                    SVGAttr.StrokeWidth 1
                ]
                [ ]
    , "Circle"
    , equalsButFunctions
    )

let private renderSymbol =
    FunctionComponent.Of(
        
        fun (props : RenderSymbolProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Symbol.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )

            let boxcolor =
                if props.Symbol.Highlight then
                    "red"
                else
                    "grey"

            let fX = props.Symbol.Pos.X
            let fY = props.Symbol.Pos.Y
            let bWidth = props.Symbol.W
            let bHeight = props.Symbol.H
            let outputPortSize = props.Symbol.Outputport.Length
            let inputPortSize = props.Symbol.Inputport.Length
            let scaleFactor=1.0 // to demonstrate svg scaling
            let rotation=0 // to demonstrate svg rotation (in degrees)

            let makeLine (pos1:XYPos) (pos2:XYPos) : ReactElement=
                line[
                    X1 pos1.X
                    Y1 pos1.Y
                    X2 pos2.X
                    Y2 pos2.Y
                    SVGAttr.Stroke (if props.Symbol.Highlight then "Red" else "Black")
                    SVGAttr.StrokeWidth 2
                    SVGAttr.StrokeLinecap "round"][]
                

            let makeBoxCoordinates (topleftPos:XYPos) (height:float) (width:float) = 
                let xCoord = topleftPos.X
                let yCoord = topleftPos.Y
                let (x1,y1) = (xCoord,yCoord)
                let (x2,y2) = (xCoord+width,yCoord)
                let (x3,y3) = (xCoord+width,yCoord+height)
                let (x4,y4) = (xCoord,yCoord+height)
                string(x1)+","+string(y1)+" "+string(x2)+","+string(y2)+" "+string(x3)+","+string(y3)+" "+string(x4)+","+string(y4)
        
            //let boxCoordinates = makeBoxCoordinates {X=fX ;Y = fY} (float bHeight) (float bWidth)

            

            let makeBox : ReactElement list =
                let strokecolour =if props.Symbol.HighlightAllPorts then
                                    "green"
                                  else
                                    "black"

                [
                    polygon[
                        SVGAttr.Points (makeBoxCoordinates props.Symbol.Pos props.Symbol.H props.Symbol.W)
                        SVGAttr.StrokeWidth "3px"
                        SVGAttr.Stroke strokecolour
                        SVGAttr.FillOpacity 0.2
                        SVGAttr.Fill boxcolor
                    ][]
                    
                    text [ // Bus Decode Text
                        X (fX + ((float bWidth)/2.0)); 
                        Y (fY + 5.0); 
                        Style [
                            TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                            DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                            FontSize "25px"
                            FontWeight "Bold"
                            Fill strokecolour // demo font color
                        ]
                    ] [str <| sprintf "%s" props.Symbol.Name]

                ]

            
            let makeWireSymbolWires() : ReactElement list =
                let outputPoint (port:PortLocal) = {X=port.Pos.X;Y=port.Pos.Y + port.Height/2.} 
                let inputPoint (port:PortLocal) = {X=port.Pos.X + port.Width; Y= port.Pos.Y + port.Height/2.}
                //outputPoint
                match props.Symbol.SymbolType with
                |CommonTypes.ComponentType.MergeWires ->let toPoint = outputPoint (List.head props.Symbol.Outputport) 
                                                        props.Symbol.Inputport
                                                        |> List.map (fun port->inputPoint port
                                                                               |>makeLine toPoint) 

                |CommonTypes.ComponentType.SplitWire i ->   let fromPoint = inputPoint (List.head props.Symbol.Inputport) 
                                                            props.Symbol.Outputport
                                                            |> List.map (fun port->outputPoint port
                                                                                   |>makeLine fromPoint) 
                |_-> printfn "bob" 
                     failwithf "error 321"
            let drawport (port:PortLocal)  =
                
                let strokecolour =if props.Symbol.HighlightAllPorts then
                                    "red"
                                  else if port.Highlight  then
                                    "green"
                                  else
                                    "black"

                
                let coord = makeBoxCoordinates port.Pos port.Height port.Width 

                let colour = if port.Inverted  then
                                "Black"
                             else 
                                "White"
                
                [
                    polygon[
                        SVGAttr.Points coord
                        SVGAttr.StrokeWidth "3px"
                        SVGAttr.Stroke strokecolour
                        SVGAttr.FillOpacity 1
                        SVGAttr.Fill colour
                    ][]

                    text [ // Bus Decode Text
                        let fontSize = 10
                        let textXPos =  if port.Port.PortType = CommonTypes.PortType.Input then
                                            port.Pos.X + port.Width + float 2.0
                                        else
                                           port.Pos.X - 2.  //- (float fontSize * float port.Name.Length) 
                        let anchor = if port.Port.PortType = CommonTypes.PortType.Input then
                                            "left"
                                        else
                                           "end"

                        let textYPos = port.Pos.Y + (port.Height/2.) 
                        X (textXPos); 
                        Y (textYPos); 
                        Style [
                            TextAnchor anchor // left/right/middle: horizontal algnment vs (X,Y)
                            DominantBaseline "middle" // auto/middle/hanging: vertical alignment vs (X,Y)
                            FontSize (string fontSize)
                            FontWeight "Bold"
                            Fill "Black" // demo font color
                        ]
                    ] [str <| sprintf "%s" port.Name]
                ]

            //let Porttext
               


            let portlistDraw = 
                List.append props.Symbol.Outputport props.Symbol.Inputport
                |> List.map drawport

            let all = match props.Symbol.SymbolType with
                      |CommonTypes.ComponentType.MergeWires -> List.concat portlistDraw
                                                               |>List.append (makeWireSymbolWires()) 
                      |CommonTypes.ComponentType.SplitWire i -> List.concat portlistDraw
                                                               |>List.append (makeWireSymbolWires()) 
                      |_->List.concat portlistDraw 
                          |> List.append makeBox
                
                //|> List.append drawEnablePortCircle
            //printfn "where it thinks x:%A y:%A h:%A w:%A" props.Symbol.Pos.X props.Symbol.Pos.Y props.Symbol.H props.Symbol.W
            //printfn "draw %A" boxCoordinates

            g   [ Style [ 
                    // the transform here does rotation, scaling, and translation
                    // the rotation and scaling happens with TransformOrigin as fixed point first
                    //TransformOrigin "0px 50px" // so that rotation is around centre of line
                    //Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " fX fY rotation scaleFactor )
                    ]
                
                ]  // g optional attributes in first list
                // use g svg element (SVG equivalent of div) to group any number of ReactElements into one.
                // use transform with scale and/or translate and/or rotate to transform group
                
                all
        
    )     






/// View function for symbol layer of SVG

(*
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = CommonTypes.ComponentId id} as circle) ->
        renderCircle 
            {
                Circle = circle
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList
*)

let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = CommonTypes.ComponentId id} as shape) ->
        renderSymbol
            {
                Symbol = shape
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList

//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)



/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"

/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth 
        (wId: CommonTypes.ConnectionId) 
        (outputPortNumber: int) 
        (inputPortWidths: int option list) : int option =
    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"



        
   
    



//interface function

let getBoundingBoxes  (model : Model) (mouseCoord: XYPos) : (CommonTypes.ComponentId * XYPos * XYPos) list =
    model
    |>List.map (fun symbol -> (symbol.Id, symbol.Pos, {X= symbol.Pos.X + symbol.W; Y = symbol.Pos.Y+symbol.H} ) )


let getPortType  (model : Model) (portID: CommonTypes.PortId) =
    let extractType (port:PortLocal) =
        port.Port.PortType

    findPortFromModel model portID
    |>extractType

let getPortCoords  (model : Model) (portID: CommonTypes.PortId) =
    let extractCoord (port:PortLocal) :XYPos =
        match port.ExtendedType with
        |In -> {X= port.Pos.X; Y= port.Pos.Y + (port.Height/2.)}
        |Out -> {X= port.Pos.X + port.Width; Y= port.Pos.Y + (port.Height/2.)}
        |En -> {X= port.Pos.X + (port.Width/2.); Y= port.Pos.Y + (port.Height)}
    findPortFromModel model portID
    |> extractCoord
    
let getPortWidth (model : Model) (portID : CommonTypes.PortId) : int =
    let extractWirewidth (port:PortLocal) =
        port.Wirewidth

    findPortFromModel model portID
    |>extractWirewidth




let getPortIds (model: Model) (symbolIds: CommonTypes.ComponentId) :  CommonTypes.PortId list =
    model
    |> List.tryFind (fun symbol -> symbol.Id = symbolIds)
    |> function
    | Some symbol -> List.append symbol.Inputport symbol.Outputport 
                    |> List.map (fun port -> CommonTypes.PortId (port.Port.Id))
    | None -> failwithf "Symbol %A not found"  symbolIds



let isPort  (model : Model) (portCoords: XYPos)  : (XYPos * CommonTypes.PortId) Option =
    

    let isCoordWithinSymbolPorts  (symbol:Symbol) = 
        List.append symbol.Inputport symbol.Outputport
        |> List.tryFind (fun port -> (isWithinBox port.Pos port.Height port.Width portCoords))

    model
    |> List.tryFind ( fun symbol -> match isCoordWithinSymbolPorts symbol with
                                    |Some port ->true
                                    |None->false  )                         
    |>function
    |Some(symbol) ->match isCoordWithinSymbolPorts symbol with
                    |Some port -> Some (port.Pos , (CommonTypes.PortId (port.Port.Id)) )
                    |None -> None 
    |None ->  None

//helper function

    

       
