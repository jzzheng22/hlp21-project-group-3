module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//

//Static variables
let CIRCLE_TO_RECT_RATIO = 0.2
let STD_HEIGHT = 20.
let HW_RATIO = 0.9


/// PortInfo extends the CommonTypes.Port
///
/// Pos: XYPos of the port on the canvas
///
/// Port: The original Port type from CommonTypes.Port
///
/// Placement: Int indicating the position on that side the port is placed in going in ascending order where horizontals would be left->right, and verticals top->bottom
type Portinfo = 
    {
        Pos: XYPos
        Port: CommonTypes.Port
        Placement: int 
        NumWires: int
        Name : string
        Invert : bool
        Box : XYPos * XYPos
    }

///Symbol is unique for each component, and shares CommonTypes.ComponentId
///
///Two positions TopL, BotR completely define the shape (all shapes are rectangular)
///
///Ports is a list of portinfo lists in the form [[left]; [top]; [right]; [bot]] such that other features such as port movement can be easily added in future
type Symbol =
    {
        TopL: XYPos
        BotR: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        Ports : Portinfo list
        Type : CommonTypes.ComponentType
        Name : string

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
    | AddSymbol of compType: CommonTypes.ComponentType * pagePos : XYPos * numIn : int * numOut : int
    | DeleteSymbol of sId:CommonTypes.ComponentId 
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface


//---------------------------------helper types and functions----------------//

//Was this necessary??? I have no idea

///Takes a component type and returns the corresponding symbol label/title/name as a string
let typeToName (compType : CommonTypes.ComponentType) =
    match compType with
    | CommonTypes.ComponentType.Constant _ -> "1" //This is a buffer right?
    | CommonTypes.ComponentType.Not -> "1"
    | CommonTypes.ComponentType.And -> "&"
    | CommonTypes.ComponentType.Or -> ">="
    | CommonTypes.ComponentType.Xor -> "=1"
    | CommonTypes.ComponentType.Nand -> "&"
    | CommonTypes.ComponentType.Nor -> "|"
    | CommonTypes.ComponentType.Xnor -> "=1"
    | CommonTypes.ComponentType.Decode4 -> "Decode"
    | CommonTypes.ComponentType.Mux2 -> "MUX"
    | CommonTypes.ComponentType.Demux2 -> "DMUX"
    | CommonTypes.ComponentType.NbitsAdder _ -> "Σ"
    | CommonTypes.ComponentType.Custom x -> x.Name
    | CommonTypes.ComponentType.DFF -> "DFF"
    | CommonTypes.ComponentType.DFFE -> "DFFE"
    | CommonTypes.ComponentType.Register _ -> "SRG"
    | CommonTypes.ComponentType.RegisterE _ -> "SRGE"
    | CommonTypes.ComponentType.AsyncROM _ -> "A/ROM"
    | CommonTypes.ComponentType.ROM _ -> "ROM"
    | CommonTypes.ComponentType.RAM _ -> "RAM"
    | _ -> failwithf "AHHH WHY AM I HERE?"


///Returns a tuple of float = (Height, Width)
///Inputs: Bottom right coordinate, top left coordinate
let getHW (botR : XYPos) (topL : XYPos) = 
    (botR.Y - topL.Y, botR.X - topL.X)

let midXY (botR : XYPos) (topL : XYPos) =
    let midY = (botR.Y + topL.Y) / 2.
    let midX = (botR.X + topL.X) / 2.
    (midX, midY)
   
let addXYVal (xy : XYPos) (n : float) : XYPos = 
    {X = xy.X + n; Y = xy.Y + n}
/// Creates Symbol.PortInfo object
///
/// i : Index of the port (e.g. INPUT1 : i = 0).
///
/// port : the Port from commontypes to convert 
///
/// topL : the top left of the symbol associated with the port
///
/// botR : the bottom right of the symbol associated with the port 
///
/// n : the total number of ports on the symbol associated with the port 
let CreatePortInfo (i : int) (portType : CommonTypes.PortType) topL botR (n : int) (compId : CommonTypes.ComponentId) (compType : CommonTypes.ComponentType) : Portinfo = 
    let h = getHW botR topL |> fst
    let portPosY = topL.Y + (h * float(i + 1) / float(n + 1))
    let pos =
        match portType with
        | CommonTypes.PortType.Input -> { X = topL.X; Y = portPosY};
        | CommonTypes.PortType.Output -> { X = botR.X; Y = portPosY};
    {   
        
        Pos = pos
            
        Port = {
            Id = Helpers.uuid();
            PortNumber = Some i
            PortType = portType
            HostId = string(compId)
        };
        Placement = i;
        NumWires = 0;
        Name = 
            match portType with
            | CommonTypes.PortType.Input -> sprintf "IN %i" i;
            | CommonTypes.PortType.Output -> sprintf "OUT %i" i;
        Invert = 
            match portType with
            | CommonTypes.PortType.Output when compType = CommonTypes.ComponentType.Not
                                            || compType = CommonTypes.ComponentType.Nand
                                            || compType = CommonTypes.ComponentType.Nor
                                            || compType = CommonTypes.ComponentType.Xnor -> true;
            | _ -> false;
        Box = (addXYVal pos -1., addXYVal pos 1.)
    }

///Creates a new object of type symbol from component type, position, number of inputs, and number of outputs
let CreateNewSymbol (compType : CommonTypes.ComponentType) (numIn : int) (numOut : int) (pos : XYPos) : Symbol =
    
    //Intermediate calculations

    let n = List.max[numIn; numOut] |> float
    let h = STD_HEIGHT * n
    let w = HW_RATIO * h
    
    let _id = CommonTypes.ComponentId (Helpers.uuid())
    let botR = {X = pos.X + w; Y = pos.Y + h}
    let Inputs = 
        [0..numIn - 1]
        |> List.map(fun x -> CreatePortInfo x CommonTypes.PortType.Input pos botR numIn _id compType)
    let Outputs = 
        [0..numOut - 1]
        |> List.map(fun x -> CreatePortInfo x CommonTypes.PortType.Output pos botR numOut _id compType)

    //Symbol Creation
    {
        TopL = pos;
        BotR = botR;
        LastDragPos = {X = 0.; Y = 0.};
        IsDragging = false;
        Id = _id
        Ports = List.append Inputs Outputs
        Type = compType
        Name = typeToName compType
            
    }

///The number of possible ports on the top/bot side of the Symbol = Width / STD_HEIGHT
let numPortsHorizontal (sym : Symbol) : int = int((sym.BotR.X - sym.TopL.X)/STD_HEIGHT)

///The number of possible ports on the left/right side of the Symbol = Height / STD_HEIGHT
let numPortsVertical (sym : Symbol) : int = int((sym.TopL.Y - sym.BotR.Y)/STD_HEIGHT)

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

///Finds whether a coordinate is within a port's bounding box
let testBox (port : Portinfo) (coord : XYPos) : bool =
    let topL = port.Box |> fst
    let botR = port.Box |> snd
    if topL.X <= coord.X && topL.Y <= coord.Y && botR.X >= coord.X && botR.Y >= coord.Y
    then true
    else false



//-----------------------------Skeleton Model Type for symbols----------------//




//-----------------------Skeleton Message type for symbols---------------------//



/// Dummy function for test. The real init would probably have no symbols.
let init () =
    List.allPairs [1..2] [1..2]
    |> List.map (fun (x,y) -> {X = float (x*64+30); Y=float (y*64+30)})
    |> List.map (fun pos -> (CreateNewSymbol CommonTypes.ComponentType.And 3 1 pos)) 
    , Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (compType, pagePos, numIn, numOut) ->
        (CreateNewSymbol compType numIn numOut pagePos) :: model, Cmd.none
    | DeleteSymbol sId -> 
        List.filter (fun sym -> sym.Id <> sId) model, Cmd.none
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
                    TopL = posAdd sym.TopL diff
                    BotR = posAdd sym.BotR diff
                    Ports = 
                        sym.Ports
                        |> List.map (fun y -> {y with Pos = posAdd y.Pos diff})
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
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags
    | _ -> failwithf "Not implemented"

//----------------------------View Function for Symbols----------------------------//

//Using the skeleton code for 'RenderCircle' as the generic parent rendering function for all blocks

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderObjProps =
    {
        Obj : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

/// View for one symbol with caching for efficient execution when input does not change
let private renderObj =
    FunctionComponent.Of(
        fun (props : RenderObjProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Obj.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )

            let color =
                if props.Obj.IsDragging then
                    "lightblue"
                else
                    "grey"

            let labels : ReactElement list = 
                props.Obj.Ports
                |> List.map(fun i ->
                    text[
                        X i.Pos.X
                        Y i.Pos.Y
                        Style[
                            TextAnchor "middle"
                            DominantBaseline "middle"
                            FontSize "5px"
                            FontWeight "bold"
                            Fill "Black"
                        ]
                    ][str <| sprintf "%s" i.Name])

            let displayBox : ReactElement list =
                [
                    rect[
                        X props.Obj.TopL.X
                        Y props.Obj.TopL.Y
                        SVGAttr.Height ((getHW props.Obj.BotR props.Obj.TopL) |> fst)
                        SVGAttr.Width ((getHW props.Obj.BotR props.Obj.TopL) |> snd)
                        SVGAttr.Fill color
                        SVGAttr.Stroke color
                        SVGAttr.StrokeWidth 1][]

                    text[
                        X ((midXY props.Obj.BotR props.Obj.TopL) |> fst)
                        Y ((midXY props.Obj.BotR props.Obj.TopL) |> snd)
                        Style[
                            TextAnchor "middle"
                            DominantBaseline "middle"
                            FontSize "5px"
                            FontWeight "bold"
                            Fill "Black"
                        ]
                    ][str <| sprintf "%A" props.Obj.Name]
                ]
            
            let drawInvert =
                props.Obj.Ports
                |> List.filter(fun x -> x.Invert = true)
                |> List.map(fun i ->
                    circle[
                        Cx i.Pos.X
                        Cy i.Pos.Y
                        R 5.
                        SVGAttr.Fill "blue"
                        SVGAttr.Stroke "blue"
                        SVGAttr.StrokeWidth 1][])
            
            
            g   [ 
                    OnMouseUp (fun ev -> 
                        document.removeEventListener("mousemove", handleMouseMove.current)
                        EndDragging props.Obj.Id
                        |> props.Dispatch
                    )
                    OnMouseDown (fun ev -> 
                        // See note above re coords wrong if zoom <> 1.0
                        StartDragging (props.Obj.Id, posOf ev.pageX ev.pageY)
                        |> props.Dispatch
                        document.addEventListener("mousemove", handleMouseMove.current)
                    )
            ](displayBox@labels@drawInvert)
            
    , "Circle"
    , equalsButFunctions
    )

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = CommonTypes.ComponentId id} as shape) ->
        renderObj 
            {
                Obj = shape
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList

//---------------Helpers for interface functions--------------------//

///An exhaustive search through the model, which returns the Portinfo object corresponding to an input string port ID
let initPortSearch (symModel: Model) : Portinfo list = 
    symModel
    |> List.map(fun sym -> sym.Ports)
    |> List.concat

let portSearchID (symModel: Model) (pId : string) : Portinfo Option =
    initPortSearch symModel
    |> List.tryFind (fun port -> string(port.Port.Id) = pId)

let portSearchPos (symModel: Model) (pos : XYPos) : Portinfo Option =
    initPortSearch symModel
    |> List.tryFind (fun port -> testBox port pos)

//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.TopL)

///Searches through the whole model until the port is found and retruns the position of that port
///This would be much faster if sheet gave me the component
let getPortCoords (symModel: Model) (pId : string) : XYPos = 
    portSearchID symModel pId
    |> function
    | Some port -> port.Pos
    | None -> failwithf "ERROR: Couldn't find port"
        
///Returns all symbols in the model in the form (ID, bounding box topLeft, bounding box botRight)
let getBoundingBoxes (symModel : Model) (startCoord : XYPos) : (string * XYPos * XYPos) list =
    symModel
    |> List.map (fun sym -> (string(sym.Id), sym.TopL, sym.BotR))

///Finds the portType of a specific port
let getPortType (symModel: Model) (pId : string) : CommonTypes.PortType =
    portSearchID symModel pId
    |> function
    | Some port -> port.Port.PortType
    | None -> failwithf "ERROR: Couldn't find port"

///Finds if a position lies on a port. Returns Some(position, portId) if found, none otherwise.
let isPort (symModel : Model) (pos : XYPos) : (XYPos * string) Option =
    portSearchPos symModel pos
    |> function
    | Some port -> Some(port.Pos, string(port.Port.Id))
    | None -> None
    

//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"
