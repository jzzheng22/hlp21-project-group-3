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
let STD_HEIGHT = 30.
let HW_RATIO = 0.75


///<summary> PortInfo extends the CommonTypes.Port
/// Pos: XYPos of the port on the canvas
/// Port: The original Port type from CommonTypes.Port
/// Orientation: Int indicating which side of the shape the port is placed in (0 - left, 1 - top, 2 - right, 3 - bot)
/// Placement: Int indicating the position on that side the port is placed in going in ascending order where horizontals would be left->right, and verticals top->bottom
type Portinfo = 
    {
        Pos: XYPos
        Port: CommonTypes.Port
        Orientation: int
        Placement: int 
        NumWires: int
        name : string
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
        TopL: XYPos
        BotR: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        Ports : Portinfo list

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
    | AddSymbol of compType: CommonTypes.ComponentType * pagePos : XYPos * numIn : int * numOut : int
    | DeleteSymbol of sId:CommonTypes.ComponentId 
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface


//---------------------------------helper types and functions----------------//

(*type Port = {
Id : string
PortNumber : int option
PortType : PortType
HostId : string
    }*)

/// <summary> Creates Symbol.PortInfo object </summary>
/// <param name = "i"> Index of the port (e.g. INPUT1 : i = 0).</param>
/// <param name = "port"> : the Port from commontypes to convert </param>
/// <param name = "topL"> : the top left of the symbol associated with the port </param>
/// <param name = "botR"> : the bottom right of the symbol associated with the port </param>
/// <param name = "n"> : the total number of ports on the symbol associated with the port </param>
let CreatePortInfo (i : int) (portType : CommonTypes.PortType) topL botR (n : int) (compId : CommonTypes.ComponentId) : Portinfo = 
    {   
        //Left, Top, Right, Bot
        Pos = 
            match portType with
            | Input -> { X = topL.X; Y = ((topL.Y - botR.Y) * float(i) / float(n)) };
            | Output -> { X = botR.X; Y = ((topL.Y - botR.Y) * float(i) / float(n)) };
        Port = {
            Id = Helpers.uuid();
            PortNumber = Some i
            PortType = portType
            HostId = string(compId)
        };
        Orientation = 
            match portType with
            | Input -> 0;
            | Output -> 2;
        Placement = i;
        NumWires = 0;
        name = 
            match portType with
            | Input -> sprintf "IN%i" i;
            | Output -> sprintf "OUT%i" i;
    }

(*type type Symbol =
{
    TopL: XYPos
    BotR: XYPos
    LastDragPos : XYPos
    IsDragging : bool
    Id : CommonTypes.ComponentId
    Ports : Portinfo list //[[left]; [top]; [right]; [bottom]]

}
*)

///Creates a new object of type symbol from component type, position, number of inputs, and number of outputs
let CreateNewSymbol (compType : CommonTypes.ComponentType) (pos : XYPos) (numIn : int) (numOut : int) : Symbol =
    let n = List.max[numIn; numOut] |> float
    let h = STD_HEIGHT * n
    let w = HW_RATIO * float(h)
    let _id = CommonTypes.ComponentId (Helpers.uuid())
    let botR = {X = pos.X - h; Y = pos.Y + w}
    let Inputs = 
        [0..numIn]
        |> List.map(fun x -> CreatePortInfo x CommonTypes.PortType.Input pos botR numIn _id)
    let Outputs = 
        [0..numOut]
        |> List.map(fun x -> CreatePortInfo x CommonTypes.PortType.Output pos botR numOut _id)

    {
        TopL = pos;
        BotR = botR;
        LastDragPos = {X = 0.; Y = 0.};
        IsDragging = false;
        Id = _id
        Ports = List.append Inputs Outputs
    }

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

///Returns an int indicating the side of the object that a port is on 0 = Left, 1 = Top, 2 = Right, 3 = Bottom
let findSide id listLabels = listLabels |> List.findIndex(List.contains id)

///Returns a tuple (side, index) indicating the index/position that the port lies in
let findPos id listLabels = 
    let i = listLabels |> findSide id 
    let j = listLabels |> List.item i |> List.findIndex((=) id)
    (i, j)

///Returns a react element for an inverter on a specific port
let drawInvert id listLabels =
    //index of outer position tells us whether port is left/top/right/bottom
    let side = findSide id listLabels
    let indx = findPos id listLabels
    //let pos = listLabels.[side].[indx]
    0


//-----------------------------Skeleton Model Type for symbols----------------//




//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (pos:XYPos) =
    let w = STD_HEIGHT * HW_RATIO
    {
        TopL = pos
        BotR = {X = pos.X + w; Y = pos.Y - STD_HEIGHT}
        LastDragPos = {X = 0. ; Y = 0.} // initial value can always be this
        IsDragging = false // initial value can always be this
        Id = CommonTypes.ComponentId (Helpers.uuid()) // create a unique id for this symbol
        Ports = []
    }


/// Dummy function for test. The real init would probably have no symbols.
let init () =
    List.allPairs [1..14] [1..14]
    |> List.map (fun (x,y) -> {X = float (x*64+30); Y=float (y*64+30)})
    |> List.map createNewSymbol
    , Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (compType, pagePos, numIn, numOut) ->
        (CreateNewSymbol compType pagePos numIn numOut) :: model, Cmd.none
    | AddCircle pos -> 
        (createNewSymbol pos) :: model, Cmd.none
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

            rect
                [ 
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
                    X props.Obj.TopL.X
                    Y props.Obj.TopL.Y
                    SVGAttr.Height (props.Obj.TopL.Y - props.Obj.BotR.Y)
                    SVGAttr.Width (-props.Obj.TopL.X + props.Obj.BotR.X)
                    SVGAttr.Fill color
                    SVGAttr.Stroke color
                    SVGAttr.StrokeWidth 1
                ]
                [ ]
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


//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.TopL)

let portPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.TopL)


/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    match List.tryFind (fun sym -> string(sym.Id) = comp.Id) symModel with
    | Some x -> x
    | None -> createNewSymbol ({X = 1.0; Y = 1.0}) //dummy for now


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
