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
let STD_HEIGHT = 30.
let HW_RATIO = 0.95
let RAD = 3.



/// PortInfo extends the CommonTypes.Port
///
/// Pos: XYPos of the port on the canvas
///
/// Port: The original Port type from CommonTypes.Port
///
/// Placement: Int indicating the position on that side the port is placed in going in ascending order where horizontals would be left->right, and verticals top->bottom
type Portinfo = 
    {
        Port: CommonTypes.Port
        NumWires: int
        Name : string
        Invert : bool
        slotPos : int
    }


///Symbol is unique for each component, and shares CommonTypes.ComponentId
///
///Two positions TopL, BotR completely define the shape (all shapes are rectangular)
type Symbol =
    {
        TopL: XYPos
        BotR: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        Type : CommonTypes.ComponentType
        Name : string
        Highlight : bool
        PortHighlight : bool
        PortMap : XYPos list
        PortList : Portinfo list
        Rotation : int

    }


type Model = Symbol list



//---------------------------------------------------------------------------//
//----------------------------Message Type-----------------------------------//
//---------------------------------------------------------------------------//


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
    | DeleteSymbol of sIdList:CommonTypes.ComponentId list
    | Highlight of sIdList: CommonTypes.ComponentId list
    | HighlightPorts of sId : CommonTypes.ComponentId
    | DragPort of sId : CommonTypes.ComponentId * pId : string * pagePos: XYPos
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface





//---------------------------------------------------------------------------//
//------------------------------helper functions-----------------------------//
//---------------------------------------------------------------------------//




///Takes a component type and returns the corresponding symbol label/title/name as a string
let typeToName (compType : CommonTypes.ComponentType) : string =
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
    | _ -> "" //For any buses/wires



///Returns a tuple of float = (Height, Width) from two coordinates
let getHW (botR : XYPos) (topL : XYPos) = (botR.Y - topL.Y, botR.X - topL.X)

///Finds the midpoint of two coordinates
let midXY (botR : XYPos) (topL : XYPos) : XYPos =
    let midY = (botR.Y + topL.Y) / 2.
    let midX = (botR.X + topL.X) / 2.
    {X = midX; Y = midY}

///Adds a float value onto an XYPos
let addXYVal (xy : XYPos) (n : float) : XYPos = {X = xy.X + n; Y = xy.Y + n}

///The number of possible ports on the top/bot side of the Symbol = Width / STD_HEIGHT
let numPortsHorizontal (sym : Symbol) : int = int((sym.BotR.X - sym.TopL.X)/STD_HEIGHT)

///The number of possible ports on the left/right side of the Symbol = Height / STD_HEIGHT
let numPortsVertical (sym : Symbol) : int = int((sym.BotR.Y - sym.TopL.Y)/STD_HEIGHT)

let posDiff a b = {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b = {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let absDiff a b = 
    let diff = (posDiff a b)
    diff.X + diff.Y

///Snaps the rotation to one of: 0, 90, 180, 270
let getRot (rot : int) : int =
    if rot >= 0 && rot < 45 then 0
    elif rot >= 45 && rot < 135 then 90
    elif rot >= 135 && rot < 225 then 180
    elif rot >= 225 && rot < 315 then 270
    elif rot >= 315 && rot < 360 then 0
    else 0


///Displace will move a port position _away_ from the box.
///
///For inverters call with positive n.
///For labels call with negative n.
let displace (n : float) (pos : XYPos) (sym : Symbol) : (float * float) =
    let x = 
        if pos.X = sym.TopL.X then (pos.X - n)
        elif pos.X = sym.BotR.X then (pos.X + n)
        else pos.X
    let y =
        if pos.Y = sym.TopL.Y then (pos.Y - n)
        elif pos.Y = sym.BotR.Y then (pos.Y + n)
        else pos.Y
    (x, y)


///Finds whether a coordinate is within a port's bounding box
let testBox (portPos : XYPos) (coord : XYPos) : bool =
    let Box = (addXYVal portPos -1., addXYVal portPos 1.);
    let topL = Box |> fst
    let botR = Box |> snd
    topL.X <= coord.X && topL.Y <= coord.Y && botR.X >= coord.X && botR.Y >= coord.Y

let testLabelBox (portPos : XYPos) (coord : XYPos) (sym : Symbol) : bool =
    let transl = displace -3. portPos sym
    testBox {X = fst(transl); Y = snd(transl)} coord

let portPos (i : int) (n : int) (topL : XYPos) (botR : XYPos) : XYPos = 
    let h = getHW botR topL |> fst
    let w = getHW botR topL |> snd
    let x = topL.X + (w * float(i + 1) / float(n + 1))
    let y = topL.Y + (h * float(i + 1) / float(n + 1))
    {X = x; Y = y}
   
let findPos (port : Portinfo) (portMap : XYPos list) : XYPos =
    List.item port.slotPos portMap
//---------------------------------------------------------------------------//
//----------------------helper initialisation funcs--------------------------//
//---------------------------------------------------------------------------//



/// Creates Symbol.PortInfo object. 
///
/// i : Index of the port (e.g. INPUT1 : i = 0).
/// port : the Port from commontypes to convert.
/// topL : the top left of the symbol associated with the port.
/// botR : the bottom right of the symbol associated with the port.
/// n : the total number of ports on the symbol associated with the port.
let CreatePortInfo (i : int) (portType : CommonTypes.PortType) (topL : XYPos) (botR : XYPos) (n : int) (compId : CommonTypes.ComponentId) (compType : CommonTypes.ComponentType) : Portinfo = 
    
    //Intermediate calculations needed
        

    let pos =
        match portType with
        | CommonTypes.PortType.Input -> { X = topL.X; Y = (portPos i n topL botR).Y};
        | CommonTypes.PortType.Output -> { X = botR.X; Y = (portPos i n topL botR).Y};
    

    //Object creation
    {   
                 
        Port = {
            Id = Helpers.uuid();
            PortNumber = Some i
            PortType = portType
            HostId = string(compId)
        };

        NumWires = 0;
        
        Name = 
            match portType with
            | CommonTypes.PortType.Input -> sprintf "IN";
            | CommonTypes.PortType.Output -> sprintf "OUT";
        
        Invert = 
            match portType with
            | CommonTypes.PortType.Output when compType = CommonTypes.ComponentType.Not
                                            || compType = CommonTypes.ComponentType.Nand
                                            || compType = CommonTypes.ComponentType.Nor
                                            || compType = CommonTypes.ComponentType.Xnor -> true;
            | _ -> false;
        
        
        slotPos = i

    }




///Creates a new object of type symbol from component type, position, number of inputs, and number of outputs
let CreateNewSymbol (compType : CommonTypes.ComponentType) (numIn : int) (numOut : int) (pos : XYPos) : Symbol =
    //Intermediate calculations

    let n = List.max[numIn; numOut] |> float
    let h = STD_HEIGHT * n
    let w = HW_RATIO * h
    let botR = {X = pos.X + w; Y = pos.Y + h}
    
    let _id = CommonTypes.ComponentId (Helpers.uuid())

    //Making slot position list
    let l = 
        [0..int(n) - 1]
        |> List.map (fun i -> {X = pos.X; Y = (portPos i (int(n)) pos botR).Y})
    let r = 
        [0..int(n) - 1]
        |> List.map (fun i -> {X = botR.X; Y = (portPos i (int(n)) pos botR).Y})
    let t = 
        [0..int(HW_RATIO * n) - 1]
        |> List.map (fun i -> {X = (portPos i (int(n)) pos botR).X; Y = pos.Y})
    let b = 
        [0..int(HW_RATIO * n) - 1]
        |> List.map (fun i -> {X = (portPos i (int(n)) pos botR).X; Y = botR.Y})
    let slots = List.concat [l; r; t; b]
    let ins = 
        [0..numIn - 1]
        |> List.map(fun x -> CreatePortInfo x CommonTypes.PortType.Input pos botR numIn _id compType)
    let outs = 
        [numIn..numOut + numIn - 1]
        |> List.map(fun x -> CreatePortInfo x CommonTypes.PortType.Output pos botR numOut _id compType)
    let inOut = List.append ins outs
    //Symbol Creation
    {
        TopL = pos;
        BotR = botR;
        LastDragPos = {X = 0.; Y = 0.};
        IsDragging = false;
        Id = _id
        Type = compType
        Name = typeToName compType
        Highlight = false
        PortHighlight = false
        PortMap = slots
        PortList = inOut
        Rotation = 180
    }


//-----------------------Skeleton Message type for symbols---------------------//


/// Dummy function for test. Creates 4 NAND gates with 2 input, 1 output.
let init () =
    List.allPairs [1..2] [1..2]
    |> List.map (fun (x,y) -> {X = float (x*64+30); Y=float (y*64+30)})
    |> List.map (fun pos -> (CreateNewSymbol CommonTypes.ComponentType.Nand 2 1 pos)) 
    , Cmd.none


/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol (compType, pagePos, numIn, numOut) ->
        (CreateNewSymbol compType numIn numOut pagePos) :: model, Cmd.none

    | DeleteSymbol sIdList -> 
        List.filter (fun sym -> List.contains sym.Id sIdList = false) model, Cmd.none

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
                    PortMap =
                        sym.PortMap
                        |> List.map (fun x -> posAdd x diff)
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

    | Highlight sIdList ->
        model
        |> List.map (fun sym ->
            if List.contains sym.Id sIdList then 
                { sym with
                    Highlight = true
                }
            else
                { sym with
                    Highlight = false 
                }
        )
        , Cmd.none

    | HighlightPorts sId ->
        model
        |> List.map (fun sym ->
            if sym.Id = sId then 
                { sym with
                    PortHighlight = true
                }
            else
                { sym with
                    PortHighlight = false 
                }
        )
        , Cmd.none

    | DragPort (sId, pId, pagePos) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                let diff = pagePos
                { sym with
                    PortList =
                        let PortIndx = List.indexed sym.PortList

                        //The port we are moving
                        let port = List.find (fun x -> string(x.Port.Id) = pId) sym.PortList

                        //The index we want to move the port to is the one closest to the mouse
                        let i = 
                            List.map (fun x -> absDiff pagePos x) sym.PortMap
                            |> List.indexed
                            |> List.minBy (snd)
                            |> fst
                        
                        //If some element in PortList x  has x.slotPos = i
                        //Swap x and port which means:
                        //Make x.slotPos = port.slotPos
                        
                        sym.PortList
                        |> List.tryFind (fun x -> x.slotPos = i)
                        |> function
                        | Some x -> sym.PortList
                                    |> List.filter (fun v -> v <> x || v <> port)
                                    |> List.append [{x with slotPos = port.slotPos}]
                                    |> List.append [{port with slotPos = i}]

                        | None ->   sym.PortList
                                    |> List.filter (fun v -> v <> port)
                                    |> List.append [{port with slotPos = i}]
                }
        )
        , Cmd.none
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags
    | _ -> failwithf "Not implemented"



//---------------------------------------------------------------------------------//
//----------------------------View Function for Symbols----------------------------//
//---------------------------------------------------------------------------------//


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
    FunctionComponent.Of( //TODO - THIS NEEDS CHANGING WHENEVER MOVE GETS SORTED OUT
        fun (props : RenderObjProps) ->
            let handleMouseMove =
                Hooks.useRef(fun (ev : Types.Event) ->
                    let ev = ev :?> Types.MouseEvent
                    // x,y coordinates here do not compensate for transform in Sheet
                    // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                    Dragging(props.Obj.Id, posOf ev.pageX ev.pageY)
                    |> props.Dispatch
                )
            
            ///TODO - Once Move has been sorted out we need to correct this, fine for now
            let color =
                if props.Obj.IsDragging || props.Obj.Highlight then
                    "lightblue"
                else
                    "gainsboro"

            

            let labels : ReactElement list = 
                props.Obj.PortList
                |> List.map(fun i ->
                    text[
                        X ((displace -10. (findPos i props.Obj.PortMap) props.Obj) |> fst)
                        Y ((displace -10. (findPos i props.Obj.PortMap) props.Obj)  |> snd)
                        SVGAttr.Transform (sprintf "rotate (-%d, %d, %d)" props.Obj.Rotation (int ((displace -10. (findPos i props.Obj.PortMap) props.Obj) |> fst)) (int ((displace -10. (findPos i props.Obj.PortMap) props.Obj)  |> snd)))
                        
                        Style[
                            TextAnchor "middle"
                            DominantBaseline "middle"
                            FontSize "6px"
                            FontWeight "bold"
                            Fill "Black"
                        ]
                    ][str <| sprintf "%s" i.Name])

            let displayBox : ReactElement list =
                [
                    rect[
                        X props.Obj.TopL.X
                        Y props.Obj.TopL.Y
                        Rx 0.75
                        Ry 0.75
                        SVGAttr.Height ((getHW props.Obj.BotR props.Obj.TopL) |> fst)
                        SVGAttr.Width ((getHW props.Obj.BotR props.Obj.TopL) |> snd)
                        SVGAttr.Fill color
                        SVGAttr.Stroke "black"
                        SVGAttr.StrokeWidth 0.5][]

                    text[
                        X ((midXY props.Obj.BotR props.Obj.TopL).X)
                        Y ((midXY props.Obj.BotR props.Obj.TopL).Y)
                        SVGAttr.Transform (sprintf "rotate (-%d, %d, %d)" props.Obj.Rotation (int ((midXY props.Obj.BotR props.Obj.TopL).X)) (int ((midXY props.Obj.BotR props.Obj.TopL).Y)))
                        Style[
                            TextAnchor "middle"
                            DominantBaseline "middle"
                            FontSize "10px"
                            FontWeight "bold"
                            Fill "Black"
                            
                        ]
                    ][str <| sprintf "%A" props.Obj.Name]
                ]
            
            let drawInvert =
                props.Obj.PortList
                |> List.filter(fun x -> x.Invert = true)
                |> List.map(fun i ->
                    circle[
                        Cx ((displace 3. (findPos i props.Obj.PortMap) props.Obj) |> fst)
                        Cy ((displace 3. (findPos i props.Obj.PortMap) props.Obj) |> snd)
                        R RAD
                        SVGAttr.Fill color
                        SVGAttr.Stroke "black"
                        SVGAttr.StrokeWidth 0.5][])

            let ports =
                if props.Obj.PortHighlight then
                    props.Obj.PortList
                    |> List.map(fun i ->
                        circle[
                            Cx ((displace 3. (findPos i props.Obj.PortMap) props.Obj) |> fst)
                            Cy ((displace 3. (findPos i props.Obj.PortMap) props.Obj) |> snd)
                            R RAD
                            
                            SVGAttr.Fill "blue"
                            SVGAttr.Stroke "blue"
                            SVGAttr.Opacity 0.5
                            SVGAttr.StrokeWidth 1][])

                else
                    []
            
            
            g   [ //TODO UPDATE THIS MOVEMENT WHEN DECIDED PROPERLY
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

                    SVGAttr.Transform (sprintf "rotate (%d, %d, %d)" props.Obj.Rotation (int ((midXY props.Obj.BotR props.Obj.TopL).X)) (int ((midXY props.Obj.BotR props.Obj.TopL).Y)))
                    
                    
                    
            ](List.concat [displayBox; labels; drawInvert; ports])
            
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
    |> List.map(fun sym -> sym.PortList)
    |> List.concat

//Tries to find port object by portID, returns Some(portInfo) if found, else None
let portSearchID (symModel: Model) (pId : string) : Portinfo Option =
    initPortSearch symModel
    |> List.tryFind (fun port -> string(port.Port.Id) = pId)






//---------------Other interface functions--------------------//

//TODO - REMOVE - ONLY USED BY BUSWIRE - Currently connects every input 0 to each other.
let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> (findPos sym.PortList.[0] sym.PortMap))

///Searches through the whole model until the port is found and retruns the position of that port
///This would be much faster if sheet gave me the component
let getPortCoords (symModel: Model) (pId : string) : XYPos = 
    symModel
    |> List.tryFind(fun sym -> List.contains pId (List.map (fun x -> string(x)) sym.PortList))
    |> function
    | Some sym -> (findPos (List.find (fun x -> string(x.Port.Id) = pId) sym.PortList) sym.PortMap)
    | None -> failwithf "ERROR: Couldn't find port"

    //|> List.tryFind (fun port -> string(port.Port.Id) = pId)
        
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
    //testBox takes portPos and coord
    //for each symbol in model
    //for each element in symbol.PortMap
    //index list -> (i, v)
    //testBox fst coord
    //if hit then find symbol.portList where el = slotPos
    
    symModel
    |> List.tryFind (fun sym -> List.exists(fun v -> testBox v pos) sym.PortMap)
    |> function
    | Some sym -> let coordIndx = 
                        List.indexed sym.PortMap
                        |> List.find(fun (i, v) -> testBox v pos)
                  let port = List.find (fun x -> x.slotPos = (coordIndx |> fst)) sym.PortList
                  Some((coordIndx |> snd), string(port.Port.Id))
    | None -> None


//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"
