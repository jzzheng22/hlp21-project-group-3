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
        width : int
    }

type SymbolType =
    | LogMem
    | Mux
    | IO
    | Wires
    | Adder
    | FF

type genericPort =
    | InOut
    | Select
    | Carry
    | Enable

///Symbol is unique for each component, and shares CommonTypes.ComponentId
///
///Two positions TopL, BotR completely define the shape (all shapes are rectangular)
type Symbol =
    {
        TopL: XYPos
        BotR: XYPos
        Id : CommonTypes.ComponentId
        Type : CommonTypes.ComponentType
        Name : string
        Highlight : bool
        PortHighlight : bool
        PortMap : XYPos list
        PortList : Portinfo list
        Rotation : int
        Scale : XYPos
        genericType : SymbolType
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
    | Move of sIDList : CommonTypes.ComponentId list * pagePos : XYPos
    | Add of compType: CommonTypes.ComponentType * pagePos : XYPos * numIn : int * numOut : int
    | Delete of sIdList : CommonTypes.ComponentId list
    | Highlight of sIdList: CommonTypes.ComponentId list
    | HighlightPorts of sId : CommonTypes.ComponentId
    | DragPort of sId : CommonTypes.ComponentId * pId : string * pagePos: XYPos
    | Rotate of sId : CommonTypes.ComponentId * rot : int
    | Scale of sId : CommonTypes.ComponentId * scale : XYPos //can make this a tuple of (x, y) or a mouse coordinate instead quite easily
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface





//---------------------------------------------------------------------------//
//------------------------------helper functions-----------------------------//
//---------------------------------------------------------------------------//




///Takes a component type and returns info (label, buswidth in, buswidth out, generic symbol type)
let typeToInfo (compType : CommonTypes.ComponentType) : (string * int * int * SymbolType) =
    match compType with
    | CommonTypes.ComponentType.Constant (x, y) -> ((string(y)), x, x, LogMem) //This is a buffer right?
    | CommonTypes.ComponentType.Not -> ("1", 1, 1, LogMem)
    | CommonTypes.ComponentType.And -> ("&", 1, 1, LogMem)
    | CommonTypes.ComponentType.Or -> (">=", 1, 1, LogMem)
    | CommonTypes.ComponentType.Xor -> ("=1", 1, 1, LogMem)
    | CommonTypes.ComponentType.Nand -> ("&", 1, 1, LogMem)
    | CommonTypes.ComponentType.Nor -> ("|", 1, 1, LogMem)
    | CommonTypes.ComponentType.Xnor -> ("=1", 1, 1, LogMem)
    | CommonTypes.ComponentType.Decode4 -> ("Decode", 1, 1, LogMem) //Check generic type
    | CommonTypes.ComponentType.Mux2 -> ("MUX", 1, 1, Mux)
    | CommonTypes.ComponentType.Demux2 -> ("DMUX" , 1, 1, Mux)
    | CommonTypes.ComponentType.NbitsAdder x -> ("Σ", x, x, Adder) //TODO CHECK
    | CommonTypes.ComponentType.Custom x -> (x.Name, 1, 1, LogMem)
    | CommonTypes.ComponentType.DFF -> ("DFF", 1, 1, LogMem)
    | CommonTypes.ComponentType.DFFE -> ("DFFE", 1, 1, FF)
    | CommonTypes.ComponentType.Register x -> ("SRG", x, x, LogMem) //Check generic type
    | CommonTypes.ComponentType.RegisterE x -> ("SRGE", x, x, FF) //Check generic type
    | CommonTypes.ComponentType.AsyncROM x -> ("A/ROM", x.AddressWidth, x.WordWidth, LogMem)
    | CommonTypes.ComponentType.ROM x -> ("ROM", x.AddressWidth, x.WordWidth, LogMem)
    | CommonTypes.ComponentType.RAM x -> ("RAM", x.AddressWidth, x.WordWidth, LogMem)
    | CommonTypes.ComponentType.Input x -> ("IN", x, x, IO) //For any buses/wires
    | CommonTypes.ComponentType.Output x -> ("OUT", x, x, IO) 
    | CommonTypes.ComponentType.IOLabel -> ("", 0, 0, IO) //Check generic type WHAT IS THIS?? 
    | CommonTypes.ComponentType.BusSelection (x, y) -> ("", x, y, Wires)
    | CommonTypes.ComponentType.MergeWires -> ("", 0, 0, Wires)
    | CommonTypes.ComponentType.SplitWire x -> ("", x, x, Wires)

///Returns a tuple of float = (Height, Width) from two coordinates
let getHW (botR : XYPos) (topL : XYPos) = (botR.Y - topL.Y, botR.X - topL.X)

///Finds the midpoint of two coordinates
let midXY (botR : XYPos) (topL : XYPos) : XYPos =
    let midY = (botR.Y + topL.Y) / 2.
    let midX = (botR.X + topL.X) / 2.
    {X = midX; Y = midY}

let midSym (sym : Symbol) : XYPos = midXY sym.BotR sym.TopL

let midSymX (sym : Symbol) : float = (midSym sym).X 

let midSymY (sym : Symbol) : float = (midSym sym).Y

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
    testBox {X = fst transl; Y = snd transl} coord

let portPos (i : int) (n : int) (topL : XYPos) (botR : XYPos) : XYPos = 
    let h = getHW botR topL |> fst
    let w = getHW botR topL |> snd
    let x = topL.X + (w * float(i + 1) / float(n + 1))
    let y = topL.Y + (h * float(i + 1) / float(n + 1))
    {X = x; Y = y}
   
let findPos (port : Portinfo) (portMap : XYPos list) : XYPos =
    List.item port.slotPos portMap

///Snaps the rotation to one of: 0, 90, 180, 270
let getRot (rot : int) : int =
    if rot >= 0 && rot < 45 then 0
    elif rot >= 45 && rot < 135 then 90
    elif rot >= 135 && rot < 225 then 180
    elif rot >= 225 && rot < 315 then 270
    elif rot >= 315 && rot < 360 then 0
    else 0

let getRotMat (rot : int) : float list list=
    match getRot rot with
    | 90 -> [[0.; -1.; 0.]; [1.; 0.; 0.]; [0.; 0.; 1.]]
    | 180 -> [[-1.; 0.; 0.]; [0.; -1.; 0.]; [0.; 0.; 1.]]
    | 270 -> [[0.; 1.; 0.]; [-1.; 0.; 0.]; [0.; 0.; 1.]]
    | _ -> [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 0.; 1.]] //rotate 0 degrees


let delHead (m : float list list) : float list list = List.map List.tail m

let getCol (m : float list list) : float list = List.map List.head m

///Multiplies two lists together by element, and sums the result
let mulList (m1 : float list) (m2 : float list) : float = List.zip m1 m2 |> List.sumBy (fun (a, b) -> a*b)

let rec transp (m : float list list) : float list list =
    match m with
    | [] -> failwithf "error"
    | []::tl -> []
    | tl -> getCol tl :: transp (delHead tl)

let getPairs (m1 : float list) (m2 : float list list) : (float list * float list) list =
    List.map (fun x -> (m1, x)) m2


let multMatrix (m1 : float list list) (m2 : float list list) : float list list  =
    let transM = transp m2
    m1 |> List.map (fun x -> getPairs x transM) |> List.map (List.map(fun (x, y) -> mulList x y))
    
//I believe this rotation is working anticlockwise - check
let transCoords (coord : XYPos) (centre : XYPos) (trans : float list list) : XYPos =
    let coordM = [[coord.X]; [coord.Y]; [1.]]
    let transNve = [[1.; 0.; -centre.X]; [0.; 1.; -centre.Y]; [0.; 0.; 1.]]
    let transPve = [[1.; 0.; centre.X]; [0.; 1.; centre.Y]; [0.; 0.; 1.]]
    let transform = multMatrix transPve ( multMatrix trans ( multMatrix transNve coordM) )
    {X = transform.[0].[0]; Y = transform.[1].[0]}

//I believe this rotation is working anticlockwise - check
let rotateCoords (coord : XYPos) (rot : int) (centre : XYPos) : XYPos =
    transCoords coord centre (getRotMat(rot))

let scaleCoords (coord : XYPos) (scale : XYPos) (centre : XYPos) : XYPos =
    transCoords coord centre [[scale.X; 0.; 0.]; [0.; scale.Y; 0.]; [0.; 0.; 1.]]

let getNewBox (topL : XYPos) (botR : XYPos) : (XYPos * XYPos) =
    let newTopL = {X = min topL.X botR.X; Y = min topL.Y botR.Y}
    let newBotR = {X = max topL.X botR.X; Y = max topL.Y botR.Y}
    (newTopL, newBotR)

//Generic transformation function that takes in a transformation function, symbol and transformation
let trans func sym trans =
    let centre = midSym sym
    let rotTopL = func sym.TopL trans centre
    let rotBotR = func sym.BotR trans centre
    let newBox = getNewBox rotTopL rotBotR
    { sym with
        TopL = newBox |> fst
        BotR = newBox |> snd
        PortMap = sym.PortMap |> List.map (fun x -> func x trans centre)
    }

///Finds the log base 2 of an int and rounds up to the nearest int
let log2 (n : int) : int =
    (log(float n) / log(2.)) |> ceil |> int

let displaceN (sym : Symbol) (i : Portinfo) (n : float) : (float * float) =
    displace n (findPos i sym.PortMap) sym

let displaceNX (sym : Symbol) (i : Portinfo) (n : float) : float = displaceN sym i n |> fst

let displaceNY (sym : Symbol) (i : Portinfo) (n : float) : float = displaceN sym i n |> snd


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
let CreatePortInfo (i : int) (num : int) (portType : CommonTypes.PortType) (genPort : genericPort) (topL : XYPos) (botR : XYPos) (compId : CommonTypes.ComponentId) (compType : CommonTypes.ComponentType) (w : int) : Portinfo = 
    //Object creation
    {      
        Port = {
            Id = Helpers.uuid()
            PortNumber = Some i
            PortType =  portType
            HostId = string(compId)
        }

        NumWires = 0
        
        Name = 
            match genPort with
            | Select -> sprintf "S%d" num
            | Carry -> match portType with 
                       | CommonTypes.PortType.Input -> sprintf "Cin" 
                       | CommonTypes.PortType.Output -> sprintf "Cout" 
            | Enable -> sprintf "En"
            | _ -> match portType with 
                    | CommonTypes.PortType.Input -> sprintf "IN %d" num
                    | CommonTypes.PortType.Output -> sprintf "OUT %d" num 

        Invert = 
            match portType with
            | CommonTypes.PortType.Output when compType = CommonTypes.ComponentType.Not
                                            || compType = CommonTypes.ComponentType.Nand
                                            || compType = CommonTypes.ComponentType.Nor
                                            || compType = CommonTypes.ComponentType.Xnor -> true;
            | _ -> false;
        
        slotPos = i
        width = w
    }


let makePort (offset : int) (range : int) (topL : XYPos) (botR : XYPos) (port : CommonTypes.PortType) (_id : CommonTypes.ComponentId) (compType : CommonTypes.ComponentType) (width : int) (pType : genericPort) = 
    List.map(fun x -> CreatePortInfo (x + offset) x port pType topL botR _id compType width) [0 .. range - 1]

///Creates a new object of type symbol from component type, position, number of inputs, and number of outputs
let CreateNewSymbol (compType : CommonTypes.ComponentType) (numIn : int) (numOut : int) (pos : XYPos) : Symbol =
    
    //Getting type info for symbol/port construction
    let info = typeToInfo compType
    let (name, wIn, wOut, symType) = info

    let (left, right, bot) = //no symbols require extra ports on the top
        match symType with
        | Mux -> (0, 0, log2 numIn)
        | Adder -> (1, 1, 0)
        | FF -> (1, 0, 0)
        | _ -> (0, 0, 0) //Logic, memory, wires, IO do not require extra ports

    //Intermediate calculations
    let n = max (numIn + left) (numOut + right) |> float
    let h = STD_HEIGHT * n
    let w = HW_RATIO * h
    let botR = {X = pos.X + w; Y = pos.Y + h}
    
    let _id = CommonTypes.ComponentId (Helpers.uuid())

    // ---- Making portMap ---- //

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

    // ---- Making symbol's ports ---- //

    //First make the generic input output ports with labels IN0...INn,  OUT0 .. OUTn
    let ins = makePort 0 numIn pos botR CommonTypes.PortType.Input _id compType wIn InOut
    let outs = makePort (n |> int) numOut pos botR CommonTypes.PortType.Output _id compType wOut InOut

    //Calculate the index required for any component specific ports - for left ports index starts from numIn:
    let posRight = numIn + left + numOut 
    let posBot = List.length l + List.length r + List.length t

    //Create any component specific ports
    let (leftPort, rightPort, topPort) =
        match symType with
        | Mux ->  ([], [], makePort posBot bot pos botR CommonTypes.PortType.Input _id compType wIn Select)
        | Adder -> ((makePort numIn left pos botR CommonTypes.PortType.Input _id compType wIn Carry), (makePort posRight right pos botR CommonTypes.PortType.Output _id compType wOut Carry), [])
        | FF -> ((makePort numIn left pos botR CommonTypes.PortType.Input _id compType wIn Enable), [], [])
        | _ -> ([],[],[])
    
    //Concatenate the generic IN/OUT ports and the component specifc ports to get the full symbol portList
    

    //---------------------------------------------------------------------------------------------//
    //----FOR DEMO PURPOSES ONLY - THIS IS AN EXACT COPY OF THE TRANS FUNCTION USED IN MESSAGES----//
    //---------------------------------------------------------------------------------------------//
    let centre = midXY pos botR
    let rot  = 0
    let rotTopL = rotateCoords pos rot centre
    let rotBotR = rotateCoords botR rot centre
    let rotSlots = slots |> List.map (fun x -> rotateCoords x rot centre)

    let scale = {X = 1.0; Y = 1.0}
    let scaleTopL = scaleCoords rotTopL scale centre
    let scaleBotR = scaleCoords rotBotR scale centre
    let scaleSlots = rotSlots |> List.map (fun x -> scaleCoords x scale centre)
    let newBox = getNewBox scaleTopL scaleBotR

    //---------------------------------------------------------------------------------------------//
    //---------------------------------------------------------------------------------------------//
    //---------------------------------------------------------------------------------------------//

    // ------- Symbol Creation ------ ///
    {
        TopL = newBox |> fst //only the demo for rotation/scaling, without demo: TopL = pos, and BotR = botR.
        BotR = newBox |> snd
        Id = _id
        Type = compType
        Name = name
        Highlight = false
        PortHighlight = false
        PortMap = scaleSlots //only the demo for rotation/scaling, without demo: PortMap = slots.
        PortList = List.concat [ins; leftPort; outs; rightPort; topPort]
        Rotation = rot
        Scale = scale
        genericType = symType
    }


//-----------------------Skeleton Message type for symbols---------------------//


/// Dummy function for test. Creates 4 NAND gates with 2 input, 1 output.
let init () =
    List.allPairs [1..2] [1..2]
    |> List.map (fun (x,y) -> {X = float (x*64+30); Y=float (y*64+30)})
    |> List.map (fun pos -> (CreateNewSymbol (CommonTypes.ComponentType.Mux2) 2 1 pos)) 
    , Cmd.none


/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | Add (compType, pagePos, numIn, numOut) ->
        (CreateNewSymbol compType numIn numOut pagePos) :: model, Cmd.none

    | Delete sIdList -> 
        List.filter (fun sym -> List.contains sym.Id sIdList = false) model, Cmd.none

    | Move (compList, translate) ->
        model
        |> List.map (fun sym ->
            if List.contains sym.Id compList then
                { sym with
                    TopL = posAdd sym.TopL translate
                    BotR = posAdd sym.BotR translate
                    PortMap = sym.PortMap |> List.map (fun x -> posAdd x translate)
                }
            else
                sym
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
                        
                        //If some element in PortList x has slotPos = i (i.e. it is inside the position we want to move the port to)
                        //Swap x and port (i.e. Make x.slotPos = port.slotPos)
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

        | Rotate (sId, rot) ->
            model
            |> List.map (fun sym ->
                if sId <> sym.Id then
                    sym
                else
                    trans rotateCoords sym rot
        )
        , Cmd.none

        | Scale (sId, scale) ->
            model
            |> List.map (fun sym ->
                if sId <> sym.Id then
                    sym
                else
                    trans scaleCoords sym scale
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

            let color =
                match props.Obj.genericType with
                | Wires -> if props.Obj.Highlight then
                                "red"
                            else
                                "darkgrey"
                | _ -> if props.Obj.Highlight then
                                "lightblue"
                            else
                                "gainsboro"
                
            let labels : ReactElement list = 
                props.Obj.PortList
                |> List.map(fun i ->
                    text[
                        X (displaceNX props.Obj i -10.)
                        Y (displaceNY props.Obj i -10.)
                        Style[
                            TextAnchor "middle"
                            DominantBaseline "middle"
                            FontSize "6px"
                            FontWeight "bold"
                            Fill "Black"
                        ]
                    ][str <| sprintf "%s" i.Name])

            let wires : ReactElement list =
            //line should be (port.x, port.y), (mid.x, port.y), (mid.x, mid.y)
                props.Obj.PortList
                |> List.map(fun i ->
                    polyline[
                        Points (sprintf "%f,%f %f,%f %f,%f" ((findPos i props.Obj.PortMap).X) ((findPos i props.Obj.PortMap).Y) (midSymX props.Obj) ((findPos i props.Obj.PortMap).Y) (midSymX props.Obj) (midSymY props.Obj))
                        Style[
                            Stroke color
                            Fill "none"
                        ]
                    ][])

            let triangles : ReactElement list =
                //line should be (port.x, port.y), (mid.x, port.y), (mid.x, mid.y)
                    props.Obj.PortList
                    |> List.map(fun i ->
                        polygon[
                            Points (sprintf "%f,%f %f,%f %f,%f" ((findPos i props.Obj.PortMap).X) ((findPos i props.Obj.PortMap).Y + RAD) ((findPos i props.Obj.PortMap).X + RAD) ((findPos i props.Obj.PortMap).Y) ((findPos i props.Obj.PortMap).X) ((findPos i props.Obj.PortMap).Y - RAD))
                            Style[
                                Stroke color
                                Fill color
                            ]
                        ][])
            
            let io : ReactElement list =
                [
                    polygon[
                        Points (sprintf "%f,%f %f,%f %f,%f %f,%f %f,%f" (props.Obj.TopL.X) (midSymY props.Obj - RAD) props.Obj.TopL.X (midSymY props.Obj + RAD) (midSymX props.Obj + RAD) (midSymY props.Obj + RAD) (props.Obj.BotR.X) (midSymY props.Obj) (midSymX props.Obj + RAD) (midSymY props.Obj - RAD))
                        Style[
                            Stroke "black"
                            Fill color
                            StrokeWidth 0.5
                        ]
                    ][]
                ]

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
                        X (midSymX props.Obj)
                        Y (midSymY props.Obj)
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
                        Cx (displaceNX props.Obj i 3.)
                        Cy (displaceNY props.Obj i 3.)
                        R RAD
                        SVGAttr.Fill color
                        SVGAttr.Stroke "black"
                        SVGAttr.StrokeWidth 0.5][])

            let ports =
                if props.Obj.PortHighlight then
                    props.Obj.PortList
                    |> List.map(fun i ->
                        circle[
                            Cx (displaceNX props.Obj i 3.)
                            Cy (displaceNY props.Obj i 3.)
                            R RAD
                            
                            SVGAttr.Fill "deepskyblue"
                            SVGAttr.Stroke "deepskyblue"
                            SVGAttr.Opacity 0.4
                            SVGAttr.StrokeWidth 1][])

                else
                    []
            
            let symDraw = 
                match props.Obj.genericType with
                | Wires -> List.concat [wires; triangles]
                | IO -> io
                | _ -> displayBox
            
            g[](List.concat [symDraw; labels; drawInvert; ports])
            
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






//-----------------------Interface functions--------------------//

//TODO - REMOVE - ONLY USED BY BUSWIRE - Currently connects every input 0 to each other.
let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> (findPos sym.PortList.[1] sym.PortMap))

///Searches through the whole model until the port is found and retruns the position of that port
///This would be much faster if sheet gave me the component
let getPortCoords (symModel: Model) (pId : CommonTypes.PortId) : XYPos = 
    symModel
    |> List.tryFind(fun sym -> List.contains (string pId) (List.map (fun x -> x.Port.Id) sym.PortList))
    |> function
    | Some sym -> (findPos (List.find (fun x -> CommonTypes.PortId x.Port.Id = pId) sym.PortList) sym.PortMap)
    | None -> failwithf "ERROR: Couldn't find port"
        
///Returns all symbols in the model in the form (ID, bounding box topLeft, bounding box botRight)
let getBoundingBoxes (symModel : Model) (startCoord : XYPos) : (CommonTypes.ComponentId * XYPos * XYPos) list =
    symModel
    |> List.map (fun sym -> (sym.Id, sym.TopL, sym.BotR))

///Finds the portType of a specific port
let getPortType (symModel: Model) (pId : CommonTypes.PortId) : CommonTypes.PortType =
    portSearchID symModel (string pId)
    |> function
    | Some port -> port.Port.PortType
    | None -> failwithf "ERROR: Couldn't find port"

///Finds if a position lies on a port. Returns Some(position, portId) if found, none otherwise.
let isPort (symModel : Model) (pos : XYPos) : (XYPos * CommonTypes.PortId) Option =
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
                        |> List.find(fun (_, v) -> testBox v pos)
                  let port = List.find (fun x -> x.slotPos = (coordIndx |> fst)) sym.PortList
                  Some((coordIndx |> snd), CommonTypes.PortId port.Port.Id)
    | None -> None

//Returns a list of Port Ids for a given symbol
let getPortIds (model : Model) (sId : CommonTypes.ComponentId) : CommonTypes.PortId list = 
    model
    |> List.tryFind (fun sym -> sym.Id = sId)
    |> function
    | Some sym -> List.map (fun x -> CommonTypes.PortId x.Port.Id) sym.PortList
    | None -> failwithf "Error, could not find symbol"

let getPortWidth (model : Model) (pId : CommonTypes.PortId) : int =
    portSearchID model (string pId)
    |> function
    | Some port -> port.width
    | None -> failwithf "ERROR: Couldn't find port"

//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"
