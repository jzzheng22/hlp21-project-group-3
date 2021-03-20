module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//

//Static variables
let stdHeight = 35.
let ratioHW = 0.9
let radius = 3.
let labelSize = 6

type PortInfo = 
    {
        Port: Port
        NumWires: int
        Name : string
        Invert : bool
        Width : int
    }

type SymbolType =
    | LogMem
    | Mux
    | IO
    | Wires
    | Adder
    | FF
    | FFE
    | RAM

type GenericPort =
    | InOut
    | Select
    | Carry
    | Enable
    | Clk
    | Addr

type Edge = Top | Bottom | Left | Right

type Rotation = 
    | R0
    | R90
    | R180
    | R270

type Symbol =
    {
        TopL: XYPos
        BotR: XYPos
        Id : ComponentId
        Type : ComponentType
        Name : string
        Highlight : string
        PortHighlight : bool
        PortMap : Map<XYPos, PortInfo Option>
        GenericType : SymbolType
        ShowSlots : bool
        Index : int
        Label : string
        IOList : (Port list * Port list)
        Rotation : Rotation
        Scale : XYPos
    }

type SymbolAdd = {
    CompType : ComponentType
    PagePos: XYPos
    Input: int
    Output: int
    Index: int    
}

type Model = Symbol list

type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    | Move of sIDList : ComponentId list * pagePos : XYPos
    // | Add of compType: ComponentType * pagePos : XYPos * numIn : int * numOut : int * index : int 
    | Add of addMsg: SymbolAdd
    | Delete of sIdList : ComponentId list
    | Highlight of sIdList: ComponentId list
    | HighlightError of sIdList: ComponentId list
    | HighlightPorts of sIdList : ComponentId list
    | DragPort of sId : ComponentId * pId : PortId * pagePos: XYPos
    | Rotate of sId : ComponentId * rot : int
    | Scale of sId : ComponentId * scale : XYPos //can make this a tuple of (x, y) or a mouse coordinate instead quite easily
    | DisplaySlots of sId : ComponentId
    | Rename of sId : ComponentId * name : string
    | UpdateSymbolModelWithComponent of Component // Issie interface


/// Takes a component type and returns info (label, buswidth in, buswidth out, generic symbol type)
let typeToInfo (compType : ComponentType) : (string * string * int * int * SymbolType) =
    match compType with
    | ComponentType.Constant (x, y) -> ("C",(string(y)), x, x, LogMem) //This is a buffer right?
    | ComponentType.Not -> ("NOT", "1", 1, 1, LogMem)
    | ComponentType.And -> ("AND", "&", 1, 1, LogMem)
    | ComponentType.Or -> ("OR", ">=", 1, 1, LogMem)
    | ComponentType.Xor -> ("XOR", "=1", 1, 1, LogMem)
    | ComponentType.Nand -> ("NAND", "&", 1, 1, LogMem)
    | ComponentType.Nor -> ("NOR", "|", 1, 1, LogMem)
    | ComponentType.Xnor -> ("XNOR", "=1", 1, 1, LogMem)
    | ComponentType.Decode4 -> ("DECODE", "Decode", 1, 1, LogMem)
    | ComponentType.Mux2 -> ("MUX", "MUX", 1, 1, Mux)
    | ComponentType.Demux2 -> ("DEMUX", "DMUX" , 1, 1, Mux)
    | ComponentType.NbitsAdder x -> ("ADDER", "Σ", x, x, Adder)
    | ComponentType.Custom x -> ("CUSTOM", x.Name, 1, 1, LogMem)
    | ComponentType.DFF -> ("FF", "DFF", 1, 1, FF)
    | ComponentType.DFFE -> ("FFE", "DFFE", 1, 1, FFE)
    | ComponentType.Register x -> ("REG", "SRG", x, x, FF)
    | ComponentType.RegisterE x -> ("REGE", "SRGE", x, x, FFE) 
    | ComponentType.AsyncROM x -> ("AROM", "AROM", x.AddressWidth, x.WordWidth, LogMem)
    | ComponentType.ROM x -> ("ROM", "ROM", x.AddressWidth, x.WordWidth, FF)
    | ComponentType.RAM x -> ("RAM", "RAM", x.AddressWidth, x.WordWidth, RAM)
    | ComponentType.Input x -> ("", (sprintf "In<%d:0>" (x - 1)), x, x, IO) 
    | ComponentType.Output x -> ("", (sprintf "Out<%d:0>" (x - 1)), x, x, IO) 
    | ComponentType.IOLabel -> ("", "", 0, 0, IO) //Check generic type WHAT IS THIS?? 
    | ComponentType.BusSelection (x, y) -> ("", "", x, y, Wires)
    | ComponentType.MergeWires -> ("", "", 0, 0, Wires)
    | ComponentType.SplitWire x -> ("", "", x, 1, Wires)

/// Returns a tuple of float = (Height, Width) from two coordinates
let getHW (botR : XYPos) (topL : XYPos) = (botR.Y - topL.Y, botR.X - topL.X)
let getHWObj (sym : Symbol) = getHW sym.BotR sym.TopL

/// Finds the midpoint of two coordinates
let midXY (botR : XYPos) (topL : XYPos) : XYPos =
    let midY = (botR.Y + topL.Y) / 2.
    let midX = (botR.X + topL.X) / 2.
    {X = midX; Y = midY}

let midSym (sym : Symbol) : XYPos = midXY sym.BotR sym.TopL
let midSymX (sym : Symbol) : float = (midSym sym).X 
let midSymY (sym : Symbol) : float = (midSym sym).Y

/// Adds a float value onto an XYPos
let addXYVal (xy : XYPos) (n : float) : XYPos = {X = xy.X + n; Y = xy.Y + n}
let posDiff (a : XYPos) (b : XYPos) = {X=a.X-b.X; Y=a.Y-b.Y}
let posAdd (a : XYPos) (b : XYPos) = {X=a.X+b.X; Y=a.Y+b.Y}
let absDiff a b = 
    let diff = (posDiff a b)
    diff.X + diff.Y

/// displace will move a port position _away_ from the box by n pixels.
/// 
/// For inverters call with positive n.
/// For labels call with negative n.
let displace (n : float) (pos : XYPos) (sym : Symbol) : XYPos =
    let x = 
        if pos.X = sym.TopL.X then (pos.X - n)
        elif pos.X = sym.BotR.X then (pos.X + n)
        else pos.X
    let y =
        if pos.Y = sym.TopL.Y then (pos.Y - n)
        elif pos.Y = sym.BotR.Y then (pos.Y + n)
        else pos.Y
    {X = x; Y = y}

///Finds whether a coordinate is within a port's bounding box
let testBox (portPos : XYPos) (coord : XYPos) : bool =
    let box = (addXYVal portPos -6., addXYVal portPos 6.);
    let topL = fst box
    let botR = snd box
    topL.X <= coord.X && topL.Y <= coord.Y && botR.X >= coord.X && botR.Y >= coord.Y

/// Calculates the port position where
/// i = int indicating the index of the port on a side
/// n = int indicating the total number of ports on a side
let portPos (n : int) (topL : XYPos) (botR : XYPos) (i : int)  : XYPos = 
    let h = getHW botR topL |> fst
    let w = getHW botR topL |> snd
    let x = topL.X + (w * float(i + 1) / float(n + 1))
    let y = topL.Y + (h * float(i + 1) / float(n + 1))
    {X = x; Y = y}
   
/// Snaps the rotation to one of: 0, 90, 180, 270
let snapRot (oldRot : int) : Rotation =
    let rot = oldRot % 360 
    if rot > 315 then R0
    elif rot > 225 then R270
    elif rot > 135 then R180
    elif rot > 45 then R90
    else R0

let incrementRot (oldRot : Rotation) : Rotation = 
    match oldRot with
    | R0 -> R90
    | R90 -> R180
    | R180 -> R270
    | R270 -> R0

let updateRot (rot : int) (oldRot : Rotation) : Rotation =
    match (snapRot rot) with  
    | R0 -> oldRot
    | R90 -> incrementRot oldRot
    | R180 -> incrementRot oldRot |> incrementRot
    | R270 -> incrementRot oldRot |> incrementRot |> incrementRot

let rotToInt (rot : Rotation) : int =
    match rot with 
    | R0 -> 0
    | R90 -> 90 
    | R180 -> 180
    | R270 -> 270

/// Creates the rotation matrix for a given rotation, snapped to a multiple of 90
let getRotMat (rot : int) : float list list =
    match snapRot rot with
    | R0 -> [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 0.; 1.]] 
    | R90 -> [[0.; -1.; 0.]; [1.; 0.; 0.]; [0.; 0.; 1.]]
    | R180 -> [[-1.; 0.; 0.]; [0.; -1.; 0.]; [0.; 0.; 1.]]
    | R270 -> [[0.; 1.; 0.]; [-1.; 0.; 0.]; [0.; 0.; 1.]]

/// Removes the first row of a 2d list
let delHead (m : float list list) : float list list = List.map List.tail m
 
/// Gets a column of a 2d list
let getCol (m : float list list) : float list = List.map List.head m

/// Multiplies two lists together by element, and sums the result
let mulList (m1 : float list) (m2 : float list) : float = List.zip m1 m2 |> List.sumBy (fun (a, b) -> a*b)

/// Transposes a list in a matrix like way
let rec transp (m : float list list) : float list list =
    match m with
    | [] -> failwithf "error"
    | []::_ -> []
    | tl -> getCol tl :: transp (delHead tl)

/// Gets all pairs of a 1d and 2d list
let getPairs (m1 : float list) (m2 : float list list) : (float list * float list) list =
    List.map (fun x -> (m1, x)) m2

/// Multiplies two 2d matrices together
let multMatrix (m1 : float list list) (m2 : float list list) : float list list  =
    List.map ((fun x -> getPairs x (transp m2)) >> (List.map(fun (x, y) -> mulList x y))) m1

/// A generic transformation - It will always transform from the centre given 
/// Mathematically it performs: Translate to origin, Transform, Translate back to centre
/// The transformation must be a 2D list supplied to the function e.g. Rotation/Scaling matrix
/// This transformation only applies to a single XYPos coordinate not a full matrix
let transCoords (coord : XYPos) (centre : XYPos) (trans : float list list) : XYPos =
    let coordM = [[coord.X]; [coord.Y]; [1.]]
    let transNve = [[1.; 0.; -centre.X]; [0.; 1.; -centre.Y]; [0.; 0.; 1.]]
    let transPve = [[1.; 0.; centre.X]; [0.; 1.; centre.Y]; [0.; 0.; 1.]]
    let transform = multMatrix transPve ( multMatrix trans ( multMatrix transNve coordM) )
    {X = transform.[0].[0]; Y = transform.[1].[0]}

/// Rotates a single set of coordinates
let rotateCoords (coord : XYPos) (rot : int) (centre : XYPos) : XYPos =
    transCoords coord centre (getRotMat(rot))

/// Scales a single set of coordinates
let scaleCoords (coord : XYPos) (scale : XYPos) (centre : XYPos) : XYPos =
    transCoords coord centre [[scale.X; 0.; 0.]; [0.; scale.Y; 0.]; [0.; 0.; 1.]]

/// Finds the new box parameters after transformations have been applied
let getNewBox (topL : XYPos) (botR : XYPos) : (XYPos * XYPos) =
    let newTopL = {X = min topL.X botR.X; Y = min topL.Y botR.Y}
    let newBotR = {X = max topL.X botR.X; Y = max topL.Y botR.Y}
    (newTopL, newBotR)

/// Converts map to list, applies a function, and returns back a map/list
let genMapList myMap func = myMap |> Map.toList |> func
let genMap myMap func = genMapList myMap func |> Map.ofList

/// Generic transformation function that takes in a transformation function, symbol and transformation
/// It returns an updated symbol object
let trans func sym trans =
    let centre = midSym sym
    let rotTopL = func sym.TopL trans centre
    let rotBotR = func sym.BotR trans centre
    let newBox = getNewBox rotTopL rotBotR
    { sym with
        TopL = fst newBox
        BotR = snd newBox
        PortMap = genMap sym.PortMap (List.map (fun (k, x) -> (func k trans centre, x)))
    }

/// Finds the log base 2 of an int and rounds up to the nearest int
let log2 (n : int) : int = (log(float n) / log(2.)) |> ceil |> int

//Helpers for makePosList
let makeLR (len : int) (x : XYPos) (func : int -> XYPos) = List.map (fun i -> {X = x.X; Y = (func i).Y}) [0..len - 1]
let makeTB (len : int) (y : XYPos) (func : int -> XYPos) = List.map (fun i -> {X = (func i).X; Y = y.Y}) [0..len - 1]
let makeGen func len pos topL botR = func len pos (portPos len topL botR)

/// Creates the list of positions for ports to slot in to on each side, returns in tupled list form
let makePosList (n : int) (nBot : int) (topL : XYPos) (botR : XYPos) : (XYPos list * XYPos list * XYPos list * XYPos list) =
    let l = makeGen makeLR n topL topL botR
    let r = makeGen makeLR n botR topL botR
    let t = makeGen makeTB nBot topL topL botR 
    let b = makeGen makeTB nBot botR topL botR
    (l, r, t, b)

//Functions for retrieving portinfo data from the option type:
let getPortName (port : PortInfo Option) : string =
   match port with
   | Some x -> x.Name
   | None -> "WHAT??"

let getPortId (port : PortInfo Option) : PortId = 
    match port with
    | Some x -> PortId x.Port.Id
    | None -> PortId ""

let isPortInverse (port : PortInfo Option) : bool =
    match port with
    | Some x -> x.Invert
    | None -> false

/// Coordinates to create the tag shape used for input/output symbols,
let tagCoords (sym : Symbol) : string =
    let (i, a) = if sym.Type = ComponentType.Input 1 then 0., -1. else snd (getHWObj sym), 1.
    let midX = midSymX sym
    let midY = midSymY sym
    (sprintf "%f,%f %f,%f %f,%f %f,%f %f,%f" 
        (sym.TopL.X + (i + (a * 5.))) (midY - 10.) 
        (sym.TopL.X + (i + (a * 5.))) (midY + 10.) 
        (midX - ((i/7.) + (a * 5.))) (midY + 10.) 
        (sym.BotR.X - ((i * 1.2) + (a * 5.))) midY 
        (midX - ((i/7.) + (a * 5.))) (midY - 10.))

let cornerCoords (sym : Symbol) (i : XYPos) : XYPos = 
    match sym.Rotation with
    | R0 | R180 -> {X = midSymX sym; Y = i.Y}
    | _ -> {X = i.X; Y = midSymY sym}

let wireCoords (sym : Symbol) (i : XYPos) : string = 
    let corner = cornerCoords sym i
    sprintf "%f,%f %f,%f %f,%f" i.X i.Y corner.X corner.Y (midSymX sym) (midSymY sym)

/// Returns the coordinates for a clock symbol
let clkCoords (pos : XYPos) =
    let p1 = {X = pos.X ; Y = pos.Y - (radius)}
    let p2 = {X = pos.X + (2. * radius); Y = pos.Y }
    let p3 = {X = pos.X ; Y = pos.Y + (radius)}
    sprintf "%f,%f %f,%f %f,%f" p1.X p1.Y p2.X p2.Y p3.X p3.Y

/// Returns the rotation for SVG transformations
let rotString (sym : Symbol) (pos : XYPos) = 
    sprintf "rotate (%d, %f, %f)" (rotToInt sym.Rotation) pos.X pos.Y

let rotStringObj (sym : Symbol) = rotString sym (midSym sym)

/// Finds the extra ports required for each side based on the symbol type in the form (left, right, bot)
let numExPorts (symType : SymbolType) (numIn : int) : (int * int * int) = 
    match symType with
    | Mux -> (0, 0, log2 numIn)
    | Adder -> (1, 1, 0)
    | FF  | FFE  -> (1, 0, 1)
    | RAM -> (2, 0, 0)
    | _ -> (0, 0, 0) //Logic, AROM, wires, IO do not require extra ports

/// Converts a list of positions and a list of portinfo into a portinfo option list
/// We want to create Some(X) for every index in the list of positions that will have a port
/// And None for every index in the list of positions that will not
let mapPorts (posList : XYPos list) (portList : PortInfo list) : PortInfo Option list = 
    posList |> List.mapi (fun i _ -> if i < List.length portList then Some (List.item i portList) else None)

/// Converts the list of portinfo and the list of XYPositions into a map of <XYPos, PortInfo Option>
let getPortMap (leftPort : PortInfo list) (rightPort : PortInfo list) (botPort : PortInfo list) (topPort : PortInfo list) (l,r,t,b) : Map<XYPos, PortInfo Option> = 
    let left = mapPorts l leftPort
    let right = mapPorts r rightPort
    let top = mapPorts t topPort
    let bot = mapPorts b botPort
    List.zip (List.concat[l; r; t; b]) (List.concat[left; right; top; bot]) |> Map.ofList

/// Returns the <key, value> of a specific port in the portmap
let findPort (sym: Symbol) (pId : PortId) = 
    genMapList sym.PortMap (List.find (fun (_, k) -> getPortId k  = pId))

/// Swaps two values in a map
let swapPort portMap k1 k2 v1 v2 =
    portMap
    |> Map.change k1 (fun _ -> Some v2)
    |> Map.change k2 (fun _ -> Some v1)

//Finds the port with position closest to a coordinate, and swap the map values of that port with a given port
let swapMap (sym : Symbol) (coord : XYPos) port = 
    genMapList sym.PortMap (List.map (fun (v, k) -> (absDiff coord v, (v, k))))
    |> List.minBy fst
    |> snd
    |> function
    | (k, x) -> swapPort sym.PortMap k (fst port) x (snd port)  
                
let drawText (x : float) (y : float) (size : string) (anchor : string, baseline : string) =
    text[
        X x
        Y y
        Style[
            TextAnchor anchor
            DominantBaseline baseline
            FontSize size
            FontWeight "bold"
            Fill "Black"
            UserSelect UserSelectOptions.None] //Prevent highlighting text
    ]

let drawPolygon (points : string) (stroke : string) (fill : string) (width : float) (trans : string) =
    polygon[
        Points points
        SVGAttr.Stroke stroke
        SVGAttr.Fill fill
        SVGAttr.StrokeWidth width
        SVGAttr.Transform trans
    ]

let drawCircle (sym : Symbol) (i : XYPos) (fill : string) (stroke : string) (opac : float) (width : float) =
    circle[
        Cx (displace 3. i sym).X
        Cy (displace 3. i sym).Y
        R radius
        SVGAttr.Fill fill
        SVGAttr.Stroke stroke
        SVGAttr.Opacity opac
        SVGAttr.StrokeWidth width]

/// Returns the portmap as a list with only the ports in use: i.e. (key ,value) where v != None
let mapSetup (sym : Symbol) = genMapList sym.PortMap (List.filter (fun (_, k) -> k <> None))

let getPosEdge (sym : Symbol) (pos : XYPos) =
    if pos.X = sym.BotR.X then Right
    elif pos.Y = sym.TopL.Y then Top
    elif pos.Y = sym.BotR.Y then Bottom
    else Left

let getTextAttr (sym : Symbol) (pos : XYPos) : (string * string) =
    match getPosEdge sym pos with
    | Left -> ("start", "middle")
    | Right -> ("end", "middle")
    | Top -> ("middle", "hanging")
    | Bottom -> ("middle", "auto")

let isInvert (portType : PortType) (compType : ComponentType) : bool =
    match portType with
    | PortType.Output when compType = ComponentType.Not
                                    || compType = ComponentType.Nand
                                    || compType = ComponentType.Nor
                                    || compType = ComponentType.Xnor -> true;
    | _ -> false;

let createLabelName (genPort : GenericPort) (portType : PortType) (compType : ComponentType) (i : int) : string = 
    match genPort with
    | Select -> sprintf "S%d" i
    | Carry -> match portType with 
               | PortType.Input -> sprintf "Cin" 
               | PortType.Output -> sprintf "Cout" 
    | Enable -> sprintf "En"
    | Clk -> sprintf "Clk"
    | Addr -> if i = 0 then "addr" elif i = 1 then "write" else "Clk"
    | _ -> match compType with 
            | ComponentType.ROM x | ComponentType.AsyncROM x ->
                match portType with 
                | PortType.Input -> sprintf "addr"
                | PortType.Output -> sprintf "data"
            | ComponentType.RAM _ | ComponentType.Register _ | ComponentType.RegisterE _ ->
                match portType with 
                | PortType.Input -> sprintf "data-in"
                | PortType.Output -> sprintf "data-out"
            | ComponentType.DFF | ComponentType.DFFE ->
                match portType with 
                | PortType.Input -> sprintf "D"
                | PortType.Output -> sprintf "Q"
            | ComponentType.Input _ | ComponentType.Output _ -> ""
            | ComponentType.Custom y -> 
                match portType with 
                | PortType.Input -> sprintf "%s" (fst(List.item i y.InputLabels))
                | PortType.Output -> sprintf "%s" (fst(List.item i y.OutputLabels))
            | _ ->
                match portType with 
                | PortType.Input -> sprintf "IN%d" i
                | PortType.Output -> sprintf "OUT%d" i 

let makeList (range : int) (port : PortType) (compType : ComponentType) (pType : GenericPort) = 
    List.map(fun x -> ((createLabelName pType port compType x), port, (isInvert port compType))) [0 .. range - 1]

let exPorts (symType: SymbolType) (bot : int) (left : int) (right : int) (compType : ComponentType) =  
    match symType with
    | Mux ->  ([], [], makeList bot PortType.Input compType Select)
    | Adder -> (makeList left PortType.Input compType Carry, makeList right PortType.Output compType Carry, [])
    | FF -> ((makeList left PortType.Input compType Clk),[],[])
    | FFE -> ((makeList left PortType.Input compType Clk), [], (makeList bot PortType.Input compType Enable))
    | RAM -> ((makeList left PortType.Input compType Addr), [], [])
    | _ -> ([],[],[])
    
let findPortList (numIn : int) (numOut : int) (compType : ComponentType) : (string * PortType * bool) list list =
    let (_, _, _, _, symType) = typeToInfo compType
    let (left, right, bot) = numExPorts symType (max numIn numOut)
    let ins = makeList numIn PortType.Input compType InOut
    let outs = makeList numOut PortType.Output compType InOut
    let (l, r, b) = exPorts symType bot left right compType
    [List.concat[ins; l]; List.concat[outs; r]; b; []]

let getSymLabel (comp : ComponentType) (i : int) : string = 
    let (a, _, _, _, _) = typeToInfo comp
    match a with 
    | "" -> ""
    | _ -> sprintf "%s%i" a i

let getDisplace (k : PortInfo Option) = 
    if getPortName k = "Clk" then -7. else -3.
    
let makeIssiePorts (l, r, b, t) (portType : PortType) : Port list =
    List.concat [l; r; b; t] 
    |> List.filter (fun x -> x.Port.PortType = portType) 
    |> List.map (fun x -> x.Port)

//---------------------------------------------------------------------------//
//----------------------helper initialisation funcs--------------------------//
//---------------------------------------------------------------------------//

/// Creates Symbol.PortInfo object. 
///
/// i : Index of the port (e.g. IN0 : i = 0).
/// w : the port width
let createPortInfo (i : int) (portType : PortType) (compId : ComponentId) (name : string) (invert : bool) (w : int) : PortInfo = 
    //Object creation
    {      
        Port = {
            Id = uuid()
            PortNumber = Some i
            PortType =  portType
            HostId = string(compId)
        }
        NumWires = 0
        Name = name
        Invert = invert
        Width = w
    }
    
/// Creates a new object of type symbol from component type, position, number of inputs, and number of outputs
let createSymbol (compType : ComponentType) (ports : (string * PortType * bool) list list) (pos : XYPos) (index : int) (label : string) : Symbol =
    
    let _id = ComponentId (Helpers.uuid())

    //Getting type info for symbol/port construction
    let (_, name, wIn, _, symType) = typeToInfo compType
    let len = String.length(name) |> float

    let portInfos = List.map (List.mapi (fun i (name, pType, inv) -> createPortInfo i pType _id name inv wIn)) ports
    let (leftPort, rightPort, botPort, topPort) = (portInfos.[0], portInfos.[1], portInfos.[2], portInfos.[3])
    let (left, right, bot, top) = (leftPort.Length, rightPort.Length, botPort.Length, topPort.Length)

    //Finding the length of labels/component name, there are 2 cases to consider
    //1. The sum of the labels on the top/bottom is larger than the component width
    //2. The max length of the label on the left/right * 2 , plus the name of the component is larger than the width
    let labelLens = 
        List.map (List.map (fun (n, _, _) -> float (String.length n)) >> (List.sum)) ports
    let maxlens = List.map (List.map (fun (n, _, _) -> float (String.length n)) >> (fun x -> if List.isEmpty x then 0. else List.max x)) ports 
    let test = 
        max (max labelLens.[2] labelLens.[3]) ((max maxlens.[0] maxlens.[1])*2. + len) 
        |> (*) (float labelSize)

    let n = max left right |> float //The max number of ports initially will always be on the left or right of the box
    let nBot = if bot > 0 || top > 0 then max bot top else (int (ratioHW * n)) //If there is no ports on the top/bot, the component should still have ports in the portmap
    let h = 
        if left = 1 && right = 1 then 2. 
        else n 
        |> (*) (stdHeight)
    let w = 
        if bot <= 0 && top <= 0 then
            max (ratioHW * h) test //Width is standard
        else max ((float nBot) * stdHeight * 1.7) test //Or width based on number of ports on the bottom
    let botR = {X = pos.X + w; Y = pos.Y + h}
    
    // ---- Making portMap ---- //
    let portMap = getPortMap leftPort rightPort botPort topPort (makePosList (int n) nBot pos botR)

    // ---- Making list for Issie ---//
    let inputList = makeIssiePorts (leftPort, rightPort, topPort, botPort) PortType.Input
    let outputList = makeIssiePorts (leftPort, rightPort, topPort, botPort) PortType.Output

    // ------- Symbol Creation ------ ///
    {
        TopL = pos
        BotR = botR
        Id = _id
        Type = compType
        Name = name
        Highlight = "gainsboro"
        PortHighlight = false
        GenericType = symType
        PortMap = portMap
        ShowSlots = false
        Index = index
        Label = label
        IOList = (inputList, outputList) //Interface with issie
        Rotation = R0
        Scale = {X = 1.; Y = 1.}
    }

//-----------------------Skeleton Message type for symbols---------------------//


/// Dummy function for test. Creates 4 NAND gates with 2 input, 1 output.
let init () =
    //use for checking rom/ram
    let memory = {
        Memory.AddressWidth = 10 
        Memory.WordWidth = 10
        Memory.Data = [(1L, 0L); (2L, 0L); (3L, 0L)] |> Map.ofList
    }
    let custom = {
        CustomComponentType.Name = "reallyLongComponentName"
        // Tuples with (label * connection width).
        CustomComponentType.InputLabels = [("myIn1", 0); ("myIn2", 1)] // (string * int) list
        CustomComponentType.OutputLabels = [("myOut1", 0); ("myOut2", 1)]  //(string * int) list 
    }
    [
        (createSymbol (ComponentType.And) [[("IN0", PortType.Input, false); ("IN1", PortType.Input, false)]; [("OUT1", PortType.Output, true)]; []; [("OUT2", PortType.Output, true); ("reallyreallyreallylonglabelname", PortType.Output, true); ("OUT4", PortType.Output, true)]] {X = 250.; Y = 150.} 0 (getSymLabel ComponentType.And 0))
        (createSymbol (ComponentType.MergeWires) (findPortList 1 4 ComponentType.MergeWires) {X = 50.; Y = 100.} 0 (getSymLabel ComponentType.MergeWires 0))
        (createSymbol (ComponentType.Nand) (findPortList 2 1 ComponentType.Nand) {X = 200.; Y = 50.} 0 (getSymLabel ComponentType.Nand 0))
        (createSymbol (ComponentType.Mux2) (findPortList 2 1 ComponentType.Mux2) {X = 300.; Y = 50.} 0 (getSymLabel ComponentType.Mux2 0))
        (createSymbol (ComponentType.Demux2) (findPortList 1 2 ComponentType.Demux2) {X = 400.; Y = 50.} 0 (getSymLabel ComponentType.Demux2 0))
        (createSymbol (ComponentType.Input 1) (findPortList 0 1 (ComponentType.Input 1)) {X = 200.; Y = 200.} 0 (getSymLabel (ComponentType.Input 1) 0))
        (createSymbol (ComponentType.Decode4) (findPortList 2 4 ComponentType.Decode4) {X = 250.; Y = 250.} 0 (getSymLabel ComponentType.Decode4 0))
        (createSymbol (ComponentType.Register 1) (findPortList 1 1 (ComponentType.Register 1)) {X = 500.; Y = 100.} 0 (getSymLabel (ComponentType.Register 1) 0))
        (createSymbol (ComponentType.DFFE) (findPortList 1 1 ComponentType.DFFE) {X = 500.; Y = 200.} 0 (getSymLabel ComponentType.DFFE 0))
        (createSymbol (ComponentType.RAM memory) (findPortList 1 1 (ComponentType.RAM memory)) {X = 500.; Y = 300.} 0 (getSymLabel (ComponentType.RAM memory) 0))
        (createSymbol (ComponentType.Custom custom) (findPortList (List.length custom.InputLabels) (List.length custom.OutputLabels) (ComponentType.Custom custom)) {X = 20.; Y = 300.} 0 (getSymLabel (ComponentType.Custom custom) 0))
    ]
    , Cmd.none

let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    // | Add (compType, pagePos, numIn, numOut, index) ->
    | Add addMsg ->
        (createSymbol addMsg.CompType (findPortList addMsg.Input addMsg.Output addMsg.CompType) addMsg.PagePos addMsg.Index (getSymLabel addMsg.CompType addMsg.Index)) :: model, Cmd.none

    | Delete sIdList -> 
        List.filter (fun sym -> not (List.contains sym.Id sIdList)) model, Cmd.none

    | Move (compList, translate) ->
        model
        |> List.map (fun sym ->
            if List.contains sym.Id compList then
                { sym with
                    TopL = posAdd sym.TopL translate
                    BotR = posAdd sym.BotR translate
                    PortMap = genMap sym.PortMap (List.map (fun (k, x) -> (posAdd k translate, x))) 
                }
            else
                sym
        )
        , Cmd.none

    | Highlight sIdList ->
        model
        |> List.map (fun sym ->
            { sym with
                Highlight = if List.contains sym.Id sIdList then "lightblue" else "gainsboro"
            }
        )
        , Cmd.none

    | HighlightError sIdList ->
        model
        |> List.map (fun sym ->
            { sym with
                Highlight = if List.contains sym.Id sIdList then "red" else "gainsboro"
            }
        )
        , Cmd.none

    | HighlightPorts sIdList ->
        model
        |> List.map (fun sym ->
            { sym with
                PortHighlight = List.contains sym.Id sIdList
            }
        )
        , Cmd.none

    | DragPort (sId, pId, pagePos) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                let port = findPort sym pId
                { sym with
                    PortMap = swapMap sym pagePos port
                }
        )
        , Cmd.none

    | Rotate (sId, rot) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                let newSym = trans rotateCoords sym rot
                { newSym with
                    Rotation = updateRot rot sym.Rotation     
                }
        )
        , Cmd.none

    | Scale (sId, scale) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                let newSym = trans scaleCoords sym scale
                { newSym with
                    Scale = {X = sym.Scale.X * scale.X; Y = sym.Scale.Y * scale.Y }
                }
        )
        , Cmd.none

    | DisplaySlots (sId) ->
        model
        |> List.map (fun sym ->
            { sym with
                ShowSlots = List.contains sym.Id [sId]
            }
        )
        , Cmd.none

    | Rename (sId, name) ->
        model
        |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                { sym with
                    Label = name
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
    FunctionComponent.Of(
        fun (props : RenderObjProps) ->
            let sym = props.Obj //for ease of use
            let strokeColour = if sym.PortHighlight then "green" else "black"
            let color =
                match sym.GenericType with
                | Wires -> 
                    if sym.Highlight = "lightblue" then
                        "purple"
                    else if sym.PortHighlight then
                        "green"
                    else
                        "darkgrey"
                | _ -> sym.Highlight

            let drawClk : ReactElement list =
                sym
                |> mapSetup
                |> List.collect (fun (i, k) ->
                    if getPortName k = "Clk" then
                        [(drawPolygon (clkCoords i) "black" color 1. (rotString sym i))[]]
                    else [])
                
            let labels : ReactElement list = 
                sym
                |> mapSetup
                |> List.map(fun (i, k) -> 
                    (drawText (displace (getDisplace k) i sym).X  (displace (getDisplace k) i sym).Y (sprintf "%dpx" labelSize) (getTextAttr sym i))[str <| sprintf "%s" (getPortName k)])

            let wires : ReactElement list =
            //line should be (port.x, port.y), (mid.x, port.y), (mid.x, mid.y)
                sym
                |> mapSetup
                |> List.map(fun (i, _) ->
                    polyline[
                        Points (wireCoords sym i)
                        SVGAttr.Stroke color
                        SVGAttr.Fill "none"
                    ][])

            let triangles : ReactElement list =
                sym
                |> mapSetup
                |> List.map(fun (i, _) -> (drawPolygon (clkCoords (cornerCoords sym i)) color color 1. (rotString sym (cornerCoords sym i)))[])
            
            let io : ReactElement = drawPolygon (tagCoords sym) strokeColour color 0.5 (rotStringObj sym)[]

            let displayBox : ReactElement =
                rect[
                    X sym.TopL.X
                    Y sym.TopL.Y
                    SVGAttr.Height (getHWObj sym |> fst)
                    SVGAttr.Width (getHWObj sym |> snd)
                    SVGAttr.Fill color
                    SVGAttr.Stroke strokeColour
                    SVGAttr.StrokeWidth 0.5][]

            let title = drawText (midSymX sym) (midSymY sym) "10px" ("middle", "middle") [str <| sprintf "%A" sym.Name]
            
            let symLabel = drawText (midSymX sym) (sym.TopL.Y - 10.) "10px" ("middle", "middle") [str <| sprintf "%A" sym.Label]
            
            let drawInvert =
                genMapList sym.PortMap (List.filter(fun (_, k) -> isPortInverse k))
                |> List.map(fun (i, _) -> drawCircle sym i color "black" 1. 0.5[])

            let ports =
                if sym.PortHighlight then
                    sym
                    |> mapSetup
                    |> List.map(fun (i, _) -> drawCircle sym i "deepskyblue" "deepskyblue" 0.4 1.[])
                else []

            let labelPos : ReactElement list = 
                if sym.ShowSlots then
                    genMapList sym.PortMap id
                    |> List.map(fun (i, _) -> drawCircle sym (displace -5. i sym) "purple" "purple" 0.4 1.[])
                else []
            
            let symDraw = 
                match sym.GenericType with
                | Wires -> List.concat [wires; triangles]
                | IO -> [io]
                | _ -> [displayBox]
            
            g[](List.concat [symDraw; labels; drawInvert; ports; [title]; [symLabel]; labelPos; drawClk])
            
    , "Circle"
    , equalsButFunctions
    )

/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = ComponentId id} as shape) ->
        renderObj 
            {
                Obj = shape
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList

//---------------Helpers for interface functions--------------------//

/// Initialises the model for a port search by converting every symbol to a list portmap
let initPortSearch (symModel: Model) : (XYPos * PortInfo Option) list = 
    symModel |> List.collect ((fun x -> x.PortMap) >> (Map.toList))

/// Gets a portinfo object from the model where PortId = pId
let getPortInfo (symModel: Model) (pId : PortId) =
    initPortSearch symModel
    |> List.map (fun (_, k) -> (k, getPortId k))
    |> List.tryFind (fun (_, id) -> id = pId)
    |> function
    | Some (Some k, _) -> k
    | Some _ ->  failwithf "Unexpected error in getPortInfo"
    | None -> failwithf "Error in getPortInfo: couldn't find portID"

//-----------------------Interface functions--------------------//

/// Searches through the whole model until the port is found and retruns the position of that port
let getPortCoords (symModel: Model) (pId : PortId) : XYPos = 
    initPortSearch symModel
    |> List.map (fun (v, k) -> (v, getPortId k))
    |> List.tryFind (fun (_, k) -> k = pId)
    |> function
    | Some x -> fst x
    | None -> failwithf "Error in getPortCoords: couldn't find portID"

/// Returns all symbols in the model in the form (ID, bounding box topLeft, bounding box botRight)
let getBoundingBoxes (symModel : Model) (startCoord : XYPos) : (ComponentId * XYPos * XYPos) list =
    List.map (fun sym -> (sym.Id, sym.TopL, sym.BotR)) symModel

/// Finds the portType of a specific port
let getPortType (symModel: Model) (pId : PortId) : PortType =
    (getPortInfo symModel pId).Port.PortType

/// Finds if a position lies on a port. Returns Some(position, portId) if found, none otherwise.
let isPort (symModel : Model) (pos : XYPos) : (XYPos * PortId) Option =
    symModel
    |>initPortSearch
    |> List.tryFind (fun (v, _) -> testBox v pos)
    |> function
    | Some(v, Some k) -> Some(v, (PortId (k.Port.Id)))
    | _ -> None

let isLabel (model : Model) (pos : XYPos) (sId : ComponentId) : (XYPos * PortId) Option =
    model
    |> List.tryFind (fun sym -> sym.Id = sId)
    |> function
    | Some sym -> Some (genMapList sym.PortMap (List.map (fun (x, y) -> (x, y, testBox (displace -9. x sym) pos))) 
                        |> List.filter (fun (x, y, z) -> z)
                        |> List.map (fun (x, y, z) -> (x, getPortId y))
                        |> List.item 0)
    | None -> None
    

let getSymbol (model : Model) (sId : ComponentId) : Symbol = 
    model
    |> List.tryFind (fun sym -> sym.Id = sId)
    |> function
    | Some sym -> sym
    | None -> failwithf "Error in getSymbol, couldn't find symbol"

//Returns a list of Port Ids for a given symbol
let getPortIds (model : Model) (sId : ComponentId) : PortId list = 
    model
    |> List.tryFind (fun sym -> sym.Id = sId)
    |> function
    | Some sym -> genMapList sym.PortMap (List.map (fun (_, k) -> getPortId k))
    | None -> failwithf "Error in getPortIds, couldn't find symbol"

let getPortWidth (model : Model) (pId : PortId) : int = 
    (getPortInfo model pId).Width

let getHostId (model : Model) (pId : PortId) : ComponentId =
    ComponentId ((getPortInfo model pId).Port.HostId)

let getPortEdge (model : Model) (pId : PortId) : Edge =
    let pos = getPortCoords model pId
    let sym = List.item 0 (List.filter (fun sym -> sym.Id = getHostId model pId) model)
    getPosEdge sym pos

let getBoundingBox symModel symID =
    List.tryFind (fun x -> x.Id = symID) symModel
    |> function 
    | Some x -> (x.TopL, x.BotR)
    | None -> failwithf "Could not get bounding box"

let getPort (model : Model) (pId : PortId) : Port = 
    model
    |> initPortSearch
    |> List.map (fun (_, x) -> x)
    |> List.filter (fun x -> getPortId x = pId)
    |> List.item 0
    |> function
    | Some x -> x.Port
    | None -> failwithf "Unexpected error in getPort"

//Returns the number of inputs and outputs for a given symbol as a tuple
let getNumIOs (sym : Symbol) : (int * int) =
    let (inPorts, outPorts) = sym.IOList
    let (i, o) = (List.length inPorts, List.length outPorts)
    match sym.GenericType with
    | RAM | FFE -> (i - 2, o)
    | FF -> (i - 1, o)
    | Adder -> (i - 1, o - 1)
    | Mux -> (i - 1, o)
    | _ -> (i, o)
    
//----------------------interface to Issie-----------------------------//

let symToIssie (sym : Symbol) : Component = 
    {
        Component.Id = string sym.Id
        Component.Type = sym.Type
        Component.Label = sym.Label
        Component.InputPorts = fst sym.IOList
        Component.OutputPorts = snd sym.IOList
        Component.X = int sym.TopL.X
        Component.Y = int sym.TopL.Y
        Component.H = getHWObj sym |> fst |> int
        Component.W = getHWObj sym |> snd |> int
     }

let extractComponent (symModel: Model) (sId:ComponentId) : Component= 
    symModel 
    |> List.tryFind (fun x -> x.Id = sId)
    |> function
    | Some x -> symToIssie x
    | None -> failwithf "couldnt find symbol id in extract component"

let extractComponents (symModel: Model) : Component list = 
    symModel |> List.map symToIssie


