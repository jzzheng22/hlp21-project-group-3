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
let STD_HEIGHT = 35.
let HW_RATIO = 0.9
let RAD = 3.

type Portinfo = 
    {
        Port: CommonTypes.Port
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

type Symbol =
    {
        TopL: XYPos
        BotR: XYPos
        Id : CommonTypes.ComponentId
        Type : CommonTypes.ComponentType
        Name : string
        Highlight : string
        PortHighlight : bool
        PortMap : Map<XYPos, Portinfo Option>
        Rotation : int
        Scale : XYPos
        GenericType : SymbolType
    }

type Model = Symbol list

//---------------------------------------------------------------------------//
//----------------------------Message Type-----------------------------------//
//---------------------------------------------------------------------------//

type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    | Move of sIDList : CommonTypes.ComponentId list * pagePos : XYPos
    | Add of compType: CommonTypes.ComponentType * pagePos : XYPos * numIn : int * numOut : int
    | Delete of sIdList : CommonTypes.ComponentId list
    | Highlight of sIdList: CommonTypes.ComponentId list
    | HighlightError of sIdList: CommonTypes.ComponentId list
    | HighlightPorts of sIdList : CommonTypes.ComponentId list
    | DragPort of sId : CommonTypes.ComponentId * pId : CommonTypes.PortId * pagePos: XYPos
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
    | CommonTypes.ComponentType.Decode4 -> ("Decode", 1, 1, LogMem)
    | CommonTypes.ComponentType.Mux2 -> ("MUX", 1, 1, Mux)
    | CommonTypes.ComponentType.Demux2 -> ("DMUX" , 1, 1, Mux)
    | CommonTypes.ComponentType.NbitsAdder x -> ("Σ", x, x, Adder)
    | CommonTypes.ComponentType.Custom x -> (x.Name, 1, 1, LogMem)
    | CommonTypes.ComponentType.DFF -> ("DFF", 1, 1, FF)
    | CommonTypes.ComponentType.DFFE -> ("DFFE", 1, 1, FFE)
    | CommonTypes.ComponentType.Register x -> ("SRG", x, x, FF)
    | CommonTypes.ComponentType.RegisterE x -> ("SRGE", x, x, FFE) 
    | CommonTypes.ComponentType.AsyncROM x -> ("AROM", x.AddressWidth, x.WordWidth, LogMem)
    | CommonTypes.ComponentType.ROM x -> ("ROM", x.AddressWidth, x.WordWidth, FF)
    | CommonTypes.ComponentType.RAM x -> ("RAM", x.AddressWidth, x.WordWidth, RAM)
    | CommonTypes.ComponentType.Input x -> ((sprintf "In<%d:0>" (x - 1)), x, x, IO) 
    | CommonTypes.ComponentType.Output x -> ((sprintf "Out<%d:0>" (x - 1)), x, x, IO) 
    | CommonTypes.ComponentType.IOLabel -> ("", 0, 0, IO) //Check generic type WHAT IS THIS?? 
    | CommonTypes.ComponentType.BusSelection (x, y) -> ("", x, y, Wires)
    | CommonTypes.ComponentType.MergeWires -> ("", 0, 0, Wires)
    | CommonTypes.ComponentType.SplitWire x -> ("", x, 1, Wires)

///Returns a tuple of float = (Height, Width) from two coordinates
let getHW (botR : XYPos) (topL : XYPos) = (botR.Y - topL.Y, botR.X - topL.X)
let getHWObj (sym : Symbol) = getHW sym.BotR sym.TopL

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

let posDiff a b = {X=a.X-b.X; Y=a.Y-b.Y}
let posAdd a b = {X=a.X+b.X; Y=a.Y+b.Y}
let posOf x y = {X=x;Y=y}

let absDiff a b = 
    let diff = (posDiff a b)
    diff.X + diff.Y

///displace will move a port position _away_ from the box by n pixels.
///
///For inverters call with positive n.
///For labels call with negative n.
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
    let box = (addXYVal portPos -7., addXYVal portPos 7.);
    let topL = fst box
    let botR = snd box
    topL.X <= coord.X && topL.Y <= coord.Y && botR.X >= coord.X && botR.Y >= coord.Y

///Calculates the port position where
///i = int indicating the index of the port on a side
///n = int indicating the total number of ports on a side
let portPos (n : int) (topL : XYPos) (botR : XYPos) (i : int)  : XYPos = 
    let h = getHW botR topL |> fst
    let w = getHW botR topL |> snd
    let x = topL.X + (w * float(i + 1) / float(n + 1))
    let y = topL.Y + (h * float(i + 1) / float(n + 1))
    {X = x; Y = y}
   
///Snaps the rotation to one of: 0, 90, 180, 270
let getRot (rot : int) : int =
    if rot >= 0 && rot < 45 then 0
    elif rot >= 45 && rot < 135 then 90
    elif rot >= 135 && rot < 225 then 180
    elif rot >= 225 && rot < 315 then 270
    elif rot >= 315 && rot < 360 then 0
    else 0

///Creates the rotation matrix for a given rotation, snapped to a multiple of 90
let getRotMat (rot : int) : float list list=
    match getRot rot with
    | 90 -> [[0.; -1.; 0.]; [1.; 0.; 0.]; [0.; 0.; 1.]]
    | 180 -> [[-1.; 0.; 0.]; [0.; -1.; 0.]; [0.; 0.; 1.]]
    | 270 -> [[0.; 1.; 0.]; [-1.; 0.; 0.]; [0.; 0.; 1.]]
    | _ -> [[1.; 0.; 0.]; [0.; 1.; 0.]; [0.; 0.; 1.]] //rotate 0 degrees

///Removes the first row of a 2d list
let delHead (m : float list list) : float list list = List.map List.tail m
 
///Gets a column of a 2d list
let getCol (m : float list list) : float list = List.map List.head m

///Multiplies two lists together by element, and sums the result
let mulList (m1 : float list) (m2 : float list) : float = List.zip m1 m2 |> List.sumBy (fun (a, b) -> a*b)

///Transposes a list in a matrix like way
let rec transp (m : float list list) : float list list =
    match m with
    | [] -> failwithf "error"
    | []::tl -> []
    | tl -> getCol tl :: transp (delHead tl)

///Gets all pairs of a 1d and 2d list
let getPairs (m1 : float list) (m2 : float list list) : (float list * float list) list =
    List.map (fun x -> (m1, x)) m2

///Multiplies two 2d matrices together
let multMatrix (m1 : float list list) (m2 : float list list) : float list list  =
    List.map ((fun x -> getPairs x (transp m2)) >> (List.map(fun (x, y) -> mulList x y))) m1

///A generic transformation - It will always transform from the centre given 
///Mathematically it performs: Translate to origin, Transform, Translate back to centre
///The transformation must be a 2D list supplied to the function e.g. Rotation/Scaling matrix
///This transformation only applies to a single XYPos coordinate not a full matrix
let transCoords (coord : XYPos) (centre : XYPos) (trans : float list list) : XYPos =
    let coordM = [[coord.X]; [coord.Y]; [1.]]
    let transNve = [[1.; 0.; -centre.X]; [0.; 1.; -centre.Y]; [0.; 0.; 1.]]
    let transPve = [[1.; 0.; centre.X]; [0.; 1.; centre.Y]; [0.; 0.; 1.]]
    let transform = multMatrix transPve ( multMatrix trans ( multMatrix transNve coordM) )
    {X = transform.[0].[0]; Y = transform.[1].[0]}

///Rotates a single set of coordinates
let rotateCoords (coord : XYPos) (rot : int) (centre : XYPos) : XYPos =
    transCoords coord centre (getRotMat(rot))

///Scales a single set of coordinates
let scaleCoords (coord : XYPos) (scale : XYPos) (centre : XYPos) : XYPos =
    transCoords coord centre [[scale.X; 0.; 0.]; [0.; scale.Y; 0.]; [0.; 0.; 1.]]

///Finds the new box parameters after transformations have been applied
let getNewBox (topL : XYPos) (botR : XYPos) : (XYPos * XYPos) =
    let newTopL = {X = min topL.X botR.X; Y = min topL.Y botR.Y}
    let newBotR = {X = max topL.X botR.X; Y = max topL.Y botR.Y}
    (newTopL, newBotR)

///Converts map to list, applies a function, and returns back a map/list
let genMapList myMap func = myMap |> Map.toList |> func
let genMap myMap func = genMapList myMap func |> Map.ofList

///Generic transformation function that takes in a transformation function, symbol and transformation
///It returns an updated symbol object
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

///Finds the log base 2 of an int and rounds up to the nearest int
let log2 (n : int) : int = (log(float n) / log(2.)) |> ceil |> int

//Helpers for makePosList
let makeLR (len : int) x func = List.map (fun i -> {X = x.X; Y = (func i).Y}) [0..len - 1]
let makeTB (len : int) y func = List.map (fun i -> {X = (func i).X; Y = y.Y}) [0..len - 1]
let makeGen func len pos topL botR = func len pos (portPos len topL botR)

///Creates the list of positions for ports to slot in to on each side, returns in tupled list form
let makePosList (n : int) (nBot : int) (topL : XYPos) (botR : XYPos) : (XYPos list * XYPos list * XYPos list * XYPos list) =
    let l = makeGen makeLR n topL topL botR
    let r = makeGen makeLR n botR topL botR
    let t = makeGen makeTB nBot topL topL botR 
    let b = makeGen makeTB nBot botR topL botR
    (l, r, t, b)

//Functions for retrieving portinfo data from the option type:
let getPortName (port : Portinfo Option) : string =
   match port with
   | Some x -> x.Name
   | None -> "WHAT??"

let getPortId (port : Portinfo Option) : CommonTypes.PortId = 
    match port with
    | Some x -> CommonTypes.PortId x.Port.Id
    | None -> CommonTypes.PortId ""

let isPortInverse (port : Portinfo Option) : bool =
    match port with
    | Some x -> x.Invert
    | None -> false

///Coordinates to create the tag shape used for input/output symbols,
let tagCoords (sym : Symbol) : string =
    let (i, a) = if sym.Type = CommonTypes.ComponentType.Input 1 then 0., -1. else snd (getHWObj sym), 1.
    let midX = midSymX sym
    let midY = midSymY sym
    (sprintf "%f,%f %f,%f %f,%f %f,%f %f,%f" 
        (sym.TopL.X + (i + (a * 5.))) (midY - 10.) 
        (sym.TopL.X + (i + (a * 5.))) (midY + 10.) 
        (midX - ((i/7.) + (a * 5.))) (midY + 10.) 
        (sym.BotR.X - ((i * 1.2) + (a * 5.))) midY 
        (midX - ((i/7.) + (a * 5.))) (midY - 10.))

///Returns the coordinates of a triangle where midpoint of the flat side = input position i
let triangleCoords (i : XYPos) (sym : Symbol) : string =
    let pos = midSymX sym
    (sprintf "%f,%f %f,%f %f,%f" pos (i.Y + RAD) (pos + (RAD * 2.)) i.Y pos (i.Y - RAD))

///Finds the extra ports required for each side based on the symbol type in the form (left, right, bot)
let numExPorts (symType : SymbolType) (numIn : int) : (int * int * int) = 
    match symType with
    | Mux -> (0, 0, log2 numIn)
    | Adder -> (1, 1, 0)
    | FF  | FFE  -> (1, 0, 1)
    | RAM -> (2, 0, 0)
    | _ -> (0, 0, 0) //Logic, AROM, wires, IO do not require extra ports

///Converts a list of positions and a list of portinfo into a portinfo option list
///We want to create Some(X) for every index in the list of positions that will have a port
///And None for every index in the list of positions that will not
let mapPorts (posList : XYPos list) (portList : Portinfo list) : Portinfo Option list = 
    posList |> List.mapi (fun i _ -> if i < List.length portList then Some (List.item i portList) else None)

///Converts the list of portinfo and the list of XYPositions into a map of <XYPos, Portinfo Option>
let getPortMap (ins: Portinfo list) (leftPort : Portinfo list) (outs : Portinfo list) (rightPort : Portinfo list) (botPort : Portinfo list) (l,r,t,b) : Map<XYPos, Portinfo Option> = 
    let left = List.concat [ins; leftPort] |> mapPorts l
    let right =  List.concat [outs; rightPort] |> mapPorts r
    let top = [] |> mapPorts t 
    let bot = botPort |> mapPorts b
    List.zip (List.concat [l; r; t; b]) (List.concat[left; right; top; bot]) |> Map.ofList

///Returns the <key, value> of a specific port in the portmap
let findPort (sym: Symbol) (pId : CommonTypes.PortId) = 
    genMapList sym.PortMap (List.find (fun (_, k) -> getPortId k  = pId))

///Swaps two values in a map
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
                
let drawText (x : float) (y : float) (size : string) =
    text[
        X x
        Y y
        Style[
            TextAnchor "middle"
            DominantBaseline "middle"
            FontSize size
            FontWeight "bold"
            Fill "Black"
            UserSelect UserSelectOptions.None] //Prevent highlighting text
    ]

let drawPolygon (points : string) (stroke : string) (fill : string) (width : float) =
    polygon[
        Points points
        SVGAttr.Stroke stroke
        SVGAttr.Fill fill
        SVGAttr.StrokeWidth width
    ]

let drawCircle (sym : Symbol) (i : XYPos) (fill : string) (stroke : string) (opac : float) (width : float) =
    circle[
        Cx (displace 3. i sym).X
        Cy (displace 3. i sym).Y
        R RAD
        SVGAttr.Fill fill
        SVGAttr.Stroke stroke
        SVGAttr.Opacity opac
        SVGAttr.StrokeWidth width]

///Returns the portmap as a list with only the ports in use: i.e. (key ,value) where v != None
let mapSetup (sym : Symbol) = genMapList sym.PortMap (List.filter (fun (_, k) -> k <> None))

//---------------------------------------------------------------------------//
//----------------------helper initialisation funcs--------------------------//
//---------------------------------------------------------------------------//

/// Creates Symbol.PortInfo object. 
///
/// i : Index of the port (e.g. IN0 : i = 0).
/// genPort :  the generic porttype used to create any extra ports/labels
/// w : the port width
let CreatePortInfo (i : int) (portType : CommonTypes.PortType) (genPort : GenericPort) (compId : CommonTypes.ComponentId) (compType : CommonTypes.ComponentType) (w : int) : Portinfo = 
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
            | Select -> sprintf "S%d" i
            | Carry -> match portType with 
                       | CommonTypes.PortType.Input -> sprintf "Cin" 
                       | CommonTypes.PortType.Output -> sprintf "Cout" 
            | Enable -> sprintf "En"
            | Clk -> sprintf "Clk"
            | Addr -> if i = 0 then "addr" elif i = 1 then "write" else "Clk"
            | _ -> match compType with 
                    | CommonTypes.ComponentType.ROM x | CommonTypes.ComponentType.AsyncROM x ->
                        match portType with 
                        | CommonTypes.PortType.Input -> sprintf "addr"
                        | CommonTypes.PortType.Output -> sprintf "data"
                    | CommonTypes.ComponentType.RAM _ | CommonTypes.ComponentType.Register _ | CommonTypes.ComponentType.RegisterE _ ->
                        match portType with 
                        | CommonTypes.PortType.Input -> sprintf "data-in"
                        | CommonTypes.PortType.Output -> sprintf "data-out"
                    | CommonTypes.ComponentType.DFF | CommonTypes.ComponentType.DFFE ->
                        match portType with 
                        | CommonTypes.PortType.Input -> sprintf "D"
                        | CommonTypes.PortType.Output -> sprintf "Q"
                    | CommonTypes.ComponentType.Input _ | CommonTypes.ComponentType.Output _ -> ""
                    | _ ->
                        match portType with 
                        | CommonTypes.PortType.Input -> sprintf "IN%d" i
                        | CommonTypes.PortType.Output -> sprintf "OUT%d" i 

        Invert = 
            match portType with
            | CommonTypes.PortType.Output when compType = CommonTypes.ComponentType.Not
                                            || compType = CommonTypes.ComponentType.Nand
                                            || compType = CommonTypes.ComponentType.Nor
                                            || compType = CommonTypes.ComponentType.Xnor -> true;
            | _ -> false;
        
        Width = w
    }


let makePort (range : int) (port : CommonTypes.PortType) (_id : CommonTypes.ComponentId) (compType : CommonTypes.ComponentType) (width : int) (pType : GenericPort) = 
    List.map(fun x -> CreatePortInfo x port pType _id compType width) [0 .. range - 1]

let getExPorts (symType: SymbolType) (bot : int) (left : int) (right : int) (_id : CommonTypes.ComponentId) (compType : CommonTypes.ComponentType) (wIn : int) (wOut : int) = 
    match symType with
    | Mux ->  ([], [], makePort bot CommonTypes.PortType.Input _id compType wIn Select)
    | Adder -> ((makePort left CommonTypes.PortType.Input _id compType wIn Carry), (makePort right CommonTypes.PortType.Output _id compType wOut Carry), [])
    | FF -> ((makePort left CommonTypes.PortType.Input _id compType wIn Clk),[],[])
    | FFE -> ((makePort left CommonTypes.PortType.Input _id compType wIn Clk), [], (makePort bot CommonTypes.PortType.Input _id compType wIn Enable))
    | RAM -> ((makePort left CommonTypes.PortType.Input _id compType wIn Addr), [], [])
    | _ -> ([],[],[])

///Creates a new object of type symbol from component type, position, number of inputs, and number of outputs
let CreateNewSymbol (compType : CommonTypes.ComponentType) (numIn : int) (numOut : int) (pos : XYPos) : Symbol =
    
    //Getting type info for symbol/port construction
    let (name, wIn, wOut, symType) = typeToInfo compType

    //Finding the component specific extra ports required
    let (left, right, bot) = numExPorts symType (max numIn numOut)

    //Intermediate calculations
    let n = max (numIn + left) (numOut + right) |> float //The max number of ports initially will always be on the left or right of the box
    let nBot = if bot > 0 then bot else (int (HW_RATIO * n)) //If there is no ports on the top/bot, the component should still have ports in the portmap
    let h = if numIn = 1 && numOut = 1 then STD_HEIGHT * 2. else STD_HEIGHT * n //ensures minimum height for 1 in 1 out components
    let w =  if bot <= 0 then (HW_RATIO * h) else ((float nBot) * STD_HEIGHT * 1.7) //Width is either standard, or based on number of ports on the bottom
    let botR = {X = pos.X + w; Y = pos.Y + h}
    
    //Symbol's Component id creation
    let _id = CommonTypes.ComponentId (Helpers.uuid())
     
    // ---- Making portMap ---- //
    let posList = makePosList (int n) nBot pos botR
    let (l, r, t, b) =  posList

    // ---- Making symbol's ports ---- //
    //First make the generic input output ports with labels IN0...INn,  OUT0 .. OUTn
    let ins = makePort numIn CommonTypes.PortType.Input _id compType wIn InOut
    let outs = makePort numOut CommonTypes.PortType.Output _id compType wOut InOut

    //Create any component specific ports
    let (leftPort, rightPort, botPort) = getExPorts symType bot left right _id compType wIn wOut
    
    //Create the map
    let portMap = getPortMap ins leftPort outs rightPort botPort (l, r, t, b)

    // ------- Symbol Creation ------ ///
    {
        TopL = pos
        BotR = botR
        Id = _id
        Type = compType
        Name = name
        Highlight = "gainsboro"
        PortHighlight = false
        Rotation = 0
        Scale = {X = 0.;Y = 0.}
        GenericType = symType
        PortMap = portMap
    }


//-----------------------Skeleton Message type for symbols---------------------//


/// Dummy function for test. Creates 4 NAND gates with 2 input, 1 output.
let init () =
    //use for checking rom/ram
    let memory = {
        CommonTypes.Memory.AddressWidth = 10 
        CommonTypes.Memory.WordWidth = 10
        CommonTypes.Memory.Data = [(1L, 0L); (2L, 0L); (3L, 0L)] |> Map.ofList
    }
    [
        (CreateNewSymbol (CommonTypes.ComponentType.SplitWire 4) 1 4 {X = 10.; Y = 0.})
        (CreateNewSymbol (CommonTypes.ComponentType.Nand) 2 1 {X = 200.; Y = 50.})
        (CreateNewSymbol (CommonTypes.ComponentType.Mux2) 2 1 {X = 300.; Y = 50.})
        (CreateNewSymbol (CommonTypes.ComponentType.Demux2) 1 2 {X = 400.; Y = 50.})
        (CreateNewSymbol (CommonTypes.ComponentType.Input 1) 0 1 {X = 200.; Y = 200.})
        (CreateNewSymbol (CommonTypes.ComponentType.Decode4) 2 4 {X = 10.; Y = 200.})
        (CreateNewSymbol (CommonTypes.ComponentType.Register 1) 1 1 {X = 500.; Y = 200.})
        (CreateNewSymbol (CommonTypes.ComponentType.Register 2) 1 1 {X = 600.; Y = 200.}) //To show getbuswidth interface func
        (CreateNewSymbol (CommonTypes.ComponentType.DFFE) 1 1 {X = 200.; Y = 300.})
        (CreateNewSymbol (CommonTypes.ComponentType.RAM memory) 1 1 {X = 400.; Y = 300.})
    ]
    , Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | Add (compType, pagePos, numIn, numOut) ->
        (CreateNewSymbol compType numIn numOut pagePos) :: model, Cmd.none

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
                match props.Obj.GenericType with
                | Wires -> if props.Obj.Highlight = "lightblue" then
                                "purple"
                            else
                                "darkgrey"
                | _ -> props.Obj.Highlight

            let labels : ReactElement list = 
                props.Obj
                |> mapSetup
                |> List.map(fun (i, k) ->
                    (drawText (displace -10. i props.Obj).X (displace -10. i props.Obj).Y "6px")[str <| sprintf "%s" (getPortName k)])

            let wires : ReactElement list =
            //line should be (port.x, port.y), (mid.x, port.y), (mid.x, mid.y)
                props.Obj
                |> mapSetup
                |> List.map(fun (i, _) ->
                    polyline[
                        Points (sprintf "%f,%f %f,%f %f,%f" i.X i.Y (midSymX props.Obj) i.Y (midSymX props.Obj) (midSymY props.Obj))
                        SVGAttr.Stroke color
                        SVGAttr.Fill "none"
                    ][])

            let triangles : ReactElement list =
                props.Obj
                |> mapSetup
                |> List.map(fun (i, _) -> (drawPolygon (triangleCoords i props.Obj) color color 1.)[])
            
            let io : ReactElement = drawPolygon (tagCoords props.Obj) "black" color 0.5 []

            let displayBox : ReactElement =
                rect[
                    X props.Obj.TopL.X
                    Y props.Obj.TopL.Y
                    SVGAttr.Height (getHWObj props.Obj |> fst)
                    SVGAttr.Width (getHWObj props.Obj |> snd)
                    SVGAttr.Fill color
                    SVGAttr.Stroke "black"
                    SVGAttr.StrokeWidth 0.5][]

            let title = drawText (midSymX props.Obj) (midSymY props.Obj) "10px"[str <| sprintf "%A" props.Obj.Name]
            
            let drawInvert =
                genMapList props.Obj.PortMap (List.filter(fun (_, k) -> isPortInverse k))
                |> List.map(fun (i, _) -> drawCircle props.Obj i color "black" 1. 0.5[])

            let ports =
                if props.Obj.PortHighlight then
                    props.Obj
                    |> mapSetup
                    |> List.map(fun (i, _) -> drawCircle props.Obj i "deepskyblue" "deepskyblue" 0.4 1.[])
                else []
            
            let symDraw = 
                match props.Obj.GenericType with
                | Wires -> List.concat [wires; triangles]
                | IO -> [io]
                | _ -> [displayBox]
            
            g[](List.concat [symDraw; labels; drawInvert; ports; [title]])
            
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

///Initialises the model for a port search by converting every symbol to a list portmap
let initPortSearch (symModel: Model) : (XYPos * Portinfo Option) list = 
    symModel |> List.collect ((fun x -> x.PortMap) >> (Map.toList))

///Gets a portinfo object from the model where PortId = pId
let getPortinfo (symModel: Model) (pId : CommonTypes.PortId) =
    initPortSearch symModel
    |> List.map (fun (_, k) -> (k, getPortId k))
    |> List.tryFind (fun (_, id) -> id = pId)
    |> function
    | Some (Some k, _) -> k
    | Some _ ->  failwithf "Unexpected error in getPortinfo"
    | None -> failwithf "Error in getPortinfo: couldn't find portID"

//-----------------------Interface functions--------------------//

///Searches through the whole model until the port is found and retruns the position of that port
let getPortCoords (symModel: Model) (pId : CommonTypes.PortId) : XYPos = 
    initPortSearch symModel
    |> List.map (fun (v, k) -> (v, getPortId k))
    |> List.tryFind (fun (_, k) -> k = pId)
    |> function
    | Some x -> fst x
    | None -> failwithf "Error in getPortCoords: couldn't find portID"

///Get bounding box of a single symbol based on the ID.      
let getBoundingBox symModel symID =
    List.tryFind (fun x -> x.Id = symID) symModel
    |> function 
    | Some x -> (x.TopL, x.BotR)
    | None -> failwithf "Could not get bounding box"

///Returns all symbols in the model in the form (ID, bounding box topLeft, bounding box botRight)
let getBoundingBoxes (symModel : Model) (startCoord : XYPos) : (CommonTypes.ComponentId * XYPos * XYPos) list =
    List.map (fun sym -> (sym.Id, sym.TopL, sym.BotR)) symModel

///Finds the portType of a specific port
let getPortType (symModel: Model) (pId : CommonTypes.PortId) : CommonTypes.PortType =
    (getPortinfo symModel pId).Port.PortType

///Finds if a position lies on a port. Returns Some(position, portId) if found, none otherwise.
let isPort (symModel : Model) (pos : XYPos) : (XYPos * CommonTypes.PortId) Option =
    symModel
    |>initPortSearch
    |> List.tryFind (fun (v, _) -> testBox v pos)
    |> function
    | Some(v, Some k) -> Some(v, (CommonTypes.PortId (k.Port.Id)))
    | _ -> None

//Returns a list of Port Ids for a given symbol
let getPortIds (model : Model) (sId : CommonTypes.ComponentId) : CommonTypes.PortId list = 
    model
    |> List.tryFind (fun sym -> sym.Id = sId)
    |> function
    | Some sym -> genMapList sym.PortMap (List.map (fun (_, k) -> getPortId k))
    | None -> failwithf "Error in getPortIds, couldn't find symbol"

let getPortWidth (model : Model) (pId : CommonTypes.PortId) : int = 
    (getPortinfo model pId).Width

let getHostId (model : Model) (pId : CommonTypes.PortId) : CommonTypes.ComponentId =
    CommonTypes.ComponentId ((getPortinfo model pId).Port.HostId)

type Edge = Top | Bottom | Left | Right
let getPortEdge (model : Model) (pId : CommonTypes.PortId) : Edge =
    let pos = getPortCoords model pId
    let sym = List.item 0 (List.filter (fun sym -> sym.Id = getHostId model pId) model)
    if pos.X = sym.BotR.X then Right
    elif pos.Y = sym.TopL.Y then Top
    elif pos.Y = sym.BotR.Y then Bottom
    else Left

//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

