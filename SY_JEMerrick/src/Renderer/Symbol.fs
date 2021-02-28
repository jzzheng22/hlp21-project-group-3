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
let STD_HEIGHT = 40.
let HW_RATIO = 0.9
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
        PortMap : Map<XYPos, Portinfo Option>
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
    | Move of sIDList : CommonTypes.ComponentId list * pagePos : XYPos
    | Add of compType: CommonTypes.ComponentType * pagePos : XYPos * numIn : int * numOut : int
    | Delete of sIdList : CommonTypes.ComponentId list
    | Highlight of sIdList: CommonTypes.ComponentId list
    | HighlightPorts of sId : CommonTypes.ComponentId
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
    let Box = (addXYVal portPos -7., addXYVal portPos 7.);
    let topL = Box |> fst
    let botR = Box |> snd
    topL.X <= coord.X && topL.Y <= coord.Y && botR.X >= coord.X && botR.Y >= coord.Y

///Tests whether a position is within a certain amount of pixels to a label
let testLabelBox (portPos : XYPos) (coord : XYPos) (sym : Symbol) : bool =
    let transl = displace -5. portPos sym
    testBox {X = fst transl; Y = snd transl} coord

///Calculates the port position where
///i = int indicating the index of the port on a side
///n = int indicating the total number of ports on a side
///topL = the top left position of the box
///botR = the bottom right position of the box
let portPos (i : int) (n : int) (topL : XYPos) (botR : XYPos) : XYPos = 
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
    let transM = transp m2
    m1 |> List.map (fun x -> getPairs x transM) |> List.map (List.map(fun (x, y) -> mulList x y))
    
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

///Transforms a map with a given transformation and centre (2 parameters)
let mapTrans myMap transf param centre = 
    myMap 
    |> Map.toList 
    |> List.map (fun (k, x) -> (transf k param centre, x)) 
    |> Map.ofList

///Transforms a map with a given transformation (1 parameter)
let mapAlter myMap transf param = 
    myMap 
    |> Map.toList 
    |> List.map (fun (k, x) -> (transf k param, x)) 
    |> Map.ofList

///Generic transformation function that takes in a transformation function, symbol and transformation
///It returns an updated symbol object
let trans func sym trans =
    let centre = midSym sym
    let rotTopL = func sym.TopL trans centre
    let rotBotR = func sym.BotR trans centre
    let newBox = getNewBox rotTopL rotBotR
    { sym with
        TopL = newBox |> fst
        BotR = newBox |> snd
        PortMap = mapTrans sym.PortMap func trans centre
    }

///Finds the log base 2 of an int and rounds up to the nearest int
let log2 (n : int) : int = (log(float n) / log(2.)) |> ceil |> int

///Displace an object by a float
let displaceN (sym : Symbol) (i : XYPos) (n : float) : (float * float) = displace n i sym

///Displace an object and retrieve the X coordinate
let displaceNX (sym : Symbol) (i : XYPos) (n : float) : float = displaceN sym i n |> fst

///Displace an object and retrieve the Y coordinate
let displaceNY (sym : Symbol) (i : XYPos) (n : float) : float = displaceN sym i n |> snd

///Creates the list of positions for ports to slot in to on each side, returns in tupled list form
let makePosList (n : int) (nBot : int) (topL : XYPos) (botR : XYPos) : (XYPos list * XYPos list * XYPos list * XYPos list) =
    let l = 
        [0..n - 1]
        |> List.map (fun i -> {X = topL.X; Y = (portPos i (int(n)) topL botR).Y})
    let r = 
        [0..n - 1]
        |> List.map (fun i -> {X = botR.X; Y = (portPos i (int(n)) topL botR).Y})
    let t = 
        [0..nBot - 1]
        |> List.map (fun i -> {X = (portPos i nBot topL botR).X; Y = topL.Y})
    let b = 
        [0..nBot - 1]
        |> List.map (fun i -> {X = (portPos i nBot topL botR).X; Y = botR.Y})
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

///Coordinates to create the tag shape used for input/output symbols
let tagCoords (sym : Symbol) : string = 
    let midX = midSymX sym
    let midY = midSymY sym
    (sprintf "%f,%f %f,%f %f,%f %f,%f %f,%f" 
        sym.TopL.X (midY - RAD) 
        sym.TopL.X (midY + RAD) 
        (midX + RAD) (midY + RAD) 
        (sym.BotR.X) midY 
        (midX + RAD) (midY - RAD))

///Returns a map with only positions that have Some port assigned to them
let getUsedPorts (portMap : Map<XYPos, Portinfo Option>) : Map<XYPos, Portinfo Option> =
    portMap |> Map.toList |> List.filter (fun (v, k) -> k <> None) |> Map.ofList

let numExPorts (symType : SymbolType) (numIn : int) : (int * int * int) = 
    match symType with
    | Mux -> (0, 0, log2 numIn)
    | Adder -> (1, 1, 0)
    | FF -> (1, 0, 0)
    | _ -> (0, 0, 0) //Logic, memory, wires, IO do not require extra ports

///Converts a list of positions and a list of portinfo into a portinfo option list
///We want to create Some(X) for every index in the list of positions that will have a port
///And None for every index in the list of positions that will not
let mapPorts (posList : XYPos list) (portList : Portinfo list) : Portinfo Option list = 
    posList
    |> List.mapi (fun i _ -> if i < List.length portList then Some (List.item i portList) else None)

///Converts the list of portinfo and the list of XYPositions into a map of <XYPos, Portinfo Option>
let getPortMap (ins: Portinfo list) (leftPort : Portinfo list) (outs : Portinfo list) (rightPort : Portinfo list) (botPort : Portinfo list) (l,r,t,b) : Map<XYPos, Portinfo Option> = 
    let left = List.concat [ins; leftPort] |> mapPorts l
    let right =  List.concat [outs; rightPort] |> mapPorts r
    let top = [] |> mapPorts t 
    let bot = botPort |> mapPorts b
    
    List.zip  (List.concat [l; r; t; b]) (List.concat[left; right; top; bot])
    |> Map.ofList

let findPort (sym: Symbol) (pId : CommonTypes.PortId) = 
    sym.PortMap
    |> Map.toList
    |> List.find (fun (v, k) -> getPortId k  = pId)

//Swaps two values in a map
let swapPortt portMap k1 k2 v1 v2 =
    portMap
    |> Map.change k1 (fun _ -> Some v2)
    |> Map.change k2 (fun _ -> Some v1)

let swapPort (sym : Symbol) (pId : CommonTypes.PortId) (pagePos : XYPos) = 
    //The port we are moving
    let port = findPort sym pId
       
    //The index we want to move the port to is the one closest to the mouse
    sym.PortMap
    |> Map.toList
    |> List.map (fun (v, k) -> (absDiff pagePos v, (v, k)))
    |> List.minBy fst
    |> snd
    |> function
    | (k, x) -> swapPortt sym.PortMap k (port |> fst) x (port |> snd)  
                

//---------------------------------------------------------------------------//
//----------------------helper initialisation funcs--------------------------//
//---------------------------------------------------------------------------//



/// Creates Symbol.PortInfo object. 
///
/// i : Index of the port (e.g. INPUT1 : i = 0).
/// portType : the portType of the port (Input/Output).
/// genPort :  the generic porttype used to create any extra ports/labels
/// compId : the Id of the component associated with the port
/// compType : the type of component associated with the port - used to determine inverters
/// w : the port width
let CreatePortInfo (i : int) (portType : CommonTypes.PortType) (genPort : genericPort) (compId : CommonTypes.ComponentId) (compType : CommonTypes.ComponentType) (w : int) : Portinfo = 
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
            | _ -> match portType with 
                    | CommonTypes.PortType.Input -> sprintf "IN %d" i
                    | CommonTypes.PortType.Output -> sprintf "OUT %d" i 

        Invert = 
            match portType with
            | CommonTypes.PortType.Output when compType = CommonTypes.ComponentType.Not
                                            || compType = CommonTypes.ComponentType.Nand
                                            || compType = CommonTypes.ComponentType.Nor
                                            || compType = CommonTypes.ComponentType.Xnor -> true;
            | _ -> false;
        
        width = w
    }


let makePort (range : int) (port : CommonTypes.PortType) (_id : CommonTypes.ComponentId) (compType : CommonTypes.ComponentType) (width : int) (pType : genericPort) = 
    List.map(fun x -> CreatePortInfo x port pType _id compType width) [0 .. range - 1]

let getExPorts (symType: SymbolType) (bot : int) (left : int) (right : int) (_id : CommonTypes.ComponentId) (compType : CommonTypes.ComponentType) (wIn : int) (wOut : int) = 
    match symType with
    | Mux ->  ([], [], makePort bot CommonTypes.PortType.Input _id compType wIn Select)
    | Adder -> ((makePort left CommonTypes.PortType.Input _id compType wIn Carry), (makePort right CommonTypes.PortType.Output _id compType wOut Carry), [])
    | FF -> ((makePort left CommonTypes.PortType.Input _id compType wIn Enable), [], [])
    | _ -> ([],[],[])

///Creates a new object of type symbol from component type, position, number of inputs, and number of outputs
let CreateNewSymbol (compType : CommonTypes.ComponentType) (numIn : int) (numOut : int) (pos : XYPos) : Symbol =
    
    //Getting type info for symbol/port construction
    let (name, wIn, wOut, symType) = typeToInfo compType

    //Finding the component specific extra ports required
    let (left, right, bot) = numExPorts symType numIn

    //Intermediate calculations
    let n = max (numIn + left) (numOut + right) |> float //The max number of ports initially will always be on the left or right of the box
    let nBot = if bot > 0 then bot else (int (HW_RATIO * n)) //If there is no ports on the top/bot, the component should still have ports in the portmap
    let h = STD_HEIGHT * n
    let w =  if bot <= 0 then (HW_RATIO * h) else ((float nBot) * STD_HEIGHT * 1.5) //Width is either standard, or based on number of ports on the bottom
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
    let myMap = getPortMap ins leftPort outs rightPort botPort (l, r, t, b)

    //---------------------------------------------------------------------------------------------//
    //----FOR DEMO PURPOSES ONLY - THIS IS AN EXACT COPY OF THE TRANS FUNCTION USED IN MESSAGES----//
    //---------------------------------------------------------------------------------------------//
    let centre = midXY pos botR
    let rot  = 0
    let rotTopL = rotateCoords pos rot centre
    let rotBotR = rotateCoords botR rot centre
    let rotSlots = mapTrans myMap rotateCoords rot centre

    let scale = {X = 1.0; Y = 1.0}
    let scaleTopL = scaleCoords rotTopL scale centre
    let scaleBotR = scaleCoords rotBotR scale centre
    let scaleSlots = mapTrans rotSlots scaleCoords scale centre
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
        Rotation = rot
        Scale = scale
        genericType = symType
        PortMap = scaleSlots
    }


//-----------------------Skeleton Message type for symbols---------------------//


/// Dummy function for test. Creates 4 NAND gates with 2 input, 1 output.
let init () =
    //use for checking rom/ram
    let myMap = [(1L, 0L); (2L, 0L); (3L, 0L)] |> Map.ofList
    let memory = {
        CommonTypes.Memory.AddressWidth = 10 
        CommonTypes.Memory.WordWidth = 10
        CommonTypes.Memory.Data = myMap
    }
    //4 logic gates
    List.allPairs [1..2] [1..2]
    |> List.map (fun (x,y) -> {X = float (x*64+30); Y=float (y*64+30)})
    |> List.map (fun pos -> (CreateNewSymbol (CommonTypes.ComponentType.Not) 1 1 pos)) 
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
                    PortMap = mapAlter sym.PortMap posAdd translate
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
                    PortMap = swapPort sym pId pagePos
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
                props.Obj.PortMap
                |> getUsedPorts
                |> Map.toList
                |> List.map(fun (i, k) ->
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
                    ][str <| sprintf "%s" (getPortName k)])

            let wires : ReactElement list =
            //line should be (port.x, port.y), (mid.x, port.y), (mid.x, mid.y)
                props.Obj.PortMap
                |> Map.toList
                |> List.map(fun (i, k) ->
                    polyline[
                        Points (sprintf "%f,%f %f,%f %f,%f" i.X i.Y (midSymX props.Obj) i.Y (midSymX props.Obj) (midSymY props.Obj))
                        Style[
                            Stroke color
                            Fill "none"
                        ]
                    ][])

            let triangles : ReactElement list =
                //line should be (port.x, port.y), (mid.x, port.y), (mid.x, mid.y)
                    props.Obj.PortMap
                    |> Map.toList
                    |> List.map(fun (i, k) ->
                        polygon[
                            Points (sprintf "%f,%f %f,%f %f,%f" i.X (i.Y + RAD) (i.X + RAD) i.Y i.X (i.Y - RAD))
                            Style[
                                Stroke color
                                Fill color
                            ]
                        ][])
            
            let io : ReactElement list =
                [
                    polygon[
                        Points (tagCoords props.Obj)
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
                props.Obj.PortMap
                |> Map.toList
                |> List.filter(fun (_, k) -> isPortInverse k)
                |> List.map(fun (i, k) ->
                    circle[
                        Cx (displaceNX props.Obj i 3.)
                        Cy (displaceNY props.Obj i 3.)
                        R RAD
                        SVGAttr.Fill color
                        SVGAttr.Stroke "black"
                        SVGAttr.StrokeWidth 0.5][])

            let ports =
                if props.Obj.PortHighlight then
                    props.Obj.PortMap
                    |> getUsedPorts
                    |> Map.toList
                    |> List.map(fun (i, k) ->
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
let initPortSearch (symModel: Model) : (XYPos * Portinfo Option) list = 
    symModel
    |> List.map (fun x -> x.PortMap)
    |> List.map (fun x -> Map.toList x)
    |> List.concat

//TODO - REMOVE - ONLY USED BY BUSWIRE - Currently connects every input 0 to each other.
let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.TopL)
    


//-----------------------Interface functions--------------------//

///Searches through the whole model until the port is found and retruns the position of that port
let getPortCoords (symModel: Model) (pId : CommonTypes.PortId) : XYPos = 
    initPortSearch symModel
    |> List.map (fun (v, k) -> (v, getPortName k))
    |> List.tryFind (fun (v, k) -> k = string pId)
    |> function
    | Some x -> x |> fst
    | None -> failwithf "Error couldn't find portID"

        
///Returns all symbols in the model in the form (ID, bounding box topLeft, bounding box botRight)
let getBoundingBoxes (symModel : Model) (startCoord : XYPos) : (CommonTypes.ComponentId * XYPos * XYPos) list =
    symModel
    |> List.map (fun sym -> (sym.Id, sym.TopL, sym.BotR))

///Finds the portType of a specific port
let getPortType (symModel: Model) (pId : CommonTypes.PortId) : CommonTypes.PortType =
    initPortSearch symModel
    |> List.map (fun (v, k) -> (k, getPortName k, v))
    |> List.tryFind (fun (_, id, _) -> id = string pId)
    |> function
    | Some (Some k, _, _) -> k.Port.PortType
    | Some (_, _, _) ->  failwithf "Unexpected error in getPortType"
    | None -> failwithf "Error couldn't find portID"

///Finds if a position lies on a port. Returns Some(position, portId) if found, none otherwise.
let isPort (symModel : Model) (pos : XYPos) : (XYPos * CommonTypes.PortId) Option =
    symModel
    |>initPortSearch
    |> List.tryFind (fun (v, k) -> testBox v pos)
    |> function
    | Some(v, Some k) -> Some(v, (CommonTypes.PortId (k.Port.Id)))
    | Some(_, None) -> None
    | None -> None

//Returns a list of Port Ids for a given symbol
let getPortIds (model : Model) (sId : CommonTypes.ComponentId) : CommonTypes.PortId list = 
    model
    |> List.tryFind (fun sym -> sym.Id = sId)
    |> function
    | Some sym -> sym.PortMap |> Map.toList |> List.map (fun (v, k) -> getPortId k)
    | None -> failwithf "Error, could not find symbol"

let getPortWidth (model : Model) (pId : CommonTypes.PortId) : int =
    initPortSearch model
    |> List.map (fun (v, k) -> (k, getPortId k, v))
    |> List.tryFind (fun (_, id, _) -> id = pId)
    |> function
    | Some (Some k, _, _) -> k.width
    | Some (_, _, _) ->  failwithf "Unexpected error in getPortWidth"
    | None -> failwithf "Error couldn't find portID"

//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"
