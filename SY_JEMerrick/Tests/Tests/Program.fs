
type PortType =
    | Input
    | Output

type XYPos = 
    {
        X : float
        Y : float
    }
type Port =
    {
        Id : string
        PortType : PortType
    }
type Portinfo = 
    {
        Port: Port
        slotPos : int
    }


///Symbol is unique for each component, and shares CommonTypes.ComponentId
///
///Two positions TopL, BotR completely define the shape (all shapes are rectangular)
type Symbol =
    {
        TopL: XYPos
        BotR: XYPos
        Id : string
        PortMap : XYPos list
        PortList : Portinfo list
    }

type Model = Symbol list

let addXYVal (xy : XYPos) (n : float) : XYPos = {X = xy.X + n; Y = xy.Y + n}

let findPos (port : Portinfo) (portMap : XYPos list) : XYPos =
    List.item port.slotPos portMap

///Finds whether a coordinate is within a port's bounding box
let testBox (portPos : XYPos) (coord : XYPos) : bool =
    let Box = (addXYVal portPos -1., addXYVal portPos 1.);
    let topL = Box |> fst
    let botR = Box |> snd
    topL.X <= coord.X && topL.Y <= coord.Y && botR.X >= coord.X && botR.Y >= coord.Y

let initPortSearch (symModel: Model) : Portinfo list = 
    symModel
    |> List.map(fun sym -> sym.PortList)
    |> List.concat

///Searches through the whole model until the port is found and retruns the position of that port
///This would be much faster if sheet gave me the component
let getPortCoords (symModel: Model) (pId : string) = 
    symModel
    |> List.tryFind(fun sym -> List.contains pId (List.map (fun x -> string(x.Port.Id)) sym.PortList))
    |> function
    | Some sym -> (findPos (List.find (fun x -> string(x.Port.Id) = pId) sym.PortList) sym.PortMap)
    | None -> failwithf "ERROR: Couldn't find port"

//Tries to find port object by portID, returns Some(portInfo) if found, else None
let portSearchID (symModel: Model) (pId : string) : Portinfo Option =
    initPortSearch symModel
    |> List.tryFind (fun port -> string(port.Port.Id) = pId)

///Returns all symbols in the model in the form (ID, bounding box topLeft, bounding box botRight)
let getBoundingBoxes (symModel : Model) (startCoord : XYPos) : (string * XYPos * XYPos) list =
    symModel
    |> List.map (fun sym -> (string(sym.Id), sym.TopL, sym.BotR))

let getPortType (symModel: Model) (pId : string) : PortType =
    portSearchID symModel pId
    |> function
    | Some port -> port.Port.PortType
    | None -> failwithf "ERROR: Couldn't find port"

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

let mySymbol1 = 
    {
        TopL = {X = 0.; Y = 0.}
        BotR = {X = 30.; Y = 30.}
        Id = "0"
        PortMap = [{X = 0.; Y = 10.}; {X = 0.; Y = 20.}; {X = 30.; Y = 10.} ]
        PortList = [{Port = {Id = "0"; PortType = Input}; slotPos = 0}; {Port = {Id = "1"; PortType = Input}; slotPos = 1}; {Port = {Id = "2"; PortType = Output}; slotPos = 2}]
    }

let mySymbol2 = 
    {
        TopL = {X = 50.; Y = 50.}
        BotR = {X = 100.; Y = 100.}
        Id = "1"
        PortMap = [{X = 50.; Y = 60.}; {X = 50.; Y = 70.}; {X = 100.; Y = 60.} ]
        PortList = [{Port = {Id = "3"; PortType = Input}; slotPos = 0}; {Port = {Id = "4"; PortType = Output}; slotPos = 1}; {Port = {Id = "5"; PortType = Output}; slotPos = 2}]
    }

let myModel = [mySymbol1; mySymbol2]

let testIsPort =
    if isPort myModel {X = 50.; Y = 60.} = Some({X = 50.; Y = 60.}, "3") then printf "Passsed isPort test1 \n" else printf "Failed isPort test1\n"
    if isPort myModel {X = 0.; Y = 0.} = None then printf "Passsed isPort test2 \n" else printf "Failed isPort test2\n"
    if isPort myModel {X = -1.; Y = 9.} = Some({X = 0.; Y = 10.}, "0") then printf "Passsed isPort test3\n" else printf "Failed isPort test3\n"
    if isPort myModel {X = 49.; Y = 71.} = Some({X = 50.; Y = 70.}, "4") then printf "Passsed isPort test4\n" else printf "Failed isPort test4\n"
    if isPort myModel {X = 1.; Y = 21.} = Some({X = 0.; Y = 20.}, "1") then printf "Passsed isPort test5\n" else printf "Failed isPort test5\n"


let testPortType =
    if getPortType myModel "0" = Input then printf "Passed getPortType test 1 \n" else printf "Failed getPortType test 1 \n"
    if getPortType myModel "0" <> Output then printf "Passed getPortType test 1 \n" else printf "Failed getPortType test 2 \n"
    if getPortType myModel "3" = Input then printf "Passed getPortType test 1 \n" else printf "Failed getPortType test 3 \n"
    if getPortType myModel "4" = Output then printf "Passed getPortType test 1 \n" else printf "Failed getPortType test 4 \n"
    
let testGetBoundingBoxes =
    if getBoundingBoxes myModel {X = 0.0; Y = 0.0} = [("0", {X = 0.0; Y = 0.0}, {X = 30.0; Y = 30.0}); ("1", {X = 50.0; Y = 50.0}, {X = 100.0; Y = 100.0})] then printf "Passed getBoundingBoxes test 1 \n" else printf "Failed getBoundingBoxes test 1 \n"
    

let testGetPortCoords =
    if getPortCoords myModel "0" = {X = 0.; Y = 10.} then printf "Passed getPortCoords test 1 \n" else printf "Failed getPortCoords test 1 \n"
    if getPortCoords myModel "1" = {X = 0.; Y = 20.} then printf "Passed getPortCoords test 2 \n" else printf "Failed getPortCoords test 2 \n"
    if getPortCoords myModel "2" = {X = 30.; Y = 10.} then printf "Passed getPortCoords test 3 \n" else printf "Failed getPortCoords test 3 \n"
    if getPortCoords myModel "3" = {X = 50.; Y = 60.} then printf "Passed getPortCoords test 4 \n" else printf "Failed getPortCoords test 4 \n"
    if getPortCoords myModel "4" = {X = 50.; Y = 70.} then printf "Passed getPortCoords test 5 \n" else printf "Failed getPortCoords test 5 \n"
    if getPortCoords myModel "5" = {X = 100.; Y = 60.} then printf "Passed getPortCoords test 6 \n" else printf "Failed getPortCoords test 6 \n"
    


