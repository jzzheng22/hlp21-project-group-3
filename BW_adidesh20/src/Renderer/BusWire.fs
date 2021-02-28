module BusWire

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers


//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

/// type for buswires
/// for demo only. The real wires will
/// connect to Ports - not symbols, where each symbol has
/// a number of ports (see Issie Component and Port types) and have
/// extra information for highlighting, width, etc.
/// NB - how you define Ports for drawing - whether they correspond to
/// a separate datatype and Id, or whether port offsets from
/// component coordinates are held in some other way, is up to groups.
type Wire = {
    Id: CommonTypes.ConnectionId 
    SourcePortId: CommonTypes.PortId
    TargetPortId: CommonTypes.PortId
    Vertices: XYPos list
    BoundingBoxes: (CommonTypes.ConnectionId * XYPos * XYPos) list
    Width: int
    Highlight: bool
    }

type Model = {
    Symbol: Symbol.Model
    WX: Wire list
    Color: CommonTypes.HighLightColor
    }

//----------------------------Message Type-----------------------------------//

/// Messages to update buswire model
/// These are OK for the demo - but not the correct messages for
/// a production system. In the real system wires must connection
/// to ports, not symbols. In addition there will be other changes needed
/// for highlighting, width inference, etc
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (CommonTypes.PortId * CommonTypes.PortId)
    | DeleteWires of CommonTypes.ConnectionId list
    | HighlightWires of CommonTypes.ConnectionId list
    | MoveWires of CommonTypes.ConnectionId list * XYPos
    | SetColor of CommonTypes.HighLightColor
    | MouseMsg of MouseT

//-------------------Helpers for functions------------------------//

let getBoundingBoxes (wModel: Model) (mouseCoord: XYPos): (CommonTypes.ConnectionId * XYPos * XYPos) list =
    let bb =
        wModel.WX
        |> List.collect (fun w -> w.BoundingBoxes)
    bb

/// Takes Source and Target positions of a wire and returns the 
/// vertices of the path it should follow
///
/// NOTE: Currently only supports inputs on left, outputs on right
let routeWire (sourcePos: XYPos) (targetPos: XYPos) : XYPos list =
    let diff = Symbol.posDiff targetPos sourcePos

    if diff.X >= 0. then // Three Segement Case
        let endPosSeg0 = {sourcePos with X = sourcePos.X + (diff.X/2.)}
        let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y }
        [sourcePos;endPosSeg0;endPosSeg1;targetPos]

    else // Five Segment Case
        let xOffset = 15. // Length of Horizontal line coming out of/going into port
        let endPosSeg0 = {sourcePos with X = sourcePos.X + xOffset}
        let endPosSeg1 = {endPosSeg0 with Y = endPosSeg0.Y + (diff.Y/2.)}
        let endPosSeg2 = {endPosSeg1 with X = endPosSeg1.X + diff.X - (2.*xOffset)}
        let endPosSeg3 = {endPosSeg2 with Y = endPosSeg2.Y + (diff.Y/2.)}
        [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;endPosSeg3;targetPos]

let posLeftMost (posList: XYPos list) : XYPos list =
    let posX pos = pos.X
    posList
    |> List.sortBy posX 
let posRightMost (posList: XYPos list) : XYPos list =
    let posX pos = pos.X
    posList
    |> List.sortByDescending posX
let posHighest (posList: XYPos list) : XYPos list =
    let posY pos = pos.Y
    posList
    |> List.sortBy posY
let posLowest (posList: XYPos list) : XYPos list =
    let posY pos = pos.Y
    posList
    |> List.sortByDescending posY

/// Calculates and returns a list of bounding boxes for all the segments of a wire
let singleWireBoundingBoxes (vertices: XYPos list) (wID: CommonTypes.ConnectionId): (CommonTypes.ConnectionId * XYPos * XYPos) list =
    let bbDist = 10. // Distance of Bounding Box Outline from wire
    let lineToBox (startPos: XYPos) (endPos: XYPos):  XYPos * XYPos =
        if startPos.Y = endPos.Y then // Horizontal Segment
            let leftmost = (posLeftMost [startPos;endPos]).Head
            let rightmost = (posRightMost [startPos;endPos]).Head
            {leftmost with Y = leftmost.Y - bbDist},{rightmost with Y = rightmost.Y + bbDist}
        else // Vertical Segment
            let highest = (posHighest [startPos;endPos]).Head
            let lowest = (posLowest [startPos;endPos]).Head
            {highest with X = highest.X - bbDist},{lowest with X = highest.X + bbDist}

    vertices
    |> List.pairwise
    |> List.map (fun x -> lineToBox (fst x) (snd x))
    |> List.map (fun (topL,botR) ->  (wID,topL,botR))


/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire option =
    wModel.WX
    |> List.tryFind (fun wire -> wire.Id = wId)

/// Creates a new wire 
let makeNewWire (model: Model) (srcPortId: CommonTypes.PortId) (tgtPortId: CommonTypes.PortId) (width: int): Wire = 
    let wId = CommonTypes.ConnectionId (Helpers.uuid())
    let newVertices = routeWire (Symbol.getPortCoords model.Symbol srcPortId) (Symbol.getPortCoords model.Symbol tgtPortId)
    let newBB = singleWireBoundingBoxes newVertices wId
  
    {
        Id = wId
        SourcePortId = srcPortId
        TargetPortId = tgtPortId
        Vertices = newVertices
        BoundingBoxes = newBB
        Width = width
        Highlight = false
    }

//----------------Render/View Functions----------------//

type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    Vertices: XYPos list
    Width: int
    Highlight: bool
    ColorP: string
    StrokeWidthP: string }


/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.
let singleWireView =        
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let singleSegmentView (segPos: XYPos*XYPos) =
                let SrcP = fst segPos
                let TgtP = snd segPos
                line [
                    X1 SrcP.X
                    Y1 SrcP.Y
                    X2 TgtP.X
                    Y2 TgtP.Y
                    // Qualify these props to avoid name collision with CSSProp
                    SVGAttr.Stroke (if props.Width = 1 then props.ColorP else "purple")
                    SVGAttr.StrokeWidth props.StrokeWidthP
                    SVGAttr.StrokeLinecap "round"] []
            let widthAnnotation = 
                let textPos = Symbol.posAdd props.Vertices.Head {X = 5. ; Y = 5.}
                text [
                    X textPos.X
                    Y textPos.Y
                    Style [
                        TextAnchor "middle"
                        DominantBaseline "hanging"
                        FontSize "10px"
                        FontWeight "Bold"
                        Fill "Black"
                    ]
                ] [str <| sprintf "%i" props.Width]   
            let highlightCircles =
                    let srcPortPos = List.head props.Vertices
                    let tgtPortPos = List.last props.Vertices
                    if props.Highlight = false then
                        []
                    else
                        [
                        circle [
                           Cx (srcPortPos.X + 3.)
                           Cy srcPortPos.Y
                           R 3.

                           SVGAttr.Fill "deepskyblue"
                           SVGAttr.Stroke "deepskyblue"
                           SVGAttr.Opacity 0.4
                           SVGAttr.StrokeWidth 1][]
                           ;
                        circle [
                            Cx (tgtPortPos.X - 3.)
                            Cy tgtPortPos.Y
                            R 3.

                            SVGAttr.Fill "deepskyblue"
                            SVGAttr.Stroke "deepskyblue"
                            SVGAttr.Opacity 0.4
                            SVGAttr.StrokeWidth 1][]
                        ]
            
            let segments =
                List.pairwise props.Vertices
                |> List.map singleSegmentView
            (widthAnnotation::segments)@highlightCircles
            |> ofList)



let view (model:Model) (dispatch: Dispatch<Msg>)=
    let allBB = getBoundingBoxes model {X=100.;Y=100.}
    printfn "viewing"
    printfn "%A" allBB
    let wires = 
        model.WX
        |> List.map (fun w ->
            let props = {
                key = w.Id
                WireP = w
                Vertices = w.Vertices
                Width = w.Width
                Highlight = w.Highlight
                ColorP = model.Color.Text()
                StrokeWidthP = "2px" }
            singleWireView props)
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    g [] [(g [] wires); symbols]

/// dummy init for testing: real init would probably start with no wires.
/// this initialisation is not realistic - ports are not used
/// this initialisation depends on details of Symbol.Model type.
let init n () =
    let symbols, cmd = Symbol.init()
    let symIds = List.map (fun (sym:Symbol.Symbol) -> sym.Id) symbols
    let rng = System.Random 0
    let ports = Symbol.initPortSearch symbols 
    let outPort = (List.tryFind (fun (p: Symbol.Portinfo) -> p.Port.PortType = CommonTypes.Output) ports).Value
    let inPort = (List.tryFind (fun (p: Symbol.Portinfo) -> p.Port.PortType = CommonTypes.Input && p.Port.HostId <> outPort.Port.HostId) ports).Value
    let id = CommonTypes.ConnectionId (Helpers.uuid())
    let vert = routeWire (Symbol.getPortCoords symbols (CommonTypes.PortId outPort.Port.Id)) (Symbol.getPortCoords symbols (CommonTypes.PortId inPort.Port.Id))
    let bb = singleWireBoundingBoxes vert id
    let testWire: Wire = 
        {
            Id = id
            SourcePortId = CommonTypes.PortId outPort.Port.Id
            TargetPortId = CommonTypes.PortId inPort.Port.Id
            Vertices = vert
            BoundingBoxes = bb
            Width = Symbol.getPortWidth symbols (CommonTypes.PortId outPort.Port.Id)
            Highlight = false
        }
    [testWire]
    |> (fun wires -> {WX=wires;Symbol=symbols; Color=CommonTypes.Red},Cmd.none)

//-------------------Helpers for Update Function-------------------//
// WIRE RULES: Must have signature Model->Wire->bool

/// Checks if a wire connects an Output Port to an Input Port
let ruleOutToIn (model: Model) (wire: Wire) : bool = 
    match Symbol.getPortType model.Symbol wire.SourcePortId , Symbol.getPortType model.Symbol wire.TargetPortId with 
    | CommonTypes.Output , CommonTypes.Input -> true
    | _ , _ -> false

/// Checks if a new wire does not already exist in the model
let ruleUnique (model: Model) (wire: Wire) : bool =
    model.WX
    |> List.filter (fun w -> w.SourcePortId = wire.SourcePortId && w.TargetPortId = wire.TargetPortId)
    |> List.isEmpty

/// Checks if the Width of the Source Port is equal to that of the Target Port
let ruleWidthEquality (model: Model) (wire: Wire) : bool =
    if Symbol.getPortWidth model.Symbol wire.SourcePortId = Symbol.getPortWidth model.Symbol wire.TargetPortId then 
        true
    else 
        false

let wireRuleList =
    [
        ruleOutToIn;
        ruleUnique;
        ruleWidthEquality
    ]
    
/// Verifies if a supplied wire is compliant with the rules for wires.
/// Returns wire wrapped in a Wire Option if it is compliant, otherwise returns None.
let verifyWire (model: Model) (wire: Wire): Wire option = 
    wireRuleList 
    |> List.map (fun f -> f model wire)
    |> List.contains false
    |> function true -> None | false -> Some wire

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        let wList = 
            model.WX
            |> List.map (fun w -> 
                let newVertices = routeWire (Symbol.getPortCoords sm w.SourcePortId) (Symbol.getPortCoords sm w.TargetPortId)
                let newBB = singleWireBoundingBoxes newVertices w.Id
                {w with 
                    Vertices = newVertices
                    BoundingBoxes = newBB })
        {model with Symbol=sm; WX = wList}, Cmd.map Symbol sCmd

    | AddWire (portId1,portId2) ->
        let unverifiedWire =
            match Symbol.getPortType model.Symbol portId1 , Symbol.getPortType model.Symbol portId2 with
            | CommonTypes.Output , CommonTypes.Input -> makeNewWire model portId1 portId2 (Symbol.getPortWidth model.Symbol portId1) // Wire was drawn from Output to Input
            | CommonTypes.Input , CommonTypes.Output -> makeNewWire model portId2 portId1 (Symbol.getPortWidth model.Symbol portId2) // Wire was drawn from Input to Output
            | _ , _ -> makeNewWire model portId1 portId2 (Symbol.getPortWidth model.Symbol portId1) // Invalid port combination, will be caught by verifyWire
            
        match verifyWire model unverifiedWire with
        | Some w -> {model with WX = w::model.WX}, Cmd.none
        | None -> model, Cmd.none

    | DeleteWires wIdList -> 
        let wList =
            model.WX
            |> List.filter (fun w -> List.contains w.Id wIdList = false)
        {model with WX = wList}, Cmd.none

    | HighlightWires wIdList -> 
        let wList =
            model.WX
            |> List.map (fun w ->
                if List.contains w.Id wIdList then
                    {w with Highlight = true}
                else 
                    {w with Highlight = false}
            )
        {model with WX = wList}, Cmd.none
    | MoveWires (_,_) -> model, Cmd.none // Not implemented yet

    | SetColor c -> {model with Color = c}, Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))

//---------------Other interface functions--------------------//

/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Used
/// to determine which wire (if any) to select on a mouse click
let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option = 
    failwith "Not implemented"
(*
let getBoundingBoxes (wModel: Model) (mouseCoord: XYPos): (CommonTypes.ConnectionId * XYPos * XYPos) list =
    let bb =
        wModel.WX
        |> List.collect (fun w -> w.BoundingBoxes)
    bb
*)

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"



    



