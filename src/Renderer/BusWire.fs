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


type WireSegment = {
    wId: CommonTypes.ConnectionId
    Index: int 
    SourcePos: XYPos
    TargetPos: XYPos
    BB: XYPos * XYPos
    Highlight: bool
    Width:int
    }

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
    SourcePortEdge: Symbol.Edge
    TargetPortId: CommonTypes.PortId
    TargetPortEdge: Symbol.Edge
    Segments: WireSegment list
    //Vertices: XYPos list
    //BoundingBoxes: (CommonTypes.ConnectionId * XYPos * XYPos) list
    Width: int
    Highlight: bool
    HighlightError : bool
    Manual: bool
    StartHoriz: bool
    EndHoriz: bool
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
    | MoveWires of CommonTypes.ConnectionId * int * XYPos
    | SetColor of CommonTypes.HighLightColor
    | MouseMsg of MouseT
    | UpdateWidth of (CommonTypes.ConnectionId * int Option) list
    | HighlightError of CommonTypes.ConnectionId list

//-------------------Helpers for functions------------------------//

/// Takes Source and Target ports of a wire and returns the 
/// vertices of the path it should follow
let routeWire (model: Model) (sourcePortId: CommonTypes.PortId) (targetPortId: CommonTypes.PortId) : XYPos list =
    let sourcePos = Symbol.getPortCoords model.Symbol sourcePortId
    let targetPos = Symbol.getPortCoords model.Symbol targetPortId
    let diff = Symbol.posDiff targetPos sourcePos
    let xOffset = 10. // Length of Horizontal line coming out of/going into port
    let yOffset = 10. // Length of Vertical line coming out of/going into port
    match Symbol.getPortEdge model.Symbol sourcePortId, Symbol.getPortEdge model.Symbol targetPortId with 
    | Symbol.Right,Symbol.Left -> 
        if diff.X >= 0. then // Three Segement Case
            let endPosSeg0 = {sourcePos with X = sourcePos.X + (diff.X/2.)}
            let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y }
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]
        
        else // Five Segment Case
            let endPosSeg0 = {sourcePos with X = sourcePos.X + xOffset}
            let endPosSeg1 = {endPosSeg0 with Y = endPosSeg0.Y + (diff.Y/2.)}
            let endPosSeg2 = {endPosSeg1 with X = endPosSeg1.X + diff.X - (2.*xOffset)}
            let endPosSeg3 = {endPosSeg2 with Y = endPosSeg2.Y + (diff.Y/2.)}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;endPosSeg3;targetPos]

    | Symbol.Right,Symbol.Right ->
        if diff.X >= 0. then // Three Segement Case
            let endPosSeg0 = {sourcePos with X = sourcePos.X + (diff.X + xOffset)}
            let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y }
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]
        
        else 
            let endPosSeg0 = {sourcePos with X = sourcePos.X + xOffset}
            let endPosSeg1 = {endPosSeg0 with Y = endPosSeg0.Y + (diff.Y)}
            
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]
            
    | Symbol.Right,Symbol.Bottom ->
        if diff.X > 0. && diff.Y > 0. then // Two Segment Case
            let endPosSeg0 = {sourcePos with X = sourcePos.X + (diff.X/2.)}
            let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y + yOffset}
            let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else if diff.X > 0. && diff.Y < 0. then // Four Segment Case
            let endPosSeg0 = {sourcePos with X = targetPos.X}
            [sourcePos;endPosSeg0;targetPos]
        else if diff.X < 0. && diff.Y > 0. then 
            let endPosSeg0 = {sourcePos with X = sourcePos.X + xOffset}
            let endPosSeg1 = {endPosSeg0 with Y = endPosSeg0.Y + (diff.Y) + yOffset}
            let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else //if diff.X < 0 && diff.Y < 0 then
           let endPosSeg0 = {sourcePos with X = sourcePos.X + xOffset}
           let endPosSeg1 = {endPosSeg0 with Y = endPosSeg0.Y + (diff.Y/2.)}
           let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
           [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]

    | Symbol.Right,Symbol.Top ->
        if diff.X > 0. && diff.Y > 0. then // Two Segment Case
            let endPosSeg0 = {sourcePos with X = sourcePos.X + (diff.X)}
            [sourcePos;endPosSeg0;targetPos]
        else if diff.X > 0. && diff.Y < 0. then // Four Segment Case
            let endPosSeg0 = {sourcePos with X = sourcePos.X + (diff.X/2.)}
            let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y - yOffset}
            let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else if diff.X < 0. && diff.Y > 0. then 
            let endPosSeg0 = {sourcePos with X = sourcePos.X + xOffset}
            let endPosSeg1 = {endPosSeg0 with Y = endPosSeg0.Y + (diff.Y) - yOffset}
            let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else //if diff.X < 0 && diff.Y < 0 then
           let endPosSeg0 = {sourcePos with X = sourcePos.X + xOffset}
           let endPosSeg1 = {endPosSeg0 with Y = endPosSeg0.Y + diff.Y - yOffset}
           let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
           [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]

    | Symbol.Left,Symbol.Right ->
        if diff.X > 0. then 
            let endPosSeg0 = {sourcePos with X = sourcePos.X - xOffset}
            let endPosSeg1 = {endPosSeg0 with Y = endPosSeg0.Y + (diff.Y/2.)}
            let endPosSeg2 = {endPosSeg1 with X = endPosSeg1.X + diff.X + (2.*xOffset)}
            let endPosSeg3 = {endPosSeg2 with Y = endPosSeg2.Y + (diff.Y/2.)}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;endPosSeg3;targetPos]
        else 
            let endPosSeg0 = {sourcePos with X = sourcePos.X + (diff.X + xOffset)}
            let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y }
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]
        
    | Symbol.Left,Symbol.Left ->
        if diff.X >= 0. then 
            let endPosSeg0 = {sourcePos with X = sourcePos.X - xOffset}
            let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y }
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]
        
        else 
            let endPosSeg0 = {sourcePos with X = sourcePos.X + diff.X - xOffset}
            let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y }
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]

    | Symbol.Left,Symbol.Top ->
        if diff.X > 0. && diff.Y > 0. then 
            let endPosSeg0 = {sourcePos with X = sourcePos.X - (xOffset)}
            let endPosSeg1 = {endPosSeg0 with Y = endPosSeg0.Y + (diff.Y/2.)}
            let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else if diff.X > 0. && diff.Y < 0. then 
            let endPosSeg0 = {sourcePos with X = sourcePos.X - (xOffset)}
            let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y - yOffset}
            let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else if diff.X < 0. && diff.Y > 0. then 
            let endPosSeg0 = {sourcePos with X = targetPos.X}
            [sourcePos;endPosSeg0;targetPos]
        else //if diff.X < 0 && diff.Y < 0 then
           let endPosSeg0 = {sourcePos with X = sourcePos.X - xOffset}
           let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y - yOffset}
           let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
           [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]

    | Symbol.Left,Symbol.Bottom ->
        if diff.X > 0. && diff.Y > 0. then 
            let endPosSeg0 = {sourcePos with X = sourcePos.X - (xOffset)}
            let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y + yOffset}
            let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else if diff.X > 0. && diff.Y < 0. then 
            let endPosSeg0 = {sourcePos with X = sourcePos.X - (xOffset)}
            let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y + yOffset}
            let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else if diff.X < 0. && diff.Y > 0. then 
            let endPosSeg0 = {sourcePos with X = sourcePos.X + (diff.X/2.)}
            let endPosSeg1 = {endPosSeg0 with Y = targetPos.Y + yOffset}
            let endPosSeg2 = {endPosSeg1 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else //if diff.X < 0 && diff.Y < 0 then
           let endPosSeg0 = {sourcePos with X = targetPos.X}
           [sourcePos;endPosSeg0;targetPos]

    | Symbol.Top, Symbol.Left ->
        if diff.X >= 0. && diff.Y > 0. then  
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y - yOffset}
            let endPosSeg1 = {endPosSeg0 with X = endPosSeg0.X + (diff.X/2.)}
            let endPosSeg2 = {endPosSeg1 with Y = targetPos.Y }
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else if diff.X >= 0. && diff.Y <= 0. then
            let endPosSeg0 = {sourcePos with Y = targetPos.Y}
            [sourcePos;endPosSeg0;targetPos]
        else 
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y - yOffset}
            let endPosSeg1 = {endPosSeg0 with X = targetPos.X - xOffset}
            let endPosSeg2 = {endPosSeg1 with Y = targetPos.Y}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        
    | Symbol.Top, Symbol.Right ->
        if diff.X >= 0. then  
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y - yOffset}
            let endPosSeg1 = {endPosSeg0 with X = targetPos.X + xOffset}
            let endPosSeg2 = {endPosSeg1 with Y = targetPos.Y }
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else if diff.X < 0. && diff.Y >= 0. then
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y - yOffset}
            let endPosSeg1 = {endPosSeg0 with X = endPosSeg0.X + (diff.X/2.)}
            let endPosSeg2 = {endPosSeg1 with Y = targetPos.Y }
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else 
            let endPosSeg0 = {sourcePos with Y = targetPos.Y}
            [sourcePos;endPosSeg0;targetPos]

    | Symbol.Top, Symbol.Top ->
        if diff.Y >= 0. then
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y - yOffset}
            let endPosSeg1 = {endPosSeg0 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]
        else
            let endPosSeg0 = {sourcePos with Y = targetPos.Y - yOffset}
            let endPosSeg1 = {endPosSeg0 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]

    | Symbol.Top, Symbol.Bottom ->
        if diff.Y >= 0. then
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y - yOffset}
            let endPosSeg1 = {endPosSeg0 with X = sourcePos.X + (diff.X/2.)}
            let endPosSeg2 = {endPosSeg1 with Y = targetPos.Y + yOffset}
            let endPosSeg3 = {endPosSeg2 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;endPosSeg3;targetPos]
        else 
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y + (diff.Y/2.)}
            let endPosSeg1 = {endPosSeg0 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]

    | Symbol.Bottom, Symbol.Left ->
        if diff.X >= 0. && diff.Y >= 0. then
            let endPosSeg0 = {sourcePos with Y = targetPos.Y}
            [sourcePos;endPosSeg0;targetPos]
        else if diff.X >= 0. && diff.Y < 0. then
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y + yOffset}
            let endPosSeg1 = {endPosSeg0 with X = endPosSeg0.X + (diff.X/2.)}
            let endPosSeg2 = {endPosSeg1 with Y = targetPos.Y }
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else 
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y + yOffset}
            let endPosSeg1 = {endPosSeg0 with X = targetPos.X - xOffset}
            let endPosSeg2 = {endPosSeg1 with Y = targetPos.Y}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
    | Symbol.Bottom, Symbol.Right ->
        if diff.X >= 0. then  
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y + yOffset}
            let endPosSeg1 = {endPosSeg0 with X = targetPos.X + xOffset}
            let endPosSeg2 = {endPosSeg1 with Y = targetPos.Y }
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]
        else if diff.X < 0. && diff.Y >= 0. then
            let endPosSeg0 = {sourcePos with Y = targetPos.Y}
            [sourcePos;endPosSeg0;targetPos]
        else 
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y + yOffset}
            let endPosSeg1 = {endPosSeg0 with X = endPosSeg0.X + (diff.X/2.)}
            let endPosSeg2 = {endPosSeg1 with Y = targetPos.Y }
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;targetPos]

    | Symbol.Bottom, Symbol.Bottom ->
        if diff.Y >= 0. then
            let endPosSeg0 = {sourcePos with Y = targetPos.Y + yOffset}
            let endPosSeg1 = {endPosSeg0 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]
        else
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y + yOffset}
            let endPosSeg1 = {endPosSeg0 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]

    | Symbol.Bottom, Symbol.Top ->
        if diff.Y >= 0. then
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y + (diff.Y/2.)}
            let endPosSeg1 = {endPosSeg0 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;targetPos]
        else 
            let endPosSeg0 = {sourcePos with Y = sourcePos.Y + yOffset}
            let endPosSeg1 = {endPosSeg0 with X = sourcePos.X + (diff.X/2.)}
            let endPosSeg2 = {endPosSeg1 with Y = targetPos.Y - yOffset}
            let endPosSeg3 = {endPosSeg2 with X = targetPos.X}
            [sourcePos;endPosSeg0;endPosSeg1;endPosSeg2;endPosSeg3;targetPos]

/// Returns the Top Left and Bottom Right Corner for a wire segment bounding box
let makeWireBB (sPos:XYPos) (tPos:XYPos): XYPos * XYPos=
    match sPos.X,sPos.Y, tPos.X, tPos.Y with
    | (sx,sy,tx,ty) when tx>sx -> ({X=sx;Y=sy-5.},{X=tx;Y=ty+5.})
    | (sx,sy,tx,ty) when tx<sx -> ({X=tx;Y=ty-5.},{X=sx;Y=sy+5.})
    | (sx,sy,tx,ty) when ty>sy -> ({X=sx-5.;Y=sy},{X=tx+5.;Y=ty})
    | (sx,sy,tx,ty) when ty<sy -> ({X=tx-5.;Y=ty},{X=tx+5.;Y=sy})
    | (sx,sy,tx,ty) when (tx=sx && ty=sy) -> ({X=tx;Y=ty},{X=sx;Y=sy})
    | _ -> failwithf "diagonal line error"

let bbCollision (bb1:XYPos*XYPos) (bb2:XYPos*XYPos) =
    match fst bb1, snd bb1, fst bb2, snd bb2 with
    | (tL1, bR1, tL2, bR2) when bR2.Y < tL1.Y -> false
    | (tL1, bR1, tL2, bR2) when tL2.X > bR1.X -> false
    | (tL1, bR1, tL2, bR2) when tL2.Y > bR1.Y -> false
    | (tL1, bR1, tL2, bR2) when bR2.X < tL1.X -> false
    |  _ ->true

/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire option =
    wModel.WX
    |> List.tryFind (fun wire -> wire.Id = wId)

/// Creates a single wire segment
let makeWireSegment (wId:CommonTypes.ConnectionId) (width:int) (srcPos) (tgtPos)  = 
    {
        wId= wId
        Index = 0
        SourcePos=srcPos
        TargetPos=tgtPos
        BB= makeWireBB srcPos tgtPos
        Highlight=false
        Width=width
    }

/// Sets the indexes for each segment in a wire
let setIndex (lst: WireSegment list) =
    lst 
    |> List.mapi (fun i seg-> {seg with Index= i})

/// Calculates and creates all wire segments
let makeWireSegments (model: Model) (wId: CommonTypes.ConnectionId) (width: int) 
    (sourcePortId: CommonTypes.PortId) (targetPortId: CommonTypes.PortId) : WireSegment list =

    (routeWire model sourcePortId targetPortId)
    |> List.pairwise
    |> List.map (fun (src,tgt) -> makeWireSegment wId 1 src tgt)
    |> setIndex


/// Creates a new wire 
let makeNewWire (model: Model) (srcPortId: CommonTypes.PortId) (tgtPortId: CommonTypes.PortId) (width: int): Wire = 
    let wId = CommonTypes.ConnectionId (Helpers.uuid())
    let srcEdge = Symbol.getPortEdge model.Symbol srcPortId
    let tgtEdge = Symbol.getPortEdge model.Symbol tgtPortId
    let newSegments = makeWireSegments model wId width srcPortId tgtPortId
    let startHoriz = 
        if srcEdge=Symbol.Top || srcEdge=Symbol.Bottom then false else true
    let endHoriz = 
        if tgtEdge=Symbol.Top || tgtEdge=Symbol.Bottom then false else true
            
    {
        Id = wId
        SourcePortId = srcPortId
        SourcePortEdge = srcEdge
        TargetPortId = tgtPortId
        TargetPortEdge = tgtEdge
        Segments = newSegments
        Width = width
        Highlight = false
        HighlightError = false
        Manual = false
        StartHoriz=startHoriz
        EndHoriz=endHoriz
    }

//----------------Render/View Functions----------------//

type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    Segments: WireSegment list
    Width: int
    Highlight: bool
    HighlightError : bool
    ColorP: string
    StrokeWidthP: string }


/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.
let singleWireView =        
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            
            let colour = if props.HighlightError then "red" elif props.Highlight then "green" elif props.Width = 1 then props.ColorP else "purple"
            let stroke = if props.Width > 1 then "2px" else props.StrokeWidthP
            let singleSegmentView (seg: WireSegment) =
                let SrcP = seg.SourcePos
                let TgtP = seg.TargetPos
                line [
                    X1 SrcP.X
                    Y1 SrcP.Y
                    X2 TgtP.X
                    Y2 TgtP.Y
                    // Qualify these props to avoid name collision with CSSProp
                    SVGAttr.Stroke colour
                    SVGAttr.StrokeWidth stroke
                    SVGAttr.StrokeLinecap "round"] []
            let widthAnnotation = 
                let textPos = Symbol.posAdd props.Segments.Head.SourcePos {X = 8. ; Y = 5.}
                text [
                    X textPos.X
                    Y textPos.Y
                    Style [
                        TextAnchor "middle"
                        DominantBaseline "hanging"
                        FontSize "7px"
                        FontWeight "Bold"
                        Fill "Black"
                        UserSelect UserSelectOptions.None
                    ]
                ] [str <| sprintf "%i" props.Width]   
            let highlightCircles =
                    
                    if props.Highlight = false then
                        []
                    else
                        let srcHighlightPos = 
                            match props.WireP.SourcePortEdge with
                            | Symbol.Left -> Symbol.posAdd (List.head props.Segments).SourcePos {X= -3.;Y=0.}
                            | Symbol.Right -> Symbol.posAdd (List.head props.Segments).SourcePos {X= 3.;Y=0.}
                            | Symbol.Bottom -> Symbol.posAdd (List.head props.Segments).SourcePos {X=0.;Y=3.}
                            | Symbol.Top -> Symbol.posAdd (List.head props.Segments).SourcePos {X=0.;Y= -3.}
                        let tgtHighlightPos = 
                            match props.WireP.TargetPortEdge with
                            | Symbol.Left -> Symbol.posAdd (List.last props.Segments).TargetPos {X= -3.;Y=0.}
                            | Symbol.Right -> Symbol.posAdd (List.last props.Segments).TargetPos {X= 3.;Y=0.}
                            | Symbol.Bottom -> Symbol.posAdd (List.last props.Segments).TargetPos {X=0.;Y=3.}
                            | Symbol.Top -> Symbol.posAdd (List.last props.Segments).TargetPos {X=0.;Y= -3.}
                            
                        [
                        circle [
                           Cx srcHighlightPos.X
                           Cy srcHighlightPos.Y
                           R 3.

                           SVGAttr.Fill "deepskyblue"
                           SVGAttr.Stroke "deepskyblue"
                           //SVGAttr.Opacity 0.4
                           SVGAttr.StrokeWidth 1][]
                           ;
                        circle [
                            Cx tgtHighlightPos.X
                            Cy tgtHighlightPos.Y
                            R 3.

                            SVGAttr.Fill "deepskyblue"
                            SVGAttr.Stroke "deepskyblue"
                            //SVGAttr.Opacity 0.4
                            SVGAttr.StrokeWidth 1][]
                        ]
            
            let segmentEls =
                props.Segments
                |> List.map singleSegmentView
            (widthAnnotation::segmentEls)@highlightCircles
            |> ofList)



let view (model:Model) (dispatch: Dispatch<Msg>)=
    let wires = 
        model.WX
        |> List.map (fun w ->
            let props = {
                key = w.Id
                WireP = w
                Segments = w.Segments
                Width = w.Width
                Highlight = w.Highlight
                HighlightError = w.HighlightError
                ColorP = model.Color.Text()
                StrokeWidthP = "1px" }
            singleWireView props)
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    g [] [(g [] wires); symbols]

/// dummy init for testing: real init would probably start with no wires.
/// this initialisation is not realistic - ports are not used
/// this initialisation depends on details of Symbol.Model type.
let init n () =
    let symbols, cmd = Symbol.init()
    let symIds = List.map (fun (sym:Symbol.Symbol) -> sym.Id) symbols
    (*let rng = System.Random 0
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
        }*)
    []
    |> (fun wires -> {WX=wires;Symbol=symbols; Color=CommonTypes.Red},Cmd.none)

//-------------------Helpers for Update Function-------------------//
// WIRE RULES: Must have signature Model->Wire->bool

/// Checks if a wire connects an Output Port to an Input Port
let ruleOutToIn (model: Model) (wire: Wire) : bool = 
    match Symbol.getPortType model.Symbol wire.SourcePortId , Symbol.getPortType model.Symbol wire.TargetPortId with 
    | CommonTypes.Output , CommonTypes.Input -> true
    | _ , _ -> false

/// Checks if the wire's target port already has a wire driving it. By-product of this is that it disallows duplicate wire creation.
let ruleUnique (model: Model) (wire: Wire) : bool =
    model.WX
    |> List.filter (fun w -> w.TargetPortId = wire.TargetPortId)
    |> List.isEmpty

/// Checks if the Width of the Source Port is equal to that of the Target Port
(*
let ruleWidthEquality (model: Model) (wire: Wire) : bool =
    if Symbol.getPortWidth model.Symbol wire.SourcePortId = Symbol.getPortWidth model.Symbol wire.TargetPortId then 
        true
    else 
        false
*)
let wireRuleList =
    [
        ruleOutToIn;
        ruleUnique;
        //ruleWidthEquality Removed because Dr Clarke wanted it gone
    ]
    
/// Verifies if a supplied wire is compliant with the rules for wires.
/// Returns wire wrapped in a Wire Option if it is compliant, otherwise returns None.
let verifyWire (model: Model) (wire: Wire): Wire option = 
    wireRuleList 
    |> List.map (fun f -> f model wire)
    |> List.contains false
    |> function true -> None | false -> Some wire

/// Returns the absolute length of a wire segment
let segLength (seg: WireSegment): float =
    (seg.TargetPos,seg.SourcePos)
    ||> Symbol.posDiff
    |> (fun diff -> (sqrt diff.X**2. + diff.Y**2.))

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol

        let manualRoute (wire: Wire) =
            let noOfSegments= List.length wire.Segments
            let last = noOfSegments - 1
            let prevSrc= wire.Segments.[0].SourcePos
            let prevTgt= wire.Segments.[last].TargetPos
            let src= Symbol.getPortCoords sm wire.SourcePortId
            let tgt= Symbol.getPortCoords sm wire.TargetPortId

            let vecSrc = {X=src.X-prevSrc.X;Y=src.Y-prevSrc.Y}
            let vecTgt = {X=tgt.X-prevTgt.X;Y=tgt.Y-prevTgt.Y}
            let head= List.head wire.Segments
            let tail = List.last wire.Segments

            let fstLength = 
                match wire.SourcePortEdge with 
                |Symbol.Right when head.TargetPos.X<src.X + 9. -> true
                |Symbol.Left when head.TargetPos.X>src.X - 9. ->true
                |Symbol.Top when head.TargetPos.Y>src.Y - 9. ->true
                |Symbol.Bottom when head.TargetPos.Y<src.Y + 9. ->true
                |_ -> false
            
            //if head.TargetPos.X<src.X + 9. then true else false
            let LstLength = 
                match wire.TargetPortEdge with 
                |Symbol.Right when tail.SourcePos.X<tgt.X + 9. -> true
                |Symbol.Left when tail.SourcePos.X>tgt.X - 9. ->true
                |Symbol.Top when tail.SourcePos.Y>tgt.Y - 9. ->true
                |Symbol.Bottom when tail.SourcePos.Y<tgt.Y + 9. ->true
                |_ -> false


            wire.Segments
            |> List.mapi (fun i seg -> 
                match i ,wire.StartHoriz ,wire.EndHoriz with
                |0,true,_ when fstLength ->makeWireSegment wire.Id wire.Width src (Symbol.posAdd seg.TargetPos vecSrc) 
                |1,true,_ when fstLength ->makeWireSegment wire.Id wire.Width (Symbol.posAdd seg.SourcePos vecSrc) {X=seg.TargetPos.X + vecSrc.X; Y=seg.TargetPos.Y}
                |2,true,_ when fstLength ->makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X + vecSrc.X; Y=seg.SourcePos.Y} seg.TargetPos

                |0,false,_ when fstLength ->makeWireSegment wire.Id wire.Width src (Symbol.posAdd seg.TargetPos vecSrc) 
                |1,false,_ when fstLength ->makeWireSegment wire.Id wire.Width (Symbol.posAdd seg.SourcePos vecSrc) {X=seg.TargetPos.X ; Y=seg.TargetPos.Y+vecSrc.Y}
                |2,false,_ when fstLength ->makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X ; Y=seg.SourcePos.Y+ vecSrc.Y} seg.TargetPos
                
                |x,_,true when x=last && LstLength ->makeWireSegment wire.Id wire.Width (Symbol.posAdd seg.SourcePos vecTgt) tgt
                |x,_,true when x=last - 1 && LstLength ->makeWireSegment wire.Id wire.Width  {X=seg.SourcePos.X + vecTgt.X; Y=seg.SourcePos.Y} (Symbol.posAdd seg.TargetPos vecTgt)
                |x,_,true when x=last - 2 && LstLength ->makeWireSegment wire.Id wire.Width  seg.SourcePos {X=seg.TargetPos.X + vecTgt.X; Y=seg.TargetPos.Y}

                |x,_,false when x=last && LstLength ->makeWireSegment wire.Id wire.Width (Symbol.posAdd seg.SourcePos vecTgt) tgt
                |x,_,false when x=last - 1 && LstLength ->makeWireSegment wire.Id wire.Width  {X=seg.SourcePos.X ; Y=seg.SourcePos.Y + vecTgt.Y} (Symbol.posAdd seg.TargetPos vecTgt)
                |x,_,false when x=last - 2 && LstLength ->makeWireSegment wire.Id wire.Width  seg.SourcePos {X=seg.TargetPos.X ; Y=seg.TargetPos.Y + vecTgt.Y}
                
                |0,true,_ ->makeWireSegment wire.Id wire.Width src {X=seg.TargetPos.X;Y=src.Y} 
                |1,true,_ when noOfSegments>3 ->makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X;Y=src.Y} seg.TargetPos 
                |x,true,true when (x= last - 1 && noOfSegments>3 )-> makeWireSegment wire.Id wire.Width seg.SourcePos {X=seg.SourcePos.X;Y=tgt.Y}  
                |1,true,true ->makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X;Y=src.Y} {X=seg.SourcePos.X;Y=tgt.Y} 
                |x,true,true when x=last-> makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X;Y=tgt.Y} tgt 
                |x,true,false when (x= last - 1 && noOfSegments>3 )-> makeWireSegment wire.Id wire.Width seg.SourcePos {X=tgt.X;Y=seg.SourcePos.Y}
                |x,true,false when x=last-> makeWireSegment wire.Id wire.Width {X=tgt.X;Y=seg.SourcePos.Y} tgt
                |0,false,_ ->makeWireSegment wire.Id wire.Width src {X=src.X;Y=seg.TargetPos.Y} 
                |1,false,_ when noOfSegments>3 ->makeWireSegment wire.Id wire.Width {X=src.X;Y=seg.SourcePos.Y} seg.TargetPos
                |x,false,false when (x= last - 1 && noOfSegments>3 )-> makeWireSegment wire.Id wire.Width seg.SourcePos {X=tgt.X;Y=seg.SourcePos.Y}
                |1,false,false ->makeWireSegment wire.Id wire.Width {X=src.X;Y=seg.SourcePos.Y} {X=tgt.X;Y=seg.SourcePos.Y} 
                |x,false,false when x=last-> makeWireSegment wire.Id wire.Width {X=tgt.X;Y=seg.SourcePos.Y} tgt 
                |x,false,true when (x= last - 1 && noOfSegments>3 )-> makeWireSegment wire.Id wire.Width seg.SourcePos {X=seg.SourcePos.X;Y=tgt.Y}
                |x,false,true when x=last-> makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X;Y=tgt.Y} tgt
                |_ -> seg    
            )
            |> setIndex

        let checkRouteCaseChange (wire: Wire) : Wire = // Very ugly function, turn it into a rule function list which is mapped

            let newDiff = Symbol.posDiff (Symbol.getPortCoords sm wire.TargetPortId) (Symbol.getPortCoords sm wire.SourcePortId) 
            let oldDiff = Symbol.posDiff ((List.last wire.Segments).TargetPos) ((List.head wire.Segments).SourcePos)
            if ((sign(newDiff.X) <> sign(oldDiff.X) || sign(newDiff.Y) <> sign(oldDiff.Y)  || (wire.SourcePortEdge,wire.TargetPortEdge) <> (Symbol.getPortEdge sm wire.SourcePortId,Symbol.getPortEdge sm wire.TargetPortId)) || List.length wire.Segments < 3) then
                {wire with Manual = false}
            else
                wire
           

        let wList = 
            model.WX
            |> List.map checkRouteCaseChange
            |> List.map (fun w -> 
                let srcEdge = Symbol.getPortEdge sm w.SourcePortId
                let tgtEdge = Symbol.getPortEdge sm w.TargetPortId
                let startHoriz = 
                    if srcEdge=Symbol.Top || srcEdge=Symbol.Bottom then false else true
                let endHoriz = 
                    if tgtEdge=Symbol.Top || tgtEdge=Symbol.Bottom then false else true
                match w.Manual with
                | false -> {w with 
                                Segments = makeWireSegments model w.Id w.Width w.SourcePortId w.TargetPortId 
                                SourcePortEdge = srcEdge
                                TargetPortEdge = tgtEdge
                                StartHoriz = startHoriz
                                EndHoriz = endHoriz}
                | true -> {w with Segments = manualRoute w}
            )
        {model with Symbol=sm; WX = wList}, Cmd.map Symbol sCmd

    | AddWire (portId1,portId2) ->
        let unverifiedWire =
            match Symbol.getPortType model.Symbol portId1 , Symbol.getPortType model.Symbol portId2 with
            | CommonTypes.Output , CommonTypes.Input -> makeNewWire model portId1 portId2 1 // Wire was drawn from Output to Input
            | CommonTypes.Input , CommonTypes.Output -> makeNewWire model portId2 portId1 1 // Wire was drawn from Input to Output
            | _ , _ -> makeNewWire model portId1 portId2 1 // Invalid port combination, will be caught by verifyWire
             
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

    | MoveWires (wId,idx,vec) ->
        let a = idx - 1
        let b = idx + 1
        let move (w:Wire) (seg:WireSegment) idx wId vector=
            let last = (List.length w.Segments) - 1
            match idx with
            | 0  -> seg
            | x when (x=last && seg.Index=last ) -> seg
            | _ when w.Id <> wId -> seg
            | _ when (idx%2=0 && w.StartHoriz) || (idx%2=1 && not w.StartHoriz)  -> 
                match seg.Index with 
                | x when x=a -> makeWireSegment wId w.Width seg.SourcePos  {X=seg.TargetPos.X;Y=seg.TargetPos.Y+vector.Y} 
                | x when x=idx-> makeWireSegment wId w.Width {X=seg.SourcePos.X;Y=seg.SourcePos.Y+vector.Y} {X=seg.TargetPos.X;Y=seg.TargetPos.Y+vector.Y} 
                | x when x=b -> makeWireSegment wId w.Width {X=seg.SourcePos.X;Y=seg.SourcePos.Y+vector.Y} seg.TargetPos 
                | _ -> seg
            | _ when (idx%2=1 && w.StartHoriz) || (idx%2=0 && not w.StartHoriz) ->
                match seg.Index with 
                |x when x=a -> makeWireSegment wId w.Width seg.SourcePos {X=seg.TargetPos.X+vector.X;Y=seg.TargetPos.Y} 
                |x when x=idx-> makeWireSegment wId w.Width {X=seg.SourcePos.X+vector.X;Y=seg.SourcePos.Y} {X=seg.TargetPos.X+vector.X;Y=seg.TargetPos.Y} 
                |x when x=b -> makeWireSegment wId w.Width {X=seg.SourcePos.X+vector.X;Y=seg.SourcePos.Y} seg.TargetPos 
                |_ -> seg
            |_ -> failwithf "Negative Index"
        
        let condition (w:Wire)=
            let head= List.head w.Segments
            let last = List.last w.Segments
            match w.SourcePortEdge, w.TargetPortEdge with 
            |Symbol.Right,_ when head.TargetPos.X<head.SourcePos.X + 9. ->false
            |Symbol.Top,_ when head.TargetPos.Y>head.SourcePos.Y - 9. ->false
            |Symbol.Left,_ when head.TargetPos.X>head.SourcePos.X - 9. ->false
            |Symbol.Bottom, _ when head.TargetPos.Y<head.SourcePos.Y + 9. ->false
            |_,Symbol.Right when last.SourcePos.X<last.TargetPos.X + 9. ->false
            |_,Symbol.Top when last.SourcePos.Y>last.TargetPos.Y - 9. ->false
            |_,Symbol.Left when last.SourcePos.X>last.TargetPos.X - 9. ->false
            |_,Symbol.Bottom when last.SourcePos.Y<last.TargetPos.Y + 9. ->false
            |_ -> true
        (*
        let validateSegments (segLstLst: WireSegment list list): bool =
            segLstLst
            |> List.map (fun segLst ->
                if (*segLength (List.head segLst) < 10. || segLength (List.last segLst) < 10.*) condition (List.head segLst) then false else true)
            |> List.contains false
            |> not
        *)

        let validateWires (wLst: Wire list): bool =
            wLst
            |> List.map (fun w ->
                condition w)
            |> List.contains false
            |> not


        let segLstLst =

            model.WX
            |> List.map (fun w ->
                List.map (fun (seg:WireSegment)-> move w seg idx wId vec) w.Segments)

        let wList= 
            model.WX
            |> List.mapi (fun i w-> {w with Segments = setIndex segLstLst.[i]})
            |> List.map (fun w ->
                if  w.Id=wId  then
                    {w with Manual = true}
                else 
                    w 
            )
        if validateWires wList then {model with WX = wList}, Cmd.none else model, Cmd.none
        //{model with WX = wList}, Cmd.none

    | SetColor c -> {model with Color = c}, Cmd.none
    | UpdateWidth lst ->        
         {model with
             WX = model.WX
                 |> List.map (fun wire ->
                     List.tryFind (fun x -> fst x = wire.Id) lst
                     |> function
                     | Some (_, width) -> 
                        match width with
                        | Some w ->
                             { wire with
                                 Width = w
                                 Segments = List.map (fun x -> {x with Width = w}) wire.Segments
                             }
                        | _ -> wire
                     | None -> 
                         wire
                 )
         }
        , Cmd.none
    | HighlightError cIdList ->
        {model with
            WX = model.WX
                |> List.map (fun wire -> {wire with HighlightError = List.contains wire.Id cIdList})
        }
        , Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))

//---------------Other interface functions--------------------//

/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Used
/// to determine which wire (if any) to select on a mouse click
let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option = 
    failwith "Not implemented"

/// Returns all bounding boxes for all wire segments in the wire model
let getBoundingBoxes (wModel: Model) (mouseCoord: XYPos): ( CommonTypes.ConnectionId * int *XYPos * XYPos) list =
    wModel.WX
    |> List.collect (fun w->
                    List.map (fun (segment:WireSegment)->
                                ( segment.wId,segment.Index,fst segment.BB, snd segment.BB))w.Segments)

/// Returns a list of wire IDs connected to the supplied ports
let getWireIdsFromPortIds (wModel: Model) (portIds: CommonTypes.PortId list) : CommonTypes.ConnectionId list =
    wModel.WX
    |> List.filter (fun w -> List.contains w.SourcePortId portIds || List.contains w.TargetPortId portIds)
    |> List.map (fun w -> w.Id)
    
/// Takes a connectionId and returns the wire object associated with it
let connectToWire (wModel : Model) (connect : CommonTypes.ConnectionId) : Wire =
    wModel.WX
    |> List.tryFind (fun x -> x.Id = connect)
    |> function 
    | Some x -> x
    | _ -> failwithf "Error in connectToWire, couldn't find connection id in model"

/// Takes a ConnectionId and returns the (source port, target port) associated with that connection
let connectToPort (wModel : Model) (connect : CommonTypes.ConnectionId) : (CommonTypes.PortId * CommonTypes.PortId) = 
    let wire = (connectToWire wModel connect)
    (wire.SourcePortId, wire.TargetPortId)


/// Takes a ConnectionId and returns the Symbol IDs connected to that connection
let connectedSymbols (wModel : Model) (connect : CommonTypes.ConnectionId) : CommonTypes.ComponentId list = 
    let (p1, p2) = connectToPort wModel connect
    let sym1 = Symbol.getHostId wModel.Symbol p1
    let sym2 = Symbol.getHostId wModel.Symbol p2
    [sym1; sym2]

let getWires (wModel : Model) (cIdList : CommonTypes.ConnectionId list) : CommonTypes.ConnectionId list =
    wModel.WX
    |> List.map (fun x -> x.Id)
    |> List.filter (fun x -> not (List.contains x cIdList))
//----------------------interface to Issie-----------------------//

/// Converts a wire segment list into a list of distinct vertices
let segToVert (wSegs : WireSegment list) : (float * float) list = 
    wSegs 
    |> List.collect (fun x -> [(x.SourcePos.X, x.SourcePos.Y); (x.TargetPos.X, x.TargetPos.Y)])
    |> List.distinct
    
/// Converts the Wire type into the Issie Connection type
let wireToIssie (wire : Wire) (wModel : Model) : CommonTypes.Connection = 
    {
        CommonTypes.Connection.Id = string wire.Id
        CommonTypes.Connection.Source = Symbol.getPort wModel.Symbol wire.SourcePortId
        CommonTypes.Connection.Target = Symbol.getPort wModel.Symbol wire.TargetPortId
        CommonTypes.Connection.Vertices = segToVert wire.Segments
    }

/// Converts the wire model from a Wire list to an Issie Connection list
let extractWires (wModel: Model) : CommonTypes.Connection list = 
    wModel.WX
    |> List.map (fun x -> wireToIssie x wModel)
    

let extractWiress (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"



    



