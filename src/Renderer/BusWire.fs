﻿module BusWire

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
    | AddSegment  of CommonTypes.ConnectionId * int * XYPos
    | SnapWire of CommonTypes.ConnectionId list

//-------------------Helpers for functions------------------------//

/// Takes Source and Target ports of a wire and returns the 
/// vertices of the path it should follow
let routeWire (model: Symbol.Model) (sourcePortId: CommonTypes.PortId) (targetPortId: CommonTypes.PortId) : XYPos list =
    let sourcePos = Symbol.getPortCoords model sourcePortId
    let targetPos = Symbol.getPortCoords model targetPortId
    let diff = Symbol.posDiff targetPos sourcePos
    let xOffset = 10. // Length of Horizontal line coming out of/going into port
    let yOffset = 10. // Length of Vertical line coming out of/going into port
    match Symbol.getPortEdge model sourcePortId, Symbol.getPortEdge model targetPortId with 
    | Symbol.Right,Symbol.Left -> 
        if diff.X > 0. then // Three Segement Case
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
/// Returns true is Wire Segment is horizontal and false otherwise    
let isHoriz (seg:WireSegment)= seg.SourcePos.Y=seg.TargetPos.Y
/// Snaps a X or Y co-ordinate to the grid.
let snap (pos:float)= (round (pos/ 10.))*10.
///Takes in WireSegment List and returns the the list with the correct index field for each segment
let setIndex (lst: WireSegment list) =
    lst 
    |> List.mapi (fun i seg-> {seg with Index= i})



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

/// Snaps a single segment to the grid 
let snapSeg (seg:WireSegment) (srcPos:XYPos)=
    if isHoriz seg then
        (makeWireSegment seg.wId seg.Width srcPos {X=snap seg.TargetPos.X ;Y=srcPos.Y},{X=snap seg.TargetPos.X ;Y=srcPos.Y})
    else 
        (makeWireSegment seg.wId seg.Width srcPos {X=srcPos.X ;Y= snap seg.TargetPos.Y},{X=srcPos.X ;Y= snap seg.TargetPos.Y})
///Snaps a segment list (that makes up a whole wire) to the grid
let rec snapWireSegments  (currSrc:XYPos) (segLstOut:WireSegment List) (count:int) (segLstIn:WireSegment List)=

    match count with 
    | x when x= List.length segLstIn -> segLstOut|>setIndex
    | _ -> 
        let newSeg=fst (snapSeg (segLstIn.[count]) currSrc)
        let newSrc=snd (snapSeg (segLstIn.[count]) currSrc)
        snapWireSegments newSrc (segLstOut@[newSeg]) (count + 1) segLstIn

/// Calculates and creates all wire segments
let makeWireSegments (sm: Symbol.Model) (wId: CommonTypes.ConnectionId) (width: int) 
    (sourcePortId: CommonTypes.PortId) (targetPortId: CommonTypes.PortId) : WireSegment list =
    
    (routeWire sm sourcePortId targetPortId)
    |> List.pairwise
    |> List.map (fun (src,tgt) -> makeWireSegment wId 1 src tgt)
    


/// Creates a new wire 
let makeNewWire (model: Model) (srcPortId: CommonTypes.PortId) (tgtPortId: CommonTypes.PortId): Wire = 
    let wId = CommonTypes.ConnectionId (Helpers.uuid())
    let srcEdge = Symbol.getPortEdge model.Symbol srcPortId
    let tgtEdge = Symbol.getPortEdge model.Symbol tgtPortId
    let portSrc= Symbol.getPortCoords model.Symbol srcPortId
    let newSegments = makeWireSegments model.Symbol wId 1 srcPortId tgtPortId |> snapWireSegments portSrc [] 0 
    let startHoriz = not (srcEdge=Symbol.Top || srcEdge=Symbol.Bottom)
    let endHoriz = not (tgtEdge=Symbol.Top || tgtEdge=Symbol.Bottom)
            
    {
        Id = wId
        SourcePortId = srcPortId
        SourcePortEdge = srcEdge
        TargetPortId = tgtPortId
        TargetPortEdge = tgtEdge
        Segments = newSegments
        Width = 1
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
                    if not props.Highlight then
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
                ColorP = "grey"
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
    [] // No wires initially in model
    |> (fun wires -> {WX=wires;Symbol=symbols; Color=CommonTypes.Red},Cmd.none)

//-------------------Helpers for Update Function-------------------//
// WIRE RULES: Must have signature Model->Wire->bool

/// Checks if a wire connects an Output Port to an Input Port
let ruleOutToIn (model: Model) (wire: Wire) : bool = 
    match Symbol.getPortType model.Symbol wire.SourcePortId, Symbol.getPortType model.Symbol wire.TargetPortId with 
    | CommonTypes.Output, CommonTypes.Input -> true
    | _ -> false

/// Checks if the wire's target port already has a wire driving it. By-product of this is that it disallows duplicate wire creation.
let ruleUnique (model: Model) (wire: Wire) : bool =
    model.WX
    |> List.filter (fun w -> w.TargetPortId = wire.TargetPortId)
    |> List.isEmpty

let wireRuleList =
    [
        ruleOutToIn;
        ruleUnique;
    ]
    
/// Verifies if a supplied wire is compliant with the rules for wires.
/// Returns wire wrapped in a Wire Option if it is compliant, otherwise returns None.
let verifyWire (model: Model) (wire: Wire): Wire option = 
    wireRuleList 
    |> List.map (fun f -> f model wire)
    |> List.contains false
    |> fun x ->
        if x then None
        else Some wire

/// Returns the absolute length of a wire segment
let segLength (seg: WireSegment): float =
    (seg.TargetPos,seg.SourcePos)
    ||> Symbol.posDiff
    |> fun diff -> (sqrt diff.X ** 2. + diff.Y ** 2.)


///Returns true if first segment is shorter than offset (9.) and false otherwise
let checkFirstSegmentLength (wire:Wire) (sm: Symbol.Model) =
    let src= Symbol.getPortCoords sm wire.SourcePortId
    let head= List.head wire.Segments
    match wire.SourcePortEdge with 
    | Symbol.Right when head.TargetPos.X<src.X + 9. -> true
    | Symbol.Left when head.TargetPos.X>src.X - 9. ->true
    | Symbol.Top when head.TargetPos.Y>src.Y - 9. ->true
    | Symbol.Bottom when head.TargetPos.Y<src.Y + 9. ->true
    | _ -> false
///Returns true if last segment is shorter than offset (9.) and false otherwise
let checkLastSegmentLength (wire:Wire) (sm: Symbol.Model) =

    let tgt= Symbol.getPortCoords sm wire.TargetPortId
    let tail = List.last wire.Segments
    match wire.TargetPortEdge with 
    | Symbol.Right when tail.SourcePos.X<tgt.X + 9. -> true
    | Symbol.Left when tail.SourcePos.X>tgt.X - 9. ->true
    | Symbol.Top when tail.SourcePos.Y>tgt.Y - 9. ->true
    | Symbol.Bottom when tail.SourcePos.Y<tgt.Y + 9. ->true
    | _ -> false
///Determines movement of Wire when in manual routing mode and there is symbol movement
let manualRouteSymbolMove (wire: Wire) (sm: Symbol.Model)  (multipleMove:bool) : WireSegment list =
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

    let fstLength = checkFirstSegmentLength wire sm
    
    
    let lstLength = checkLastSegmentLength wire sm



    wire.Segments
    |> List.mapi (fun i seg ->
         
        match i ,wire.StartHoriz ,wire.EndHoriz with
        
        | _ when multipleMove -> 
            makeWireSegment wire.Id wire.Width (Symbol.posAdd seg.SourcePos vecSrc) (Symbol.posAdd seg.TargetPos vecSrc) 
        (* 'PUSHING' FUNCTIONALITY - When symbol moves and either the first or last segment length get too small these 
        lengths remain fixed and symbol pushes wire*)
        
        //when first segment is too short
        | 0,_,_ when fstLength ->
           makeWireSegment wire.Id wire.Width src (Symbol.posAdd seg.TargetPos vecSrc) 

        | 1,true,_ when fstLength ->
            makeWireSegment wire.Id wire.Width (Symbol.posAdd seg.SourcePos vecSrc) {X=seg.TargetPos.X + vecSrc.X; Y=seg.TargetPos.Y}
        | 2,true,_ when fstLength ->
            makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X + vecSrc.X; Y=seg.SourcePos.Y} seg.TargetPos
 
        | 1,false,_ when fstLength ->
            makeWireSegment wire.Id wire.Width (Symbol.posAdd seg.SourcePos vecSrc) {X=seg.TargetPos.X ; Y=seg.TargetPos.Y+vecSrc.Y}
        | 2,false,_ when fstLength ->
            makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X ; Y=seg.SourcePos.Y+ vecSrc.Y} seg.TargetPos
        
        //when last segment is too short
        | x,_,_ when x=last && lstLength ->
            makeWireSegment wire.Id wire.Width (Symbol.posAdd seg.SourcePos vecTgt) tgt
        
        | x,_,true when x=last - 1 && lstLength ->
            makeWireSegment wire.Id wire.Width  {X=seg.SourcePos.X + vecTgt.X; Y=seg.SourcePos.Y} (Symbol.posAdd seg.TargetPos vecTgt)
        | x,_,true when x=last - 2 && lstLength ->
            makeWireSegment wire.Id wire.Width  seg.SourcePos {X=seg.TargetPos.X + vecTgt.X; Y=seg.TargetPos.Y}


        | x,_,false when x=last - 1 && lstLength ->
            makeWireSegment wire.Id wire.Width  {X=seg.SourcePos.X ; Y=seg.SourcePos.Y + vecTgt.Y} (Symbol.posAdd seg.TargetPos vecTgt)
        | x,_,false when x=last - 2 && lstLength ->
            makeWireSegment wire.Id wire.Width  seg.SourcePos {X=seg.TargetPos.X ; Y=seg.TargetPos.Y + vecTgt.Y}
        
        (* WHEN THERE IS SYMBOL MOVEMENT THE 2 CLOSEST SEGMENT ATTACHED TO THE SYMBOL MUST UPDATE CORRECTLY, THE OTHERS REMAIN FIXED *)
        // update first 2 segments when first segment is horizontal 
        | 0,true,_ ->
            makeWireSegment wire.Id wire.Width src {X=seg.TargetPos.X;Y=src.Y} 
        | 1,true,_ when noOfSegments>3 ->
            makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X;Y=src.Y} seg.TargetPos 
        | x,true,true when (x= last - 1 && noOfSegments>3 )-> 
            makeWireSegment wire.Id wire.Width seg.SourcePos {X=seg.SourcePos.X;Y=tgt.Y}  
        | 1,true,true ->
            makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X;Y=src.Y} {X=seg.SourcePos.X;Y=tgt.Y} 
        // update last 2 segments when first segment is horizontal
        | x,true,true when x=last-> 
            makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X;Y=tgt.Y} tgt 
        | x,true,false when (x= last - 1 && noOfSegments>3 )-> 
            makeWireSegment wire.Id wire.Width seg.SourcePos {X=tgt.X;Y=seg.SourcePos.Y}
        | x,true,false when x=last-> 
            makeWireSegment wire.Id wire.Width {X=tgt.X;Y=seg.SourcePos.Y} tgt
        // update first 2 segments when first segment is vertical 
        | 0,false,_ ->
            makeWireSegment wire.Id wire.Width src {X=src.X;Y=seg.TargetPos.Y} 
        | 1,false,_ when noOfSegments>3 ->
            makeWireSegment wire.Id wire.Width {X=src.X;Y=seg.SourcePos.Y} seg.TargetPos
        | x,false,false when (x= last - 1 && noOfSegments>3 )-> 
            makeWireSegment wire.Id wire.Width seg.SourcePos {X=tgt.X;Y=seg.SourcePos.Y}
        | 1,false,false ->
            makeWireSegment wire.Id wire.Width {X=src.X;Y=seg.SourcePos.Y} {X=tgt.X;Y=seg.SourcePos.Y} 
        // update last 2 segments when first segment is vertical
        | x,false,false when x=last-> 
            makeWireSegment wire.Id wire.Width {X=tgt.X;Y=seg.SourcePos.Y} tgt 
        | x,false,true when (x= last - 1 && noOfSegments>3 )-> 
            makeWireSegment wire.Id wire.Width seg.SourcePos {X=seg.SourcePos.X;Y=tgt.Y}
        | x,false,true when x=last-> 
            makeWireSegment wire.Id wire.Width {X=seg.SourcePos.X;Y=tgt.Y} tgt

        | _ -> seg    
    )
    |> setIndex


    




// RULES FOR DECIDING WIRE ROUTING METHOD: 
// Name must be of format routeRuleXXX
// Must have signature Wire -> Symbol.Model -> bool

/// Rule that chekds if a the number of segments would change after 
/// routing the wire to its new position, as this indicates a need to 
/// switch back to autorouting.
/// 
let routeRuleShortEnds (wire: Wire) (sm: Symbol.Model) (model:Model) : bool =
    not (checkFirstSegmentLength wire sm && checkLastSegmentLength wire sm) 


let routeRuleCaseChange (wire: Wire) (sm: Symbol.Model) (model:Model) : bool =
    let currentSegmentCount = //wire.Segments.Length
        routeWire model.Symbol wire.SourcePortId wire.TargetPortId
        |> List.length
    let newVertexCount = 
        routeWire sm wire.SourcePortId wire.TargetPortId
        |> List.length
    currentSegmentCount = newVertexCount (*-1*) 


/// Rule that states that the wire must switch back to autorouting after symbol rotation
let routeRuleAfterRotation (wire: Wire) (sm: Symbol.Model) (model:Model) : bool =
    (wire.SourcePortEdge, wire.TargetPortEdge) = (Symbol.getPortEdge sm wire.SourcePortId, Symbol.getPortEdge sm wire.TargetPortId) 


/// Rule stating two segment wires may not be autorouted
let routeRuleTwoSegment (wire: Wire) (sm: Symbol.Model) (model:Model): bool =
    not (List.length wire.Segments < 3)

        
let routeRuleList =
    [
        routeRuleCaseChange
        routeRuleAfterRotation
        routeRuleTwoSegment
        routeRuleShortEnds
    ]

/// Decides whether a wire ought to continue being manually routed or be
/// switched back to autorouting, and updates the Manual field accordingly
let decideManual (wire: Wire) (sm: Symbol.Model) (model:Model): Wire =
    routeRuleList 
    |> List.map (fun f -> f wire sm model)
    |> fun x ->
        if List.contains false x then
            {wire with Manual = false} 
        else 
            wire

/// Calculates and returns the IDs of wires that must be updated using a  
/// provided list of symbols that have been updated.
let chooseWiresToUpdate (sIdList: CommonTypes.ComponentId list) (model: Model) (sm: Symbol.Model) 
    : CommonTypes.ConnectionId list =
    sIdList
    |> List.collect (fun sId -> Symbol.getPortIds sm sId)
    |> List.collect (fun pId ->
        model.WX
        |> List.filter (fun w ->
            pId = w.SourcePortId || pId = w.TargetPortId))
    |> List.map (fun w -> w.Id)
    |> List.distinct

/// Updates the wires in the model as specified by the provided list
let updateWires (model: Model) (sm: Symbol.Model) (multipleMove:bool) (wIds: CommonTypes.ConnectionId list)  : Wire list =

    model.WX
    |> List.map (fun w -> 
        if List.contains w.Id wIds then
            let newWire = decideManual w sm model
            let srcEdge = Symbol.getPortEdge sm newWire.SourcePortId
            let tgtEdge = Symbol.getPortEdge sm newWire.TargetPortId
            let startHoriz = not (srcEdge=Symbol.Top || srcEdge=Symbol.Bottom)
            let endHoriz = not (tgtEdge=Symbol.Top || tgtEdge=Symbol.Bottom)
            if newWire.Manual then

                {newWire with Segments = (manualRouteSymbolMove newWire sm multipleMove)}
            else
                {newWire with 
                    Segments = makeWireSegments sm newWire.Id newWire.Width newWire.SourcePortId newWire.TargetPortId 
                    SourcePortEdge = srcEdge
                    TargetPortEdge = tgtEdge
                    StartHoriz = startHoriz
                    EndHoriz = endHoriz}
        else w
    )

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        let wList = 
            match sMsg with
            | Symbol.Move (sIdList,_) -> 
                let multMove= List.length sIdList >1
                chooseWiresToUpdate sIdList model sm
                |>updateWires  model sm multMove 
            | Symbol.Rotate (sIdList,_) ->
                
                chooseWiresToUpdate sIdList model sm
                |>updateWires model sm false
            | Symbol.Scale (sIdList,_) ->
                
                chooseWiresToUpdate sIdList model sm
                |>updateWires model sm false 
            | _ -> model.WX
            
        {model with Symbol=sm; WX = wList}, Cmd.map Symbol sCmd

    | AddWire (portId1,portId2) ->
        let unverifiedWire =
            match Symbol.getPortType model.Symbol portId1 , Symbol.getPortType model.Symbol portId2 with
            | CommonTypes.Output , CommonTypes.Input -> makeNewWire model portId1 portId2 // Wire was drawn from Output to Input
            | CommonTypes.Input , CommonTypes.Output -> makeNewWire model portId2 portId1 // Wire was drawn from Input to Output
            | _ -> makeNewWire model portId1 portId2 // Invalid port combination, will be caught by verifyWire
             
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
                {w with Highlight = List.contains w.Id wIdList}
            )
        {model with WX = wList}, Cmd.none

    | MoveWires (wId,idx,vec) ->
        let a = idx - 1
        let b = idx + 1
        let move (w:Wire)  idx wId vector (seg:WireSegment)=
            
            let last = (List.length w.Segments) - 1
            match idx with
            | 0  -> seg
            | x when (x=last) -> seg
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
        /// Returns false if first or last segment is too short and true otherwise
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
           
          

        let validateWires (wLst: Wire list): bool =
            wLst
            |> List.map (fun w ->
                condition w)
            |> List.contains false
            |> not


        let segLstLst =
            model.WX
            |> List.map (fun w ->
                List.map (fun (seg:WireSegment)-> move w  idx wId vec seg) w.Segments)

        let wList= 
            model.WX
            |> List.mapi (fun i w-> {w with Segments = setIndex segLstLst.[i]})
            |> List.map (fun w ->
                if  w.Id=wId then
                    
                    {w with Manual = true}
                            
                else 
                    w 
            )
        if validateWires wList then 
            {model with WX = wList}, Cmd.none 
        else 
            model, Cmd.none
        

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
    | AddSegment (id, index, mousePos) -> 
        
        let direction (seg:WireSegment) =
            match seg.SourcePos, seg.TargetPos with
            | (src,tar)when tar.X>src.X -> "E"
            |(src,tar)when tar.X<src.X -> "W"
            |(src,tar)when tar.Y>src.Y -> "S"
            |(src,tar)when tar.Y<src.Y -> "N"
            | _ -> failwithf "not a perpendicular wire"


        let addHumpVertices (seg:WireSegment)=
            let pos1= {X=seg.SourcePos.X;Y=seg.SourcePos.Y}
            let pos6= {X=seg.TargetPos.X;Y=seg.TargetPos.Y}
            match direction seg with 

            | "E"->
                
                let pos2= {X=mousePos.X - 10.;Y=seg.SourcePos.Y}
                let pos3= {X=mousePos.X - 10.;Y=seg.SourcePos.Y - 20.}
                let pos4= {X=mousePos.X + 10.;Y=seg.SourcePos.Y - 20.}
                let pos5= {X=mousePos.X + 10.;Y=seg.SourcePos.Y}
                
                [pos1;pos2;pos3;pos4;pos5;pos6]
            | "W"->
                let pos2= {X=mousePos.X + 10.;Y=seg.SourcePos.Y}
                let pos3= {X=mousePos.X + 10.;Y=seg.SourcePos.Y - 20.}
                let pos4= {X=mousePos.X - 10.;Y=seg.SourcePos.Y - 20.}
                let pos5= {X=mousePos.X - 10.;Y=seg.SourcePos.Y}
                [pos1;pos2;pos3;pos4;pos5;pos6]
            | "S" ->
                
                let pos2= {Y=mousePos.Y - 10.;X=seg.SourcePos.X}
                let pos3= {Y=mousePos.Y - 10.;X=seg.SourcePos.X + 20.}
                let pos4= {Y=mousePos.Y + 10.;X=seg.SourcePos.X + 20.}
                let pos5= {Y=mousePos.Y + 10.;X=seg.SourcePos.X}
                
                [pos1;pos2;pos3;pos4;pos5;pos6]
            | "N"->
                let pos2= {Y=mousePos.Y + 10.;X=seg.SourcePos.X}
                let pos3= {Y=mousePos.Y + 10.;X=seg.SourcePos.X + 20.}
                let pos4= {Y=mousePos.Y - 10.;X=seg.SourcePos.X + 20.}
                let pos5= {Y=mousePos.Y - 10.;X=seg.SourcePos.X}
                [pos1;pos2;pos3;pos4;pos5;pos6]                        
            | _ -> failwithf "not a perpendicular wire"
            
            
  
        let humpSegments (seg:WireSegment)=
            addHumpVertices seg
            |> List.pairwise
            |> List.map (fun (src,tgt) -> makeWireSegment id seg.Width src tgt)
        
        let transformSeg(seg:WireSegment) =
            match seg.Index with 
            |x when x=index -> humpSegments seg
            |_ -> [seg]
            | _ -> failwithf "wagwan sexi"

        let segments (w:Wire)=
            w.Segments
            |> List.collect transformSeg
            |> setIndex
        let wList=    
            model.WX
            |> List.map (fun w -> 
                if w.Id = id then
                    {w with Segments=segments w;
                            Manual= true}
                else 
                    w
            )

        {model with WX = wList}, Cmd.none
    | SnapWire (wIds) -> 

        let wList=    
            model.WX
            |> List.map (fun w -> 
                if List.contains w.Id wIds then
                    let portSrc= Symbol.getPortCoords model.Symbol (w.SourcePortId)
                    {w with Segments=snapWireSegments portSrc [] 0 w.Segments}
     
                else 

                    w
            )
        {model with WX = wList}, Cmd.none
        

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

/// Takes a connectionId list and returns the connections in the model not in that list
let getWires (wModel : Model) (cIdList : CommonTypes.ConnectionId list) : CommonTypes.ConnectionId list =
    wModel.WX
    |> List.map (fun x -> x.Id)
    |> List.filter (fun x -> not (List.contains x cIdList))

let getWireSegList (wireId : CommonTypes.ConnectionId) (wModel : Model) : ( CommonTypes.ConnectionId * int) list =
    wModel.WX
    |> List.tryFind (fun w -> w.Id = wireId)
    |> function
    | Some wire -> 
        List.map (fun segment -> segment.wId, segment.Index) wire.Segments
    | None -> failwithf "Could not find wire"

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

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"
