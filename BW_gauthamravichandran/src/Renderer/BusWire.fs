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

type WireSegment = {
    wId: CommonTypes.ConnectionId
    Index: int 
    SrcPos: XYPos
    TargetPos: XYPos
    BB: XYPos * XYPos
    Highlight: bool
    Width:int
    }
type Wire = {
    Id: CommonTypes.ConnectionId 
    SrcPort: CommonTypes.PortId
    TargetPort: CommonTypes.PortId
    Segments: WireSegment list
    WireHighlight: bool
    Width: int
    Manual:bool
    
    }

type Model = {
    Symbol: Symbol.Model
    WX: Wire list
    Color: CommonTypes.HighLightColor
    }

let makeWireBB (sPos:XYPos) (tPos:XYPos)=
    match sPos.X,sPos.Y, tPos.X, tPos.Y with
    | (sx,sy,tx,ty) when tx>sx -> ({X=sx;Y=sy-5.},{X=tx;Y=ty+5.})
    | (sx,sy,tx,ty) when tx<sx -> ({X=tx;Y=ty-5.},{X=sx;Y=sy+5.})
    | (sx,sy,tx,ty) when ty>sy -> ({X=sx-5.;Y=sy},{X=tx+5.;Y=ty})
    | (sx,sy,tx,ty) when ty<sy -> ({X=tx-5.;Y=ty},{X=tx+5.;Y=sy})
    | (sx,sy,tx,ty) when (tx=sx && ty=sy) -> ({X=tx;Y=ty},{X=sx;Y=sy})
    | _ -> failwithf "diagonal line error"

let makeWireSegment (WId:CommonTypes.ConnectionId) (sPos) (tPos) (width:int) = 
    //printf "%A" (makeWireBB sPos tPos)
    {
        wId=WId
        Index=0
        SrcPos=sPos
        TargetPos=tPos
        BB= makeWireBB sPos tPos
        Highlight=false
        Width=width
    }

let bbCollision (bb1:XYPos*XYPos) (bb2:XYPos*XYPos) =
    match fst bb1, snd bb1, fst bb2, snd bb2 with
    | (tL1, bR1, tL2, bR2) when bR2.Y < tL1.Y -> false
    | (tL1, bR1, tL2, bR2) when tL2.X > bR1.X -> false
    | (tL1, bR1, tL2, bR2) when tL2.Y > bR1.Y -> false
    | (tL1, bR1, tL2, bR2) when bR2.X < tL1.X -> false
    |  _ ->true
(*
let findNearestBox (symbols: Symbol.Symbol list) (src:XYPos) =
    let list =
        Symbol.getBoundingBoxes symbols
        |> List.filter (fun (sym,l,r)-> (l.X>src.X && r.Y>src.Y && src.Y>l.Y)  ) 
    if List.isEmpty list 
        then None
    else 
        List.minBy (fun (sym,l,r)->l) list
        |> (fun (sym,l,r)->Some sym)
*)
let goPastBox (wId:CommonTypes.ConnectionId) (src:XYPos) (bb:XYPos*XYPos) (width:int) =

    match (fst bb), (snd bb) with
    | (tl,br) when tl.X> src.X ->
        let tar= snd bb
        let pos1= {X= (tar.X + 12.* src.X)/(13.) ; Y=src.Y}
        let pos2= {X=pos1.X;Y=tar.Y + 20.}
        
        [   
        makeWireSegment wId src pos1 width
        makeWireSegment wId pos1 pos2 width
        
        ], pos2
    | (tl,br) when tl.X< src.X ->
        let tar= snd bb
        let pos1= {X= br.X + 10. ; Y=src.Y}
        let pos2= {X=pos1.X;Y=tar.Y + 20.}
        
        [   
        makeWireSegment wId src pos1 width
        makeWireSegment wId pos1 pos2 width
        
        ], pos2
    | (tl,br) when tl.Y> src.Y ->
        let tar= snd bb
        let pos1= {X= br.X+10. ; Y=src.Y}
        let pos2= {X=pos1.X;Y=br.Y + 20.}
        
        [   
        makeWireSegment wId src pos1 width
        makeWireSegment wId pos1 pos2 width
        
        ], pos2
    | (tl,br) when br.Y< src.Y ->
        let tar= snd bb
        let pos1= {X= br.X+10. ; Y=src.Y}
        let pos2= {X=pos1.X;Y=tl.Y - 10.}
        
        [   
        makeWireSegment wId src pos1 width
        makeWireSegment wId pos1 pos2 width
        
        ], pos2
    | _ -> failwithf "nah mate"






let goToTarget (wId:CommonTypes.ConnectionId) (src:XYPos) (tar:XYPos) (width:int) =
    let pos1= {X= ( 12. * tar.X + src.X)/(13.) ; Y=src.Y}
    let pos2= {X=pos1.X;Y=tar.Y}
    [   
        makeWireSegment wId src pos1 width
        makeWireSegment wId pos1 pos2 width
        makeWireSegment wId pos2 tar width
    ]
    


let findBoxesInPath (srcSym: CommonTypes.ComponentId) (tarSym: CommonTypes.ComponentId) (symbols: Symbol.Symbol list) (src:XYPos) (tar:XYPos)=
    let newsrc= {X= src.X + 5. ; Y= src.Y}
    let newtar= {X= tar.X - 5. ; Y= tar.Y}
    let pos1= {X= (12.* tar.X + src.X)/(13.) ; Y=src.Y}
    let pos2= {X=pos1.X;Y=tar.Y}
    let bb1= makeWireBB newsrc pos1
    let bb2= makeWireBB pos1 pos2
    let bb3= makeWireBB pos2 newtar
    // printf "%A" (bb1,bb2,bb3)
    let list =
        Symbol.getBoundingBoxes symbols src
        |> List.filter (fun (sym,a,b)-> not (sym=srcSym || sym=tarSym))
        |> List.map (fun(a,b,c)->(b,c)) 
        |> List.filter (fun x-> (bbCollision bb1 x || bbCollision bb2 x || bbCollision bb3 x))
    if List.isEmpty list 
        then None
    else 
        printf "%A" (List.minBy id list)
        Some (List.minBy id list)
        //|> (fun (l,r)->Some sym)


let rec routing (srcSym: CommonTypes.ComponentId) (tarSym: CommonTypes.ComponentId) (wId:CommonTypes.ConnectionId) (symbols: Symbol.Symbol list) (src:XYPos) (tar:XYPos) (width:int)=
    
    match findBoxesInPath srcSym tarSym symbols src tar with
    | None -> goToTarget wId src tar width
    //| Some sym when sym.Id = tar.Id-> goToTarget wId src tar
    | Some bb -> 
        let tuple= goPastBox wId src bb width
        ( fst tuple) @ (routing srcSym tarSym wId (symbols) (snd tuple) tar width)
    

    //goToTarget  wId src tar width


let setIndex (lst: WireSegment list) =
    lst 
    |> List.mapi (fun i seg-> {seg with Index= i})

let makeWire (srcId:CommonTypes.PortId) (tarId:CommonTypes.PortId) (symbols: Symbol.Symbol list) (width:int) =
    let srcSym= Symbol.getHostId symbols srcId
    let tarSym= Symbol.getHostId symbols tarId 
    let wId= CommonTypes.ConnectionId (uuid())
    let segments = routing srcSym tarSym wId symbols (Symbol.getPortCoords symbols srcId) (Symbol.getPortCoords symbols tarId) width
    {
    Id= wId 
    SrcPort= srcId
    TargetPort= tarId
    Segments= setIndex segments
    WireHighlight=false
    Width=width
    Manual= false
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
    | MoveWires of int * CommonTypes.ConnectionId  * XYPos
    | SetColor of CommonTypes.HighLightColor
    | MouseMsg of MouseT




/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire option =
    wModel.WX
    |> List.tryFind (fun wire -> wire.Id = wId)

type WireRenderProps = {
    Index : int
    WireP: WireSegment
    SrcP: XYPos 
    TgtP: XYPos
    ColorP: string
    StrokeWidthP: string
    Highlight:bool
    BB: XYPos * XYPos
    Width: int

    }

/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.

let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let segmentView=
                [
                line [
                    X1 props.SrcP.X
                    Y1 props.SrcP.Y
                    X2 props.TgtP.X
                    Y2 props.TgtP.Y
                    // Qualify these props to avoid name collision with CSSProp
                    SVGAttr.Stroke (if props.Width =1 then props.ColorP else "purple" )
                    SVGAttr.StrokeWidth props.StrokeWidthP ] []
                ]
            let widthAnnotation = 
                let textPos = Symbol.posAdd props.SrcP {X = 5. ; Y = 5.}
                if  props.Index=0 then 
                    [
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
                    ]  
                else
                    []
            let highlightBox =
                    if props.Highlight = false then
                        []
                    else
                        [
                        rect[
                            let h= float (snd props.BB).Y - float (fst props.BB).Y
                            let w= float (snd props.BB).X - float (fst props.BB).X
                            X (fst props.BB).X
                            Y (fst props.BB).Y
                            SVGAttr.Height (h)
                            SVGAttr.Width (w)
                            SVGAttr.Fill "deepskyblue"
                            SVGAttr.Stroke "deepskyblue"
                            SVGAttr.Opacity 0.4
                            SVGAttr.StrokeWidth 0.5][]
                        ]
            segmentView @ widthAnnotation @ highlightBox |> ofList      
            )

let view (model:Model) (dispatch: Dispatch<Msg>)=

    let wires = 
        model.WX

        |> List.map (fun w ->
            List.map (fun (segment:WireSegment)->               
                let props = {
                    Index = segment.Index
                    WireP = segment
                    (*SrcP = Symbol.symbolPos model.Symbol w.SrcSymbol 
                    TgtP = Symbol. symbolPos model.Symbol w.TargetSymbol *)
                    SrcP=segment.SrcPos
                    TgtP=segment.TargetPos
                    //SrcP=w.SrcPos
                    //TgtP=w.TargetPos
                    ColorP = model.Color.Text()
                    StrokeWidthP = "2px" 
                    Highlight=w.WireHighlight
                    BB= segment.BB
                    Width=segment.Width
                    
                    
                    }
                
                singleWireView props) w.Segments)
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    g [] [(g [] (List.concat wires)); symbols]

/// dummy init for testing: real init would probably start with no wires.
/// this initialisation is not realistic - ports are not used
/// this initialisation depends on details of Symbol.Model type.
    /// 


let init n () =
    let symbols, cmd = Symbol.init()
    let symIds = List.map (fun (sym:Symbol.Symbol) -> sym.Id) symbols
    let rng = System.Random 0
    (*
    let makeRandomWire() =
        let n = symIds.Length
        let s1,s2 =
            match rng.Next(0,n-1), rng.Next(0,n-2) with
            | r1,r2 when r1 = r2 -> 
                symbols.[r1],symbols.[n-1] // prevents wire target and source being same
            | r1,r2 -> 
                symbols.[r1],symbols.[r2]
        {
            Id=CommonTypes.ConnectionId (uuid())
            SrcSymbol = s1.Id
            TargetSymbol = s2.Id
        }
        *)

    // findNearestBox symbols (symbols.[0].TopL)
    // |> goPastBox (symbols.[0].TopL)

    [
        //printfn (Symbol.getPortIds symbols symbols.[0].Id)
        //makeWire  ((Symbol.getPortIds symbols symbols.[0].Id).[0]) ((Symbol.getPortIds symbols symbols.[1].Id).[0]) symbols
        //makeWire  ((Symbol.getPortIds symbols symbols.[2].Id).[0]) ((Symbol.getPortIds symbols symbols.[3].Id).[0]) symbols
    ]
    //List.map (fun i -> makeRandomWire()) [1..n]
    |> (fun wires -> {WX=wires;Symbol=symbols; Color=CommonTypes.Red},Cmd.none)
(*
let isCommon lst1 lst2=
    let set1= Set.ofList lst1
    let set2 =Set.ofList lst2
    let intersect= Set.intersect set1 set2
    not (Set.isEmpty intersect)

let isSegmentInCommon (w:Wire) (segIdlst:CommonTypes.ConnectionId list)=
    let WireSegIds=List.map (fun (seg:WireSegment)->seg.Id) w.Segments
    isCommon WireSegIds segIdlst 
*)

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        let manualRoute wire =
            let noOfSegments= List.length wire.Segments
            let last = noOfSegments - 1
            let src= Symbol.getPortCoords sm wire.SrcPort
            let tar= Symbol.getPortCoords sm wire.TargetPort
            
            wire.Segments
            |> List.mapi (fun i seg -> 
                match i with 
                |0 ->makeWireSegment wire.Id src {X=seg.TargetPos.X;Y=src.Y} wire.Width
                |1 when noOfSegments>3 ->makeWireSegment wire.Id {X=seg.SrcPos.X;Y=src.Y} seg.TargetPos wire.Width
                |x when (x= last - 1 && noOfSegments>3 )-> makeWireSegment wire.Id seg.SrcPos {X=seg.SrcPos.X;Y=tar.Y}  wire.Width
                |1->makeWireSegment wire.Id {X=seg.SrcPos.X;Y=src.Y} {X=seg.SrcPos.X;Y=tar.Y} wire.Width
                |x when x=last-> makeWireSegment wire.Id {X=seg.SrcPos.X;Y=tar.Y} tar wire.Width

                |_ -> seg    
                )
            |> setIndex
        let wires = 
            model.WX
            |> List.map (fun w ->
                let srcSym= Symbol.getHostId sm w.SrcPort
                let tarSym= Symbol.getHostId sm w.TargetPort 
                let segments=routing srcSym tarSym w.Id sm (Symbol.getPortCoords sm w.SrcPort) (Symbol.getPortCoords sm w.TargetPort) (Symbol.getPortWidth model.Symbol w.SrcPort)
                
                if w.Manual=false then 
                    {w with Segments= setIndex segments}
                else 
                    {w with Segments= manualRoute w}
            )
        
        {model with Symbol=sm; WX=wires}, Cmd.map Symbol sCmd
    | AddWire (srcPort, tarPort) ->
        let w= makeWire srcPort tarPort model.Symbol (Symbol.getPortWidth model.Symbol srcPort)
        {model with WX=w::model.WX}, Cmd.none
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
                    {w with WireHighlight = true}
                else 
                    {w with WireHighlight = false}
            )
        
        {model with WX = wList}, Cmd.none
    | MoveWires (idx,wId,vec) ->

        let a = idx - 1
        let b = idx + 1
        let move (w:Wire) (seg:WireSegment) idx wId =
            let last = (List.length w.Segments) - 1

            match idx with
            |0  -> failwithf "do nothing"
                (*let pos1= {X=seg.SrcPos.X ;Y=seg.SrcPos.Y}
                let pos2= {X=seg.SrcPos.X ;Y=seg.SrcPos.Y+vec.Y}
                let pos3= {X=seg.TargetPos.X  ;Y=seg.SrcPos.Y+vec.Y}

                match seg.Index with 
                |x when x=idx-> 
                    [ 
                     //makeWireSegment wId seg.SrcPos pos1 w.Width   
                     //makeWireSegment wId pos1 pos2 w.Width 
                     makeWireSegment wId pos2 pos3 w.Width
                    ]
                |x when x=b -> [makeWireSegment wId {X=seg.SrcPos.X;Y=seg.SrcPos.Y+vec.Y} seg.TargetPos w.Width]
                | _ -> [seg] *)
            |x when (x=last && seg.Index=last ) -> failwithf "do nothing"
            |_ when w.Id <> wId -> seg
            |_ when idx%2=0 -> 
                match seg.Index with 
                |x when x=a -> makeWireSegment wId seg.SrcPos {X=seg.TargetPos.X;Y=seg.TargetPos.Y+vec.Y} w.Width
                |x when x=idx-> makeWireSegment wId {X=seg.SrcPos.X;Y=seg.SrcPos.Y+vec.Y} {X=seg.TargetPos.X;Y=seg.TargetPos.Y+vec.Y} w.Width
                |x when x=b -> makeWireSegment wId {X=seg.SrcPos.X;Y=seg.SrcPos.Y+vec.Y} seg.TargetPos w.Width
                | _ -> seg
            |_ when idx%2=1 ->
                match seg.Index with 
                |x when x=a -> makeWireSegment wId seg.SrcPos {X=seg.TargetPos.X+vec.X;Y=seg.TargetPos.Y} w.Width
                |x when x=idx-> makeWireSegment wId {X=seg.SrcPos.X+vec.X;Y=seg.SrcPos.Y} {X=seg.TargetPos.X+vec.X;Y=seg.TargetPos.Y} w.Width
                |x when x=b -> makeWireSegment wId {X=seg.SrcPos.X+vec.X;Y=seg.SrcPos.Y} seg.TargetPos w.Width
                |_ -> seg

        let segLstLst =
            model.WX
            |> List.map (fun w ->
                List.map (fun (seg:WireSegment)-> 
                    (*   
                    if (seg.Index+1,w.Id) = (idx,wId)then
                        makeWireSegment wId seg.SrcPos {X=seg.TargetPos.X;Y=seg.TargetPos.Y+vec.Y} w.Width
                        //{seg with TargetPos={X=seg.TargetPos.X;Y=seg.TargetPos.Y+vec.Y}  }
                    elif (seg.Index,w.Id) = (idx,wId) then
                        makeWireSegment wId {X=seg.SrcPos.X;Y=seg.SrcPos.Y+vec.Y} {X=seg.TargetPos.X;Y=seg.TargetPos.Y+vec.Y} w.Width
                        //{seg with SrcPos={X=seg.SrcPos.X;Y=seg.SrcPos.Y+vec.Y}; TargetPos={X=seg.TargetPos.X;Y=seg.TargetPos.Y+vec.Y}}
                    elif (seg.Index-1,w.Id) = (idx,wId) then
                        makeWireSegment wId {X=seg.SrcPos.X;Y=seg.SrcPos.Y+vec.Y} seg.TargetPos w.Width
                        //{seg with SrcPos={X=seg.SrcPos.X;Y=seg.SrcPos.Y+vec.Y}  }
                    else    
                        seg
                    *)
                (*
                match (seg.Index,w.Id) with 
                |x when x=(a,wId) -> makeWireSegment wId seg.SrcPos {X=seg.TargetPos.X;Y=seg.TargetPos.Y+vec.Y} w.Width
                |x when x=(idx,wId)-> makeWireSegment wId {X=seg.SrcPos.X;Y=seg.SrcPos.Y+vec.Y} {X=seg.TargetPos.X;Y=seg.TargetPos.Y+vec.Y} w.Width
                |x when x=(b,wId) -> makeWireSegment wId {X=seg.SrcPos.X;Y=seg.SrcPos.Y+vec.Y} seg.TargetPos w.Width
                | _ -> seg
                *)
                    
                    move w seg idx wId
                    //move w segment.Index vec
                ) w.Segments
            )
        

        let wires= 
            model.WX
            |> List.mapi (fun i w-> {w with Segments = setIndex segLstLst.[i]})
            |> List.map (fun w ->
                if  w.Id=wId  then
                    {w with Manual = true}
                else 
                    {w with Manual = false}
            )
        {model with WX = wires}, Cmd.none        

    | SetColor c -> {model with Color = c}, Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))

//---------------Other interface functions--------------------//
let getBoundingBoxes (wModel: Model) (mouseCoord: XYPos): ( int * CommonTypes.ConnectionId * XYPos * XYPos) list=
    wModel.WX
    |> List.collect (fun w->
                    List.map (fun (segment:WireSegment)->
                                (segment.Index, segment.wId,fst segment.BB, snd segment.BB))w.Segments)


let getWireIdsFromPortIds (wModel: Model) (portIds: CommonTypes.PortId list) : CommonTypes.ConnectionId list =
    wModel.WX
    |> List.filter (fun w -> List.contains w.SrcPort portIds || List.contains w.TargetPort portIds)
    |> List.map (fun w -> w.Id)
/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Used
/// to determine which wire (if any) to select on a mouse click
let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option = 
    failwith "Not implemented"

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"



    



