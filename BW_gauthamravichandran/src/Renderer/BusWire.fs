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
    Id: CommonTypes.ConnectionId 
    SrcPos: XYPos
    TargetPos: XYPos
    BB: XYPos * XYPos
    }
type Wire = {
    Id: CommonTypes.ConnectionId 
    SrcSymbol: CommonTypes.ComponentId
    TargetSymbol: CommonTypes.ComponentId

    Segments: WireSegment list
    }

type Model = {
    Symbol: Symbol.Model
    WX: Wire list
    Color: CommonTypes.HighLightColor
    }

let makeWireBB (sPos:XYPos) (tPos:XYPos)=
    match sPos.X,sPos.Y, tPos.X, tPos.Y with
    | (sx,sy,tx,ty) when tx>sx -> ({X=sx;Y=sy+2.},{X=tx;Y=ty-2.})
    | (sx,sy,tx,ty) when tx<sx -> ({X=tx;Y=ty+2.},{X=sx;Y=sy-2.})
    | (sx,sy,tx,ty) when ty>sy -> ({X=sx-2.;Y=sy},{X=tx+2.;Y=ty})
    | (sx,sy,tx,ty) when ty<sy -> ({X=tx-2.;Y=ty},{X=sx+2.;Y=sy})
    | (sx,sy,tx,ty) when (tx=sx && ty=sy) -> ({X=tx;Y=ty},{X=sx;Y=sy})
    | _ -> failwithf "diagonal line error"

let makeWireSegment (sPos) (tPos) = 
    {
        Id=CommonTypes.ConnectionId (uuid())
        SrcPos=sPos
        TargetPos=tPos
        BB= makeWireBB sPos tPos
    }
let bbCollision (bb1:XYPos*XYPos) (bb2:XYPos*XYPos) =
    match fst bb1, snd bb1, fst bb2, snd bb2 with
    | (tL1, bR1, tL2, bR2) when bR2.Y < tL1.Y -> false
    | (tL1, bR1, tL2, bR2) when tL2.X > bR1.X -> false
    | (tL1, bR1, tL2, bR2) when bR2.Y > tL1.Y -> false
    | (tL1, bR1, tL2, bR2) when bR2.X < tL1.X -> false
    |_ ->true

let findNearestBox (symbols: Symbol.Symbol list) (src:XYPos) =
    let list =
        Symbol.getBoundingBoxes symbols
        |> List.filter (fun (sym,l,r)-> (l.X>src.X && r.Y>src.Y && src.Y>l.Y)  ) 
    if List.isEmpty list 
        then None
    else 
        List.minBy (fun (sym,l,r)->l) list
        |> (fun (sym,l,r)->Some sym)

let goPastBox (src:XYPos) (sym:Symbol.Symbol) =

    
    let pos1= {X=src.X; Y=src.Y}
    let pos2= {X=pos1.X;Y=sym.BotR.Y}
    [   
        makeWireSegment src pos1
        makeWireSegment pos1 pos2
        makeWireSegment pos2 sym.BotR
    ], sym.BotR
    
let goPastBox2 (src:XYPos) (sym:Symbol.Symbol) =

    
    let pos1= {X=src.X; Y=src.Y}
    let pos2= {X=pos1.X;Y=sym.BotR.Y}
    [   
        makeWireSegment src pos1
        makeWireSegment pos1 pos2
        makeWireSegment pos2 sym.BotR
    ], sym.BotR
    





let goToTarget (src:XYPos) (tar:Symbol.Symbol) =
    let pos1= {X= (tar.TopL.X + src.X)/(2.) ; Y=src.Y}
    let pos2= {X=pos1.X;Y=tar.TopL.Y}
    [   
        makeWireSegment src pos1
        makeWireSegment pos1 pos2
        makeWireSegment pos2 tar.TopL
    ]
 
let findBoxesInPath (symbols: Symbol.Symbol list) (src:XYPos) (tar:Symbol.Symbol)=
    let pos1= {X= (tar.TopL.X + src.X)/(2.) ; Y=src.Y}
    let pos2= {X=pos1.X;Y=tar.TopL.Y}
    let bb1= makeWireBB src pos1
    let bb2= makeWireBB pos1 pos2
    let bb3= makeWireBB pos2 tar.TopL

    let list =
        Symbol.getBoundingBoxes symbols
        |> List.map (fun (sym,l,r)->(l,r))
        |> List.filter (fun x-> (bbCollision bb1 x || bbCollision bb2 x || bbCollision bb3 x))
    if List.isEmpty list 
        then None
    else 
        Some (List.minBy (fun (l,r)->l) list)
        //|> (fun (l,r)->Some sym)


let rec routing (symbols: Symbol.Symbol list) (src:XYPos) (tar:Symbol.Symbol)=
(*
    match findNearestBox symbols src with
    | None -> goToTarget src tar
    | Some sym when sym.Id = tar.Id-> goToTarget src tar
    | Some sym -> 
        let tuple= goPastBox src sym
        ( fst tuple) @ (routing (symbols) (snd tuple) tar)
*)
    goToTarget  src tar

let makeWire (src:Symbol.Symbol) (tar:Symbol.Symbol) (symbols: Symbol.Symbol list) =
    
    let segments = routing symbols src.BotR tar
    {
    Id= CommonTypes.ConnectionId (uuid()) 
    SrcSymbol= src.Id
    TargetSymbol= tar.Id
    Segments= segments
    }
//----------------------------Message Type-----------------------------------//

/// Messages to update buswire model
/// These are OK for the demo - but not the correct messages for
/// a production system. In the real system wires must connection
/// to ports, not symbols. In addition there will be other changes needed
/// for highlighting, width inference, etc
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (CommonTypes.ConnectionId * CommonTypes.ConnectionId)
    | SetColor of CommonTypes.HighLightColor
    | MouseMsg of MouseT




/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    failwithf "Not impelmented"

type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: WireSegment
    SrcP: XYPos 
    TgtP: XYPos
    ColorP: string
    StrokeWidthP: string }

/// react virtual DOM SVG for one wire
/// In general one wire will be multiple (right-angled) segments.

let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            line [
                X1 props.SrcP.X
                Y1 props.SrcP.Y
                X2 props.TgtP.X
                Y2 props.TgtP.Y
                // Qualify these props to avoid name collision with CSSProp
                SVGAttr.Stroke props.ColorP
                SVGAttr.StrokeWidth props.StrokeWidthP ] [])


let view (model:Model) (dispatch: Dispatch<Msg>)=

    let wires = 
        model.WX

        |> List.map (fun w ->
            List.map (fun segment->               
                let props = {
                    key = w.Id
                    WireP = segment
                    (*SrcP = Symbol.symbolPos model.Symbol w.SrcSymbol 
                    TgtP = Symbol. symbolPos model.Symbol w.TargetSymbol *)
                    SrcP=segment.SrcPos
                    TgtP=segment.TargetPos
                    //SrcP=w.SrcPos
                    //TgtP=w.TargetPos
                    ColorP = model.Color.Text()
                    StrokeWidthP = "2px" }
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
        makeWire  symbols.[0] symbols.[1] symbols
        makeWire  symbols.[2] symbols.[3] symbols
    
    ]
    //List.map (fun i -> makeRandomWire()) [1..n]
    |> (fun wires -> {WX=wires;Symbol=symbols; Color=CommonTypes.Red},Cmd.none)



let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        let wires = 
            model.WX
            |> List.map (fun w ->
                let segments=routing sm ((Symbol.symbolPos sm w.SrcSymbol).BotR) (Symbol.symbolPos sm w.TargetSymbol)
                {w with Segments= segments})

        {model with Symbol=sm; WX=wires}, Cmd.map Symbol sCmd
    | AddWire _ -> failwithf "Not implemented"
    | SetColor c -> {model with Color = c}, Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))

//---------------Other interface functions--------------------//

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



    



