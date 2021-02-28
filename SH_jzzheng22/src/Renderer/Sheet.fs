module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type Model = {
    Wire: BusWire.Model
    Symbol: Symbol.Model
    Zoom: float 
    SelectedPorts: CommonTypes.PortId list
    SelectedComponents: CommonTypes.ComponentId list
    SelectedWires: CommonTypes.ConnectionId list
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | Del

type Msg =
    | Wire of BusWire.Msg
    | Symbol of Symbol.Msg
    | KeyPress of KeyboardMsg
    | SelectPort of (XYPos * CommonTypes.PortId)
    | SelectComponents of (XYPos * CommonTypes.ComponentId list)
    | SelectWires of (XYPos * CommonTypes.ConnectionId list)


/// This function zooms an SVG canvas by transforming its content and altering its size.
/// Currently the zoom expands based on top left corner. Better would be to collect dimensions
/// current scroll position, and chnage scroll position to keep centre of screen a fixed point.
let displaySvgWithZoom (model: Model) (svgReact: ReactElement) (dispatch: Dispatch<Msg>)=
    let sizeInPixels = sprintf "%.2fpx" ((1000. * model.Zoom))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = ev.buttons <> 0.
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev:Types.MouseEvent) = 
        dispatch <| Wire (BusWire.MouseMsg {Op = op ; Pos = { X = ev.clientX / model.Zoom ; Y = ev.clientY / model.Zoom}})
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
            ] 
          OnMouseDown (fun ev -> 
            let coordX = ev.clientX / model.Zoom
            let coordY = ev.clientY / model.Zoom
            let mousePos = {X = coordX; Y = coordY}

            match Symbol.isPort model.Symbol mousePos with
            | Some (portCoords, portId) -> dispatch <| SelectPort (portCoords, portId)
            | None -> 
                let symbolList = Symbol.getBoundingBoxes model.Symbol mousePos
                if List.isEmpty symbolList then
                  
                failwithf "asdf"
            
            (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
        ]
        [ svg
            [ Style 
                [
                    Border "3px solid green"
                    Height sizeInPixels
                    Width sizeInPixels           
                ]
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" model.Zoom)]] // top-level transform style attribute for zoom
                [ 
                    text [ // a demo text svg element
                        X 500; 
                        Y 50; 
                        Style [
                            TextAnchor "middle" // horizontal algnment vs (X,Y)
                            DominantBaseline "middle" // vertical alignment vs (X,Y)
                            FontSize "40px"
                            FontWeight "Bold"
                            Fill "Green" // font color
                        ]
                    ] [str "sample text"]

                    svgReact // the application code

                    polygon [ // a demo svg polygon triangle written on top of the application
                        SVGAttr.Points "10,10 900,900 10,900"
                        SVGAttr.StrokeWidth "5px"
                        SVGAttr.Stroke "Black"
                        SVGAttr.FillOpacity 0.1
                        SVGAttr.Fill "Blue"] []
                ]
            ]
        ]



/// for the demo code
let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom model wireSvg dispatch
       

let update (msg : Msg) (model : Model): Model * Cmd<Msg> =
    match msg with
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    | Symbol sMsg ->
        let sModel, sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol = sModel}, Cmd.map Symbol sCmd
    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none // do nothing else and return model unchanged
    | KeyPress s -> // all other keys are turned into SetColor commands
        let c =
            match s with
            | AltC -> CommonTypes.Blue
            | AltV -> CommonTypes.Green
            | AltZ -> CommonTypes.Red
            | _ -> CommonTypes.Grey
        printfn "Key:%A" c
        model, Cmd.ofMsg (Wire <| BusWire.SetColor c)
    | SelectPort spMsg -> 
        let mousePos, portId = spMsg
        failwithf "Not implemented"
    | SelectComponents scMsg -> failwithf "Not implemented"
    | SelectWires swMsg -> failwithf "Not implemented"

let init() = 
    let model,cmds = (BusWire.init 400)()
    {
        Wire = model
        Symbol = model.Symbol
        Zoom = 1.0
        SelectedPorts = []
        SelectedComponents = []
        SelectedWires = []
    }, Cmd.map Wire cmds
