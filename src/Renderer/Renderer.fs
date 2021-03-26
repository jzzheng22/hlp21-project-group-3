module Renderer

open Elmish
open Elmish.React
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Browser

open Helpers
open Electron

open Sheet

let menuSeparator =
    let sep = createEmpty<MenuItemOptions>
    sep.``type`` <- MenuItemType.Separator
    sep
let makeRoleItem (role:MenuItemRole) =
    jsOptions<MenuItemOptions> <| fun item ->
        item.role <- role
let makeKeyItem (label:string) (accelerator : string) (action : unit -> unit) =
    jsOptions<MenuItemOptions> <| fun item ->
        item.label <- label
        item.accelerator <- accelerator
        item.click <- fun _ _ _ -> action()

// Editor Keybindings (also items on Edit menu)
// Use Elmish subscriptions to attach external source of events such as keyboard
// shortcuts. According to electron documentation, the way to configure keyboard
// shortcuts is by creating a menu.
let editMenu dispatch =
    jsOptions<MenuItemOptions> <| fun invisibleMenu ->
        invisibleMenu.``type`` <- MenuItemType.SubMenu
        invisibleMenu.label <- "Edit"
        invisibleMenu.visible <- true // false if you want keys but no "Edit" menu
        invisibleMenu.submenu <-
            [| makeKeyItem "Zoom Canvas In" "CmdOrCtrl+]" (fun () -> dispatch KeyboardMsg.ZoomCanvasIn)
               makeKeyItem "Zoom Canvas Out" "CmdOrCtrl+[" (fun () -> dispatch KeyboardMsg.ZoomCanvasOut)
               menuSeparator
               makeKeyItem "Print Statistics" "Alt+Shift+Z" (fun () -> dispatch KeyboardMsg.AltShiftZ)
               makeRoleItem MenuItemRole.ForceReload
               makeRoleItem MenuItemRole.Reload
               makeRoleItem MenuItemRole.ToggleDevTools
               makeRoleItem MenuItemRole.ZoomIn
               makeRoleItem MenuItemRole.ZoomOut|]
            |> U2.Case1

///Menu for operations relating to symbols    
let symbolMenu dispatch =    
    jsOptions<MenuItemOptions> <| fun invisibleMenu ->
        invisibleMenu.``type`` <- MenuItemType.SubMenu
        invisibleMenu.label <- "Symbol"
        invisibleMenu.visible <- true // visible
        invisibleMenu.submenu <-
            [| makeKeyItem "Add" "Alt+A" (fun () -> dispatch KeyboardMsg.SymbolAddBegin)
               makeKeyItem "Delete"  "delete" (fun () -> dispatch KeyboardMsg.Del)
               menuSeparator
               makeKeyItem "Align Horizontally" "Alt+X" (fun () -> dispatch KeyboardMsg.AltX)
               makeKeyItem "Align Vertically" "Alt+Y" (fun () -> dispatch KeyboardMsg.AltY)
               menuSeparator
               makeKeyItem "Rotate Clockwise" "CmdOrCtrl+Q" (fun () -> dispatch KeyboardMsg.SymbolClockwise)
               makeKeyItem "Rotate Anticlockwise" "CmdOrCtrl+E" (fun () -> dispatch KeyboardMsg.SymbolAntiClock)
               makeKeyItem "Magnify" "Alt+M" (fun () -> dispatch KeyboardMsg.SymbolMagnify)
               makeKeyItem "Shrink"  "Alt+D" (fun () -> dispatch KeyboardMsg.SymbolShrink)
               makeKeyItem "Copy"  "CmdOrCtrl+Shift+C" (fun () -> dispatch KeyboardMsg.CtrlShiftC)
               makeKeyItem "Cancel"  "Esc" (fun () -> dispatch KeyboardMsg.Esc)

            |]
            |> U2.Case1


let attachMenusAndKeyShortcuts dispatch =
    let sub dispatch =
        let menu = 
            [| editMenu dispatch
               symbolMenu dispatch
            |]          
            |> Array.map U2.Case1
            |> electron.remote.Menu.buildFromTemplate   
        menu.items.[0].visible <- Some true
        electron.remote.app.applicationMenu <- Some menu
    Cmd.map KeyPress (Cmd.ofSub sub)   

let update' = fun msg -> recordExecutionTimeStats "Update" (Sheet.update msg)
let view'  = recordExecutionTimeStats "View" Sheet.view
let printMsg (msg:Msg) =
    match msg with
    | Wire (BusWire.Msg.MouseMsg busWireMouseMsg) -> sprintf "BusWireMsg:%A" busWireMouseMsg.Op
    | KeyPress key -> sprintf "%A" key
    | Wire (BusWire.Msg.Symbol (Symbol.Msg.MouseMsg symMouseMsg)) -> sprintf "SymbolMsg:%A"  symMouseMsg.Op
    | x -> sprintf "Other:%A" x

let traceFn (msg:Msg) model = printfn "Msg=%A\n\n" (printMsg msg)

let mutable firstPress = true

///Used to listen for pressing down of Ctrl for selection toggle
let keyPressListener initial = 
    let subDown dispatch =
        document.addEventListener("keydown", fun e ->
                                                let ke: KeyboardEvent = downcast e
                                                if ke.ctrlKey && firstPress then 
                                                    firstPress <- false 
                                                    dispatch Sheet.ToggleSelectionOpen
                                                else 
                                                    ())
    let subUp dispatch = 
        document.addEventListener("keyup", fun e -> 
                                                    firstPress <- true
                                                    dispatch Sheet.ToggleSelectionClose)
    Cmd.batch [Cmd.ofSub subDown; Cmd.ofSub subUp] 
    

// App
Program.mkProgram Sheet.init update' view'
|> Program.withReactBatched "app"
|> Program.withSubscription attachMenusAndKeyShortcuts
|> Program.withSubscription keyPressListener
|> Program.withTrace traceFn
//|> Program.withConsoleTrace
|> Program.run
