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

    

    
    // Editor Keybindings (also items on Edit menu)
    // Use Elmish subscriptions to attach external source of events such as keyboard
    // shortcuts. According to electron documentation, the way to configure keyboard
    // shortcuts is by creating a menu.
    let editMenu dispatch =
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

    
        jsOptions<MenuItemOptions> <| fun invisibleMenu ->
            invisibleMenu.``type`` <- MenuItemType.SubMenu
            invisibleMenu.label <- "Edit"
            invisibleMenu.visible <- true // false if you want keys but no "Edit" menu
            invisibleMenu.submenu <-
                [| makeKeyItem "Default" "CmdOrCtrl+S" (fun () -> dispatch KeyboardMsg.CtrlS)
                   makeKeyItem "Red" "Alt+Z" (fun () -> dispatch KeyboardMsg.AltZ)
                   menuSeparator
                   makeKeyItem "Diagram Zoom In" "CmdOrCtrl+z" (fun () -> dispatch KeyboardMsg.ZoomCanvasIn)
                   makeKeyItem "Diagram Zoom Out" "CmdOrCtrl+y" (fun () -> dispatch KeyboardMsg.ZoomCanvasOut)
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
        let menuSeparator =
           let sep = createEmpty<MenuItemOptions>
           sep.``type`` <- MenuItemType.Separator
           sep
        let makeKeyItem (label:string) (accelerator : string) (action : unit -> unit) =
            jsOptions<MenuItemOptions> <| fun item ->
                item.label <- label
                item.accelerator <- accelerator
                item.click <- fun _ _ _ -> action()

    
        jsOptions<MenuItemOptions> <| fun invisibleMenu ->
            invisibleMenu.``type`` <- MenuItemType.SubMenu
            invisibleMenu.label <- "Symbol"
            invisibleMenu.visible <- true // visible
            invisibleMenu.submenu <-
                [| makeKeyItem "Add" "Alt+A" (fun () -> dispatch KeyboardMsg.AltA)
                   makeKeyItem "Delete"  "delete" (fun () -> dispatch KeyboardMsg.Del)
                   menuSeparator
                   makeKeyItem "Align Horizontally" "Alt+C" (fun () -> dispatch KeyboardMsg.AltC)
                   makeKeyItem "Align Vertically" "Alt+V" (fun () -> dispatch KeyboardMsg.AltV)
                   menuSeparator
                   makeKeyItem "Rotate Clockwise" "CmdOrCtrl+Q" (fun () -> dispatch KeyboardMsg.SymbolClockwise)
                   makeKeyItem "Rotate Anticlockwise" "CmdOrCtrl+E" (fun () -> dispatch KeyboardMsg.SymbolAntiClock)
                   makeKeyItem "Magnify" "Alt+M" (fun () -> dispatch KeyboardMsg.SymbolMagnify)
                   makeKeyItem "Shrink"  "Alt+D" (fun () -> dispatch KeyboardMsg.SymbolShrink)
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
    // App
    Program.mkProgram Sheet.init update' view'
    |> Program.withReactBatched "app"
    |> Program.withSubscription attachMenusAndKeyShortcuts
    |> Program.withTrace traceFn
    //|> Program.withConsoleTrace
    |> Program.run
