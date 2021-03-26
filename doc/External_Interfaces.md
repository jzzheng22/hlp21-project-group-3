# External interfaces

This describes code interfaces. UI interfaces are described under [*Features.md*](Features.md)

## BusWire

`extractWires (wModel : Model) : CommonTypes.Connection list`
- This function will convert the wire model from the Wire datatype into the ISSIE-compliant Connection datatype

## Symbol

`extractComponents (symModel : Model) : CommonTypes.Component list`
- This function with convert the model from a Symbol list to an ISSIE Component list

## Sheet

### Interface with WidthInferer
- Whenever a wire is added or deleted (or a symbol is deleted)
- `UpdateWidths` message is dispatched
- This first calls the `extract` functions in Symbol and BusWire to convert to ISSIE types
- Then sends this into `widthInferer`
- In order to get all the error messages, if there was an error, Sheet sends each one into `widthInferer` separately and concatenate the results

### Adding symbols
- Pressing `Alt+A` currently adds a single MUX to the canvas.
- This is detected as a KeyPress message in Sheet. The update function of Sheet sends a `Symbol.Add of Symbol.SymbolAdd` message to the Symbol module, which then adds the new symbol. 
    * The `Symbol.SymbolAdd` type contains the information to create the new Symbol. In Sheet this is currently hardcoded to a mux with two inputs and one output.
    * The `copyElements` function shows the use of `Symbol.SymbolAdd` when copying elements.
    * `Symbol.SymbolAdd` is a record type containing:
   
        ```
        type SymbolAdd = {
            CompType : ComponentType
            PagePos: XYPos
            Input: int
            Output: int
            Index: int    
        }
        ```
    
    * `CompType` is the component type, `PagePos` is where the top left corner of the symbol should be, `Input` and `Output` are the number of required input and output ports, respectively, and `Index` is the number at the end of the symbol label.

- To make this work with an external catalog, we merely need to dispatch the `Symbol.Add of Symbol.SymbolAdd` message with information relevant to the selected symbol.

### Copy and Paste/Duplication
- `Renderer.fs` detects `KeyboardMsg.CtrlShiftC` and sends this message to Sheet. This starts 'copying' mode.
- The list of currently selected Components and Connections are immediately copied, and will follow the position of the mouse.
- On a mouse click, the copied elements are dropped on the canvas at the mouse position.
- To cancel a copy and exit copying mode, press `Delete`. To drop the currently selected elements and then exit copying mode, press `Esc`.
- Known issues:
    * If the symbols that are connected to any selected wires are not selected when entering copying mode, then copying will not work. That is, to copy a wire, the symbols that are connected to that wire must also be selected. You cannot copy a wire that is 'floating'.
    * CtrlShiftV not yet implemented: this should drop the copied components at a programmer-defined location relative to the current mouse position. They should drop as 'selected' so the user can then move these elements around with ease.
    * Pressing `Esc` should not drop the currently copied components; these should be deleted instead.
    * Performance issue when adding too many wires. When you add too many wires, the program's performance declines sharply. This is not an issue when adding symbols.