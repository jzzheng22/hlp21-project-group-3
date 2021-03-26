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
