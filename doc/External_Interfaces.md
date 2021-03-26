# External interfaces

This describes code interfaces. UI interfaces are described under [*Features.md*](Features.md)

## BusWire

`extractWires (wModel : Model) : CommonTypes.Connection list`
- This function will convert the wire model from the Wire datatype into the Issie compliant Connection datatype

## Symbol

`extractComponents (symModel : Model) : CommonTypes.Component list`
- This function with convert the model from a Symbol list to an Issie Component list

## Sheet

### Interface with WidthInferer
- Whenever a wire is added or deleted (or a symbol is deleted)
- UpdateWidths message is called
- This first calls the extract functions in symbol and buswire to convert to issie types
- Then sends this into widthInferer
- In order to get all the error messages, if there was an error, it must send each one into widthInferer seperately and concatenate the results

`Add symbol`
- Pressing Alt+A currently adds a single MUX to the canvas.
- This is detected as a KeyPress message in sheet. The update function of sheet in turn sends a `Symbol.Add of Symbol.SymbolAdd` message to the Symbol module, which then adds the new symbol. The *Symbol.SymbolAdd* type contains the information to create the new Symbol and is currently hardcoded to a mux with two inputs and one output.
- To make this work with an external catalog, we merely need to dispatch the `Symbol.Add of Symbol.SymbolAdd` with information relevant to the selected symbol.
