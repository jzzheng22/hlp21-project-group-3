# External interfaces

This describes code interfaces. UI interfaces are described under [*Features.md*](Features.md)

## BusWire

`extractWires (wModel : Model) : CommonTypes.Connection list`
- This function will convert the wire model from the Wire datatype into the Issie compliant Connection datatype

## Symbol

`extractComponents (symModel : Model) : CommonTypes.Component list`
- This function with convert the model from a Symbol list to an Issie Component list

## Sheet

`Add symbol`
- Pressing Alt+A currently adds a single MUX to the canvas.
- This is detected as a KeyPress message in sheet. The update function of sheet in turn sends a `Symbol.Add of Symbol.SymbolAdd` message to the Symbol module, which then adds the new symbol. The *Symbol.SymbolAdd* type contains the information to create the new Symbol and is currently hardcoded to a mux with two inputs and one output.
- To make this work with an external catalog, we merely need to dispatch the `Symbol.Add of Symbol.SymbolAdd` with information relevant to the selected symbol.
