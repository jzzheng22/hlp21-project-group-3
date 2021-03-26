# External interfaces

## BusWire

`extractWires (wModel : Model) : CommonTypes.Connection list`
- This function will convert the wire model from the Wire datatype into the Issie compliant Connection datatype

## Symbol

`extractComponents (symModel : Model) : CommonTypes.Component list`
- This function with convert the model from a Symbol list to an Issie Component list

## Sheet

`Add symbol`
- Pressing Alt+A currently adds a single MUX to the canvas
- This is detected as a KeyPress message in sheet, and calls `addSymbol (model : Model)`
- To make this work with an external catalog, it would need to take in the ComponentType as well, and replace the 'Mux2' values with that