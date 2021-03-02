# Interface Documentation for Symbol Module

Owners:
 - Shafir Rahman
 - Joanna Merrick

Any changes must be agreed by owners then communicated to the rest of the group.

Generating symbols and ports

Has list of symbols

Each Symbol owns its ports
 - Ports need to keep track of how many wires are connected to those ports
 - Internal implementation details: each port could have its own bounding box
    - If used, this should be invisible to other modules

Might want to add in portID and symbolID and wireID as types instead of strings for type safety. **UPDATE:** As Ids for symbols, wires and ports we use the types CommonTypes.ComponentId, CommonTypes.ConnectionId, CommonTypes.PortId respectively.

## Interface Functions

`Symbol.getPortCoords (symbolModel : Model) (portID: CommonTypes.PortId)`
 - Used by BusWire for connecting wires to ports
 - Returns XYPos

`Symbol.getBoundingBoxes (symbolModel : Model) (mouseCoord: XYPos)`
 - Called by sheet. Returns list of (id: CommonTypes.ComponentId * topleft: XYPos * bottomright: XYPos)
 - Initially return all bounding boxes.

`Symbol.getPortType (symbolModel : Model) (portID: CommonTypes.PortId)`
 - Returns if port is input or output

`Symbol.isPort (symbolModel : Model) (portCoords: XYPos)`
 - Returns Option type indicating if mouse has clicked down on port. Called by sheet.
 - `Some (portCoords: XYPos * portID: string)`
 - `None`

`Symbol.getPortIds (model: Model) (symbolId: CommonTypes.ComponentID)`
- Returns list of ports for each symbol ID.
- Is used to find wires connected to symbol(s).


## Messages

`Move of (CommonTypes.ComponentId list * XYPos)`
 - XYPos is a translation vector
 - Received from Sheet.

`Delete of CommonTypes.ComponentId list`
 - Deletes symbols from model based on ID.
 - Received from Sheet.

`Add of symbolInfo`
 - This still needs discussion **UPDATE:** The below arrangement was decided upon.
 - Symbol info would be a record with fields: "componentType", "position", "numberInputs", "numberOutputs"
 - Properties like "inputPortList", "outputPortList", "highlighted", "Id", "boundingBoxCoordinates" (this is XYPos * XYPos) can be set up and calculated by Symbol in addition to what is provided.
 - Coords - how to choose initial position of symbol. Look at how ISSIE does it? Issie chooses an arbitrary random position to put the symbol. From here we can drag. This arbitrary initial position changes each time.
 - Received from Sheet.

`Highlight of CommonTypes.ComponentId list`
- Receieved from Sheet to highlight symbols.

`HighlightPorts of CommonTypes.ComponentId list`
 - Highlights ports of symbols - ISSIE highlights ports on multiple symbols if the mouse is in range of all of them, so the list helps to highlight ports on multiple symbols.

**EXTRA based on additional features inplemented by JEMerrick**
 
The below messages can also be sent to JEMerrick's version of Symbol:

`Rotate of sId : CommonTypes.ComponentId * rot : int`
- Used to rotate a single selected symbol 'rot' degrees clockwise. Can be called from Sheet.

`Scale of sId : CommonTypes.ComponentId * scale : XYPos`
- The 'scale' parameter can scale a symbol by a specified factor in X and Y directions (corresponding to the X and Y fields in the XYPos type). This is used to magnify, shrink, stretch and distort symbols. Again can be called from Sheet.





## State Outputs
 - SymbolBBs (to Sheet and BusWire)
 - Symbol port positions (to Sheet and BusWire)
 - Symbol port type (to Sheet)
 - Symbol information (to BusWire)
 - Connections (with highlight info) (to ISSIE: extension)

## Symbol needs to be able to do these:
 - Move symbol (from Sheet)
 - Delete symbols (from Sheet)
 - Add symbols (from Sheet and ISSIE)
 - Highlight symbols (from Sheet and ISSIE)
 - Change symbol (extension)
 - Init canvas (extension)

 ## Stretch Goals
 
