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

Might want to add in portID and symbolID and wireID as types instead of strings for type safety.

## Interface Functions

`Symbol.getPortCoords (portID: string) (model : Model)`
 - Used by BusWire for connecting wires to ports
 - Returns XYPos

`Symbol.getBoundingBoxes (mouseCoord: XYPos) (model : Model)`
 - Returns list of (id: string * topleft: XYPos * bottomright: XYPos)
 - Initially return all bounding boxes

`Symbol.getPortType (portID: string) (model : Model)`
 - Returns if port is input or output

`Symbol.isPort (portCoords: XYPos) (model : Model)`
 - Returns Option type indicating if mouse has clicked down on port
 - `Some (portCoords: XYPos * portID: string)`
 - `None`

## Messages

`Move of symbolIdList *  XYPos`
 - XYPos is a translation vector

`Delete of symbolIdList`
 - Deletes symbols from model based on ID.

`Add of symbolInfo`
 - This still needs discussion
 - Symbol info would be a record with fields: "componentType", "numberInputs", "numberOutputs", "position"
 - Properties like "inputPortList", "outputPortList", "highlighted", "Id", "boundingBoxCoordinates" (this is XYPos * XYPos) can be set up and calculated by Symbol in addition to what is provided.
 - Coords (mouse click position)
 - Look at how ISSIE does it? Issie chooses an arbitrary random position to put the symbol. From here we can drag. This arbitrary initial position changes each time.

`Highlight of symbolIdList`

`HighlightPorts of symbolId`
 - Highlights ports of symbols

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
 