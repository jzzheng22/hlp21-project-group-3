# Interface Documentation for Symbol Module

Owners:
 - Shafir Rahman
 - Joanna Merrick

Any changes must be agreed by owners then communicated to the rest of the group.

Generating symbols and ports.

Has list of symbols.

Each Symbol owns its ports.
 - Ports need to keep track of how many wires are connected to those ports.
 - Internal implementation details: each port could have its own bounding box.
    - If used, this should be invisible to other modules.

## Interface Functions

`Symbol.getPortCoords (symbolModel : Model) (portID: CommonTypes.PortId)`
 - Used by BusWire for connecting wires to ports.
 - Returns `XYPos`.

`Symbol.getBoundingBoxes (symbolModel : Model) (mouseCoord: XYPos)`
 - Returns list of `(id: CommonTypes.ComponentId * topleft: XYPos * bottomright: XYPos)`. Called by Sheet.
 - Initially returns all bounding boxes.

`Symbol.getPortType (symbolModel : Model) (portID: CommonTypes.PortId)`
 - Returns if port is input or output.

`Symbol.isPort (symbolModel : Model) (portCoords: XYPos)`
 - Returns Option type indicating if mouse has clicked down on port. Called by Sheet.
 - `Some (portCoords: XYPos * portID: CommonTypes.PortID)`
 - `None`

`Symbol.getPortIds (model: Model) (symbolId: CommonTypes.ComponentID)`
 - Returns list of ports for each symbol ID.
 - Is used to find wires connected to symbol(s).

*Optional interface functions*
These are called by adidesh20's BusWire and implemented in JEMerrick's Symbol:
`Symbol.getPortWidth (model : Model) (pId : CommonTypes.PortId) : int`
 - Returns width of specified port.

`Symbol.getHostId (model : Model) (pId : CommonTypes.PortId) : CommonTypes.ComponentId`
 - Returns the ComponentID which the PortID belongs to.

 `Symbol.getPortEdge (model : Model) (pId : CommonTypes.PortId) : Edge'
 -Returns the edge of the symbol that the port is on

## Messages
**Received from Sheet via BusWire**

`Move of (CommonTypes.ComponentId list * XYPos)`
 - XYPos is a translation vector.
 - Move symbols in list by given translation vector.

`Add of compType: CommonTypes.ComponentType * pagePos: XYPos * numIn: int * numOut: int`
 - Adds a new symbol based on the provided information.
 - compType: type of component.
 - pagePos: location on canvas.
 - numIn: number of input ports.
    - This number does not include any enable or clock signals
 - numOut: number of output ports.
 - Properties like "inputPortList", "outputPortList", "highlighted", "Id", "boundingBoxCoordinates" (XYPos * XYPos) can be set up and calculated by Symbol in addition to what is provided.
 - Initial position of symbol:
    - ISSIE chooses an arbitrary random position to put the symbol. From here we can drag. 
    - This arbitrary initial position changes each time.

`Delete of CommonTypes.ComponentId list`
 - Deletes symbols from model based on IDs in list.

`Highlight of CommonTypes.ComponentId list`
- Highlight symbols in list.

`HighlightPorts of CommonTypes.ComponentId list`
 - Highlights all ports of symbols in list.

*Optional messages implemented in JEMerrick's Symbol* 
The below messages can also be sent to JEMerrick's version of Symbol:

`Rotate of sId : CommonTypes.ComponentId * rot : int`
 - Rotates a single symbol clockwise by `rot` degrees.
 - Sent from Sheet via BusWire.

`Scale of sId : CommonTypes.ComponentId * scale : XYPos`
 - Scales a symbol by specified factor in X and Y directions.
 - Used to magnify, shrink, stretch and distort symbols.
 - Sent from Sheet via BusWire.

`HighlightError of sIdList: CommonTypes.ComponentId list`
 - Highlights symbols when they are in an error state.
 - Sent from Sheet via BusWire.
 
`DragPort of sId : CommonTypes.ComponentId * pId : CommonTypes.PortId * pagePos: XYPos`
 - Moves the selected port to the port position closest to the mouse.
 - Ports have pre-specified locations where they can be moved to.



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
 
