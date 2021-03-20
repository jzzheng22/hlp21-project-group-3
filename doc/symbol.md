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
 - Returns list of port IDs for a given symbol ID.
 - Used to find wires connected to a symbol.

`Symbol.getPortWidth (model : Model) (pId : CommonTypes.PortId) : int`
 - Returns width of specified port.

`Symbol.getHostId (model : Model) (pId : CommonTypes.PortId) : CommonTypes.ComponentId`
 - Returns the ComponentID which the PortID belongs to.
 - This function is also called by jzzheng22's Sheet

`Symbol.getBoundingBox symModel symID`
 - Returns the bounding box for a given Symbol ID
 - Called in jzzheng22's Sheet

 `Symbol.getPortEdge (model : Model) (pId : CommonTypes.PortId) : Edge'
 - Returns the edge of the symbol that the port is on

 `isLabel (model : Model) (pos : XYPos) (sId : CommonTypes.ComponentId) : (XYPos * CommonTypes.PortId) Option`
 - Returns an option if the user clicked on a port label or not

## Issie interface functions

`getPort (model : Model) (pId : PortId) : Port`
- Returns the CommonTypes.Port object of a given pId
- Used by buswire to convert to Issie datatypes

`symToIssie (sym : Symbol) : Component`
- Converts our Symbol type to Issie Component

`extractComponent (symModel: Model) (sId:ComponentId) : Component`
- Finds a component in the model from its ComponentId, and converts this into the Issie Component

`extractComponents (symModel: Model) : Component list`
- Converts the model from a Symbol list to a Issie Component list

## Messages
**Received from Sheet via BusWire**

`Move of (CommonTypes.ComponentId list * XYPos)`
 - XYPos is a translation vector.
 - Move symbols in list by given translation vector.

`Add of compType: CommonTypes.ComponentType * pagePos: XYPos * numIn: int * numOut: int * i : int`
 - Adds a new symbol based on the provided information.
 - compType: type of component.
 - pagePos: location on canvas.
 - numIn: number of input ports.
    - This number does not include any enable or clock signals
 - numOut: number of output ports.
 - i : the number indicating how many of that symbol are currently on the sheet

`Delete of CommonTypes.ComponentId list`
 - Deletes symbols from model based on IDs in list.

`Highlight of CommonTypes.ComponentId list`
- Highlight symbols in list.

`HighlightPorts of CommonTypes.ComponentId list`
 - Highlights all ports of symbols in list.

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

 `DisplaySlots of sId : CommonTypes.ComponentId`
 - Highlights all possible positions a port may be moved to on the symbol

`Rename of sId : CommonTypes.ComponentId * name : string`
- Changes the label of the component to a given string

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
 
