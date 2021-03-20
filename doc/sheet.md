# Interface Documentation for Sheet Module

Owners:
 - Aaman Rebello
 - Jason Zheng

Any changes must be agreed by owners then communicated to the rest of the group.

Top level of data model - linking everything together.
Model of Sheet must contain a data structure of BusWires and Symbols.
First point of contact for mouse events on canvas.

## Responsibilities of Sheet
 - Click, select and highlight multiple components e.g. symbols, wires on sheet i.e. svg canvas.
 - Move selected components over the canvas on dragging the mouse.
 - Highlight the ports of a symbol when the mouse comes within a suitable range of the symbol.
 - Draw selection box. Send message to each component covered by selection box to indicate which components are highlighted.
 - Change shape of cursor.
 - Draw dotted line on dragging from one port to another to create wires between the ports.
 - Extension goals - specific decision for each of the owners of this document.
 

## Messages
Message mechanism:
 - `Sheet.update` takes in a message and model, and outputs a tuple of `Model * Cmd<Msg>`.
 - `Sheet.update` calls `BusWire.update` to send messages to BusWire module.
 - `BusWire.update` calls `Symbol.update`to send messages to Sheet module.
Sheet can also access Symbol directly if needed.


**To Sheet:**
 - From mouse and keyboard - keyboard shortcuts may be bound to dropdown menu options (see *src/Renderer/Renderer.fs*).


**To BusWire:** (see [buswire.md](./buswire.md) for further description)

`AddWire of (CommonTypes.PortId * CommonTypes.PortId)`
 - Creates wire between two ports.
 - Sent by Sheet on mouse click up depending on return value of `Symbol.isPort`.

`DeleteWires of CommonTypes.ConnectionId list`
 - Deletes all wires whose IDs are in the ConnectionID list.
 - Deleting a BusWire does not delete any Symbols connected to it.

`HighlightWires of CommonTypes.ConnectionId list`  
 - Highlights wires in list.

`MoveWires of CommonTypes.ConnectionId list * XYPos`
 - XYPos is a translation vector.
 - Move wires in list by given translation vector.


**To Symbol:** (see [symbol.md](./symbol.md) for description)

`Move of CommonTypes.ComponentId list *  XYPos`
 - XYPos is a translation vector.
 - Move symbols in list by given translation vector.

`Add of compType: CommonTypes.ComponentType * pagePos: XYPos * numIn: int * numOut: int`
 - Adds a new symbol based on the provided information.
 - compType: type of component.
 - pagePos: location on canvas.
 - numIn: number of input ports.
    - This number does not include any enable or clock signals
 - numOut: number of output ports.

`Delete of CommonTypes.ComponentId list`
 - Deletes all symbols whose IDs are in the list.
 - Deleting Symbols deletes any BusWires connected to it.

`Highlight of CommonTypes.ComponentId list`
 - Highlights symbols in list.
 - See also later discussion.

`HighlightPorts of CommonTypes.ComponentId list`
- Highlight all ports of symbols in list.

*Optional messages implemented in JEMerrick's Symbol:*
`Rotate of sId : CommonTypes.ComponentId * rot : int`
 - Rotates a symbol clockwise by `rot` degrees.
 - Unit of rotation is degrees, and limited to ints.

`Scale of CommonTypes.ComponentId * XYPos`
 - Scales a symbol by specified factor in X and Y directions.
 - Used to magnify, shrink, stretch and distort symbols.

`HighlightError of sIdList: CommonTypes.ComponentId list`
 - Highlights symbols when they are in an error state.
 - Sent from Sheet via BusWire.

`DisplaySlots of sId : ComponentId`
- Displays all slots a port may be moved to on the component

`Rename of sId : ComponentId * name : string`
- Changes the label on a component to the name given

## BusWidthInferer interface
`BusWidthInferer.inferConnectionsWidth ((comps,conns) : CanvasState) : Result<ConnectionsWidth, WidthInferError>`
- Takes in a list of Issie Components and Issie Connections and returns 
    * Ok x -> Some <Map<ConnectionId, int Option> | None
    * Error {Msg : string; ConnectionsAffected : ConnectionId list}
- In the first case where we have Ok x and Some y, we simply tell all the connections to update with the specified width
- In the second case where we have Ok x and None, we ask Symbol what the width should be.
    * If both port widths for the connection are equal, update the connection to that width
    * If the port widths are not equal, then send the error highlight messages for the relavent connections/symbols
- In the third case where we have Error then send the error highlight messages for the relavent connections/symbols

## Interface Functions

`BusWire.getBoundingBoxes (mouseCoord: XYPos) (model : Model)`
 - Returns list of `(id: CommonTypes.ComponentId * topLeft: XYPos * bottomRight: XYPos)`
 - Initially just returns all bounding boxes.

`BusWire.getWireIdsFromPortIds (wModel: Model) (portIds: CommonTypes.PortId list) : CommonTypes.ConnectionId list`
 - Takes a list of PortIds as input and returns the IDs of all the wires connected to the supplied ports.

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
 - Returns list of ports for a given symbol ID.
 - Used to find wires connected to a symbol.

*Optional interface functions:*
Called by jzzheng22's Sheet and implemented by JEMerrick's Symbol:
`Symbol.getHostId (model : Model) (pId : CommonTypes.PortId) : CommonTypes.ComponentId`
 - Returns the ComponentID which the PortID belongs to.

`Symbol.getBoundingBox symModel symID`
 - Returns the bounding box for a given Symbol ID
 - Called in jzzheng22's Sheet

## Bounding Box
 - Each buswire and symbol owns its bounding box. 
 - Can be fully defined using top left and bottom right corners. 
 - Should be slightly bigger than each component to allow for negation bubble.

**OPTION 1:** (Currently using this method)
Sheet is able to access bounding boxes of BusWires and Symbols.
`BusWire.getBoundingBoxes (wireModel: Model) (mouseCoord: XYPos)`
 - Return type: `(ID: CommonTypes.ConnectionId * topLeft: XYPos * bottomRight:  XYPos)`.

`Symbol.getBoundingBoxes (symbolModel: Model) (mouseCoord: XYPos)`
 - Return type: `(ID: CommonTypes.ComponentId * topLeft: XYPos * bottomRight:  XYPos)`.

 - Sheet has coordinates of mouse/mouse click.
 - Sheet calls `getBoundingBoxes`, which returns a list of the bounding box of each component for that module.
 - Sheet performs exhaustive search to find which bounding box(es) are selected.

Potential Optimisations:
 - `getBoundingBoxes` takes mouse coordinates as parameter.
    - Can be removed later.
    - Could be used to filter down boxes based on `mouseCoord`.
 - Split up canvas into sections: 
    - Identify which section the coordinates are in and return bounding boxes in that section only.
    - Consider: do we define this split in Sheet or in submodules?
    - How to handle bounding boxes which overlap across multiple sections.
 - Sort list by coordinates (proximity to bounding box) - binary search.
    - Could return single bounding box instead of list.

**OPTION 2**
 1. Send coords to BusWire and Symbol.
 2. BusWire and Symbol run through their list of components.
 3. Return list of components where mouse coords inside bounding box.
 4. Sheet sends highlight message to highlight components in those lists.

## Highlight
On mouse click down (including click and drag):
1. Sheet identifies which bounding boxes are inside selection.
2. Sheet sends a `Highlight` message to the selected component(s).
3. The selected component(s) are highlighted using an interface function.
- Components highlighted only when mouse is in bounding box (no explicit message to unhighlight).

### Highlighting Ports
Ports should become visible when mouse is hovering in range.
 - `Symbol.highlightPorts (symbolIdList: CommonTypes.ComponentId list)` sent to Symbol when in bounding box of symbol.
 - This highlights ports but not symbol.

### Selecting Ports
 - A port is selected if it is highlighted and then mouse click down.
 - If we select a port then drag, Sheet draws a dashed line.
 - If we drag to another port, Sheet sends message to BusWire to make wire between two ports when mouse is released.

Selecting port: on mouse click down, call `Symbol.isPort (symbolModel: Model) (mouseCoord: XYPos)`. Return value:
 - `Some (portCoord: XYPos * portID: CommonTypes.PortID)`: draw dotted line.
 - `None`: don't draw - it may be another case e.g. drag select, selecting an actual component.

On mouse click up: call `Symbol.isPort (symbolModel: Model) (mouseCoord: XYPos)`. Return value:
 - `Some (portCoord: XYPos * portID: CommonTypes.PortID)`: tell BusWire to draw new wire.
 - `None`: don't draw.
In all cases stop rendering the dotted line.

## State outputs (extension)
 - Inferred width of wires (to BusWire and ISSIE)

## Stretch Goals
Zooming, snap-to-grid etc can hopefully be implemented without adding to the above existing interface.
