# Interface Documentation for Sheet Module

Owners:
 - Aaman Rebello
 - Jason Zheng

Any changes must be agreed by owners then communicated to the rest of the group.

Top level of data model - linking everything together.
Model of Sheet must contain a data structure of BusWires and Symbols.
First point of contact for mouse events on canvas

Might want to add in portID and symbolID and wireID as types instead of strings for type safety. **UPDATE:** The interface functions between sheet and the other modules make use of the type CommonTypes.ComponentID for symbol IDs, CommonTypes.ConnectionID for wire IDs and CommonTypes.PortID for portIDs. These provide type safety and can easily be converted to strings and back. 

## Responsibilities of Sheet
 - Click, select and highlight multiple components e.g. symbols, wires on sheet i.e. svg canvas.
 - Move selected components over the canvas on dragging the mouse.
 - Highlight the ports of a symbol when the mouse comes within a suitable range of the symbol.
 - Draw selection box. Send message to each component covered by selection box to indicate which components are highlighted.
 - Change shape of cursor
 - Draw dotted line on dragging from one port to another to create wires between the ports.
 - Extension goals - specific decision for each of the owners of this document.
 

## Messages
Message mechanism:
 - `Sheet.update` takes in a message and model, and outputs a tuple of `Model * Cmd<Msg>`
 - `Sheet.update` calls `BusWire.update` to send messages to BusWire module.
 - `BusWire.update` calls `Symbol.update`to send messages to Sheet module.
Sheet can also access Symbol directly if needed (implementation decision - will NOT affect the interfaces).

**To Sheet:**
 - From mouse and keyboard - keyboard shortcuts my be bound to dropdown menu options (see *src/Renderer/Renderer.fs*).

**To BusWire:** (see [buswire.md](./buswire.md) for description)

`AddWire of (CommonTypes.PortId * CommonTypes.PortId)`
 - Message to create a wire between two ports - whether busWire actually does this always is an implementation decision (e.g. input port to input port is probably not allowed).
 - WireInfo is tuple of portID `(startPort: CommonTypes.PortId * endPort: CommonTypes.PortId)`
 - Sent by Sheet on mouse click up depending on return value of `Symbol.isPort`

`DeleteWires of CommonTypes.ConnectionId list`
 - Deletes all wires whose IDs are in the ConnectionID list.
 - Deleting a BusWire does not delete any Symbols connected to it

`MoveWires of CommonTypes.ConnectionId list * XYPos`
- Moves all wires in the list according to a translation vector of type XYPos. The vector takes the form {X = x_translation; Y = y_translation}.
- wireSegmentIdList is a list of wire segment IDs

`HighlightWires of CommonTypes.ConnectionId list`
- Message to highlight all wires in the WireIdList of type CommonTypes.ConnectionId list.
- WireIdList is a list of wire IDs

**To Symbol:** (see [symbol.md](./symbol.md) for description)

`Move of CommonTypes.ComponentId list *  XYPos`
- Moves all symbols whose Ids are in the list, according to a translation vector of type XYPos.
- The translation vector takes the form {X = x_translation; Y = y_translation}.

`Delete of CommonTypes.ComponentId list`
 - Deletes all symbols whose IDs are in the list.
 - Deleting Symbols deletes any BusWires connected to it

`Add of symbolInfo`
- Adds a new symbol based on the provided information.
- symbolinfo is a tuple: (compType: CommonTypes.ComponentType * pagePos : XYPos * numIn : int * numOut : int)
- compType indicates the type of created component, pagePos indicates where in the canvas to put the new symbol, numIn and numOut indicate the number of input and output ports.

`Highlight of CommonTypes.ComponentId list`
 - Message to highlight all symbols in the list.
 - See also later discussion

`HighlightPorts of CommonTypes.ComponentId list`
- Message to highlight all the ports on each symbol in the list.
- Ports are intended to become coloured and more prominent.

*Two optional messages (for extended version of Symbol used by JEMErrick)*

`Rotate of CommonTypes.ComponentId * int`
- Used to rotate a symbol of a given Id. The amount of rotation is in degrees and specified as an integer.

`Scale of CommonTypes.ComponentId * XYPos`
- Used to scale a symbol of a given id in the x and y directions. The scaling factor in X and Y directions is specified in a variable of type XYPos.

**To ISSIE:** 
 - Infer widths (extension)

 ## Bounding Box

**OPTION 1:** (Currently using this method)
Each buswire and symbol owns its bounding box. Bounding box can be fully defined using top left and bottom right corners. It should be slightly bigger than each component to allow for negation bubble.

Sheet is able to access bounding boxes of buswires and symbols
`BusWire.getBoundingBoxes (wireModel: Model) (mousecoord: XYPos)`
`Symbol.getBoundingBoxes (symbolModel: Model) (mousecoord: XYPos)`
These functions will return the top-left and bottom-right corners of the boxes along with the ids of the related components `(id: IDtype * topleft: XYPos * bottomright:  XYPos)`
 - IDtype may be CommonTypes.ComponentId or CommonTypes.ConnectionId for Symbol or BusWire respectively.
 - Sheet has coordinates of mouse/mouse click
 - Sheet calls `getBoundingBoxes`, which returns a list of the bounding box of each component for that module
 - Sheet performs exhaustive search to find which bounding box(es) are selected - hopefully we make this more efficient.

Things to consider:
 - Optimisations: We pass in mouse coordinates to `getBoundingBoxes` for now, even though it may not be needed. We can delete later of course, although we may also be able to filter down the boxes based on mouseCoord.
 - Split up canvas into sections: Identify which section the coordinates are in and return bounding boxes in that section only. Consider: do we define this split in Sheet or in submodules?. Bounding boxes which overlap across sections - what do you do?
 - Maybe sort list by coordinate (proximity to bounding box) - binary search.
    - Might as well just return single bounding box instead of list?

**OPTION 2**
 - Send coords to BusWire and Symbol
 - BusWire and Symbol run through their list of components
 - Return list of components where mouse coords inside bounding box
 - Sheet sends highlight message to highlight components in those lists

## Highlight
On mouse click down (including click and drag):
1. Sheet identifies which bounding boxes are inside selection
2. Sheet sends a `highlight` message to the selected component(s)
3. The selected component(s) are highlighted using an interface function

e.g. in Sheet we would send these messages:
`BusWire.HighlightWires of (WireIdList: CommonTypes.ConnectionID list)`
`Symbol.Highlight of (SymbolIdList: CommonTypes.ComponentID list)`

Highlight components only when mouse is in bounding box (i.e. no unhighlight message). Components not highlighted when mouse not in bounding box i.e. the corresponding symbol/wire would not be in the list passed with the message.

### Highlighting Ports
Ports should become visible when mouse is hovering in range
 - Highlight message (called `Symbol.highlightPorts (symbolIdList: CommonTypes.ComponentId list)` sent to Symbol when in bounding box of symbol and not mouse click down.
 - These need to highlight ports but not symbol - the highlighting for port is different from the highlighting for symbols.

### Selecting Ports
 - A port is selected if it is highlighted and then mouse click down
 - If we select a port then drag, Sheet draws a dashed line
 - If we drag to another port, Sheet sends message to BusWire to make wire between two ports

Selecting port: on mouse click down, call the interface function in Symbol module: `Symbol.isPort (symbolModel: Model) (mouseCoord: XYPos)`. Return value:
 - `Some (portCoord: XYPos * portID: CommonTypes.PortID)`: draw dotted line.
 - `None`: don't draw - it may be another case e.g. drag select, selecting an actual component.

On mouse click up: call the function `Symbol.isPort (symbolModel: Model) (mouseCoord: XYPos)`. Return value:
 - `Some (portCoord: XYPos * portID: CommonTypes.PortID)`: tell BusWire to draw new wire.
 - `None`: don't draw.
In all cases stop rendering the dotted line.

## State outputs (extension)
 - Inferred width of wires (to BusWire and ISSIE)

## Stretch Goals
Zooming, snap-to-grid etc can hopefully be implemented without adding to the above existing interface.
