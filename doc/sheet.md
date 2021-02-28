# Interface Documentation for Sheet Module

Owners:
 - Aaman Rebello
 - Jason Zheng

Any changes must be agreed by owners then communicated to the rest of the group.

Top level of data model - linking everything together
Contains a data structure of BusWires and Symbols
First point of contact for mouse events on canvas

Might want to add in portID and symbolID and wireID as types instead of strings for type safety.

## Responsibilities of Sheet
 - Click and select multiple components on sheet
 - Click and highlight
 - Draw selection box
 - Change shape of cursor
 - Send message to each component covered by selection box to indicate which are highlighted

## Messages
Message mechanism:
 - `Sheet.update` takes in a message and model, and outputs a tuple of `Model * Cmd<Msg>`
 - `Sheet.update` calls `BusWire.update`
 - `BusWire.update` calls `Symbol.update`
Sheet can also access Symbol directly.

To Sheet:
 - From mouse and keyboard

To BusWire: (see [buswire.md](./buswire.md) for description)
`AddConnection of wireInfo` 
 - WireInfo is tuple of portID `(startPort: string * endPort: string)`
 - Sent by Sheet on mouse click up depending on return value of `Symbol.isPort`

`DeleteConnection of wireId`
 - Deleting a BusWire does not delete any Symbols connected to it

`MoveConnection of wireIdList * posX * posY`
- wireSegmentIdList is a list of wire segment IDs

`HighlightConnection of wireIdList`
- WireIdList is a list of wire IDs

To Symbol: (see [symbol.md](./symbol.md) for description)
`MoveSymbol of symbolIdList * posX * posY`

`DeleteSymbol of symbolIdList`
 - Deleting Symbols deletes any BusWires connected to it

`AddSymbol of symbolInfo`

`HighlightSymbol of symbolIdList`(see later discussion and the end for description)

To ISSIE: 
 - Infer widths (extension)

 ## Bounding Box

**OPTION 1:** (Currently using this method)
Each buswire and symbol owns its bounding box. Bounding box can be fully defined using top left and bottom right corners. It should be slightly bigger than each component to allow for negation bubble.

Sheet is able to access bounding boxes of buswires and symbols
`BusWire.getBoundingBoxes mouseCoord model`
`Symbol.getBoundingBoxes mouseCoord model`
These functions will return the top-left and bottom-right corners of the boxes along with the ids of the related components `(id: string * topleft: XYPos * bottomright:  XYPos)`
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
`BusWire.Highlight WireIdList model`
`Symbol.Highlight SymbolIdList model`

Highlight components only when mouse is in bounding box (i.e. no unhighlight message). Components not highlighted when mouse not in bounding box

### Highlighting Ports
Ports should become visible when mouse is hovering in range
 - Highlight message (called `Symbol.highlightPort symbolId`) sent to Symbol when in bounding box of symbol and not mouse click down.
 - These need to highlight ports but not symbol - the highlighting for port (turning black) is different from the highlighting for symbols (turning green)

### Selecting Ports
 - A port is selected if it is highlighted and then mouse click down
 - If we select a port then drag, Sheet draws a dashed line
 - If we drag to another port, Sheet sends message to BusWire to make wire between two ports

Selecting port: on mouse click down, call the function `Symbol.isPort mouseCoord: XYPos`. Return value:
 - `Some (portCoord: XYPos * portID: string)`: draw dotted line.
 - `None`: don't draw - it may be another case e.g. drag select, selecting an actual component.

On mouse click up: call the function `Symbol.isPort mouseCoord: XYPos`. Return value:
 - `Some (portCoord: XYPos * portID: string)`: tell BusWire to draw new wire.
 - `None`: don't draw.
In all cases stop rendering the dotted line.

## State outputs (extension)
 - Inferred width of wires (to BusWire and ISSIE)

## Stretch Goals
