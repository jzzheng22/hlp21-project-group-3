# Sheet

Top level of data model
Contains a data structure of buswires and symbols
First point of contact for mouse events on canvas

Messages sent using IDs
 - Initial thoughts: message in one module translates to interface function in another module

Message mechanism:
 - `Sheet.update` takes in a message and model, and outputs a tuple of `Model * Cmd<Msg>`
 - `Sheet.update` calls `BusWire.update`
 - `BusWire.update` calls `Symbol.update`
 - I think Sheet should be able to access `Symbol.update` directly without going through BusWire, but this needs some extra thought to prevent conflicts

## Messages
To BusWire:
`AddConnection of wireInfo`
 - Maybe BusWire should call function from Symbol to get port info?
 - Mouse down and then mouse up
 - Selecting ports: this function returns (x,y) coord of port and portId of start and end ports: can pass this into AddConnection

`DeleteConnection of wireId`

`MoveConnection of wireIdList * posX * posY`

`HighlightConnection of wireIdList`

To Symbol:
`MoveSymbol of symbolIdList * posX * posY`
`DeleteSymbol of symbolIdList`
`AddSymbol of symbolInfo`
`HighlightSymbol of symbolIdList`

## Bounding Box

Each buswire and symbol owns its bounding box

Bounding box can be fully defined using top left and bottom right corners
 - Should be slightly bigger than each component to allow for negation bubble

Sheet is able to access bounding boxes of buswires and symbols
`BusWire.getBoundingBoxes`
`Symbol.getBoundingBoxes`
 - Sheet has coordinates of mouse/mouse click
 - Sheet calls `getBoundingBoxes`, which returns a list of the bounding box of each component for that module
 - Sheet performs exhaustive search to find which bounding box(es) are selected

Things to consider:
 - Optimisations: can we pass in coordinates to `getBoundingBoxes`
 - Split up canvas into sections
    - Identify which section the coordinates are in
    - Return bounding boxes in that section only
    - Consider: split in Sheet or in submodules?
    - Boxes which overlap across sections
 - Maybe sort list by coordinate (proximity to bounding box)
    - Might as well just return single bounding box instead of list?

Alternative method:
 - Send coords to BusWire and Symbol
 - BusWire and Symbol run through their list of components
 - Return list of components where mouse coords inside bounding box
 - Sheet sends highlight message to highlight components in those lists

## Highlight
On mouse click down (including click and drag):
1. Sheet identifies which bounding boxes are inside selection
2. Sheet sends a `highlight` message to the selected component(s)
3. The selected component(s) are highlighted using an interface function

e.g. in Sheet:
`BusWire.highlight ListOfBusWireComponents`
`Symbol.highlight ListOfSymbolComponents`

Probably easiest if we only highlight when the mouse is in the bounding box?
 - As in, we don't have a separate function for unhighlighting
 - Once the mouse is out of range, we stop sending 'True'
 - Does that make sense???
 - We did discuss sending a message that says when mouse leaves an area: let me know if you still think this is a good idea

### Highlighting Ports
Ports should become visible when mouse is hovering in range
 - Highlight message sent to Symbol when in range even if not mouse click down
 - These need to highlight ports but not symbol

### Selecting Ports
 - A port is selected if it is highlighted and then mouse click down
 - If we select a port then drag, Sheet draws a dashed line
 - If we drag to another port, Sheet sends message to BusWire to make wire between two ports

Selecting port: on mouse click down:
Call the function `Symbol.isPort xPos yPos`

On mouse click up:
Call the function `Symbol.isPort xPos yPos`
    If yes, tell BusWire to draw new wire

`AddConnection of portA.Id portB.Id`?
 - Called by Sheet on mouse click up if there is a second port
 - I will let BusWire people determine how they want to implement this
 - Initial thoughts: BusWire people can access Port IDs from Symbol, and then get Port coords


## Delete

Delete message from Sheet to BusWire and Symbol
 - If we delete a Symbol, does that delete the BusWires connected to that Symbol?
 - If we delete a BusWire, the Symbol should not be deleted


## Responsibilities of Sheet
 - Click and select multiple components on sheet
 - Click and highlight
 - Draw selection box
 - Change shape of cursor
 - Send message to each component covered by selection box to indicate which are highlighted


# BusWire

Has list of wires and list of symbols
 - Need to consider: what about symbols without wires connected to them?

Wires should be selectable
 - Probably an extension?
 - Wires should do autorouting initially but should be selectable and moveable in the end product

## Interface Functions

`BusWire.getBoundingBoxes`
 - Returns data structure containing IDs, and bounding box coords

## Messages

`Delete of wireIdList`
 - Deletes wires from model

`Add of wireInfo`
 - This still needs discussion
 - Wire info (probably just if bus or wire?)
 - Look at how ISSIE does it?

`Highlight of wireIdList`
 - Highlights wires in list
 - If not in list, should not be highlighted?
    - See discussion above
    - Look at how ISSIE does it?

`Move of wireIdList *  posX * posY`
 - Maybe this should be list of wire IDs
 - posX and posY should probably be translation vector, not new coords
 - This allows for moving multiple wires in one function call

# Symbol

Has list of symbols

Each Symbol owns its ports
 - Ports need to keep track of how many wires are connected to those ports
 - Internal implementation details: each port could have its own bounding box
    - If used, this should be invisible to other modules

## Interface Functions

`Symbol.getPortCoords`
 - Used by BusWire for connecting wires to ports

`Symbol.getBoundingBoxes`
 - Returns data structure containing IDs, and bounding box coords

`Symbol.getPortType portID`
 - Returns if port is input or output

`Symbol.isPort xPos yPos`
 - Returns Option type indicating if mouse has clicked down on port
 - Option of (x, y) coord of port and portID, or None

## Messages

`Move of symbolIdList *  posX * posY`
 - Maybe this should be list of symbol IDs
 - posX and posY should probably be translation vector, not new coords
     - This allows for moving multiple symbols in one function call

`Delete of symbolIdList`
 - Deletes symbols from model

`Add of symbolInfo`
 - This still needs discussion
 - Symbol info would probably include component type, number of inputs/outputs, etc
 - Coords (mouse click position)
 - Look at how ISSIE does it?

`Highlight of symbolIdList`
 - Highlights symbols in list
 - If not in list, should not be highlighted?
    - See discussion above
    - Look at how ISSIE does it?

`HighlightPorts of symbolId`
 - Highlights ports of symbols
 - Maybe need further discussion?