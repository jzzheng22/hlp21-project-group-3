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
 - I think Sheet should be able to access `Symbol.update` directly without going through BusWire, but this needs some extra thought to prevent conflicts.
 - *For now* let's assume that Sheet has to access Symbol through BusWire, this is something that is very easy to change afterwards.

## Messages
To BusWire:
`AddConnection of wireInfo`
 - Maybe BusWire should call function from Symbol to get port info?
 - Mouse down and then mouse up
 - Selecting ports: this function returns (x,y) coord of port and portId of start and end ports: can pass this into AddConnection
 - A good idea that we could start with is we pass the ids of the two ports with AddConnection as a tuple (i.e. wireInfo is a tuple of port IDs). Then, gathering the port IDs from this message, BusWire can query Symbol for the coordinates of the ports (Let's call the function getCoordsOfPort portID which is string -> XYPos).

`DeleteConnection of wireId`

`MoveConnection of wireIdList * posX * posY`
- This may not necessarily be a list of wires...this is likely to be a list of wire segments although I am not sure how Aditya is handling wires. If he's making separate entities for each wire
segment then the message should carry a list of wire segment IDs.

`HighlightConnection of wireIdList`
- However, this should be a list of wires since when we select a wire segment the whole wire would be highlighted.

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
These functions will return the top-left and bottom-right corners of the boxes along with the ids of the related components (id: string * topleft: XYPos * bottomright:  XYPos)
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
NOTE: The above two optimisations can work together. Passing the coordinates could allow Symbol and BusWire to return only a subset of all the bounding boxes. To this end, it makes sense to pass coordinates as a parameter to `getBoundingBoxes`. To start with, the functions would return all the boxes, but later on the function could use the coordinates to filter the boxes down.

It would make sense to define canvas sections in Symbol or globally. This depends on whether the symbol people are ok with making this definition or not.

 - Maybe sort list by coordinate (proximity to bounding box)
    - Might as well just return single bounding box instead of list?

Alternative method:
 - Send coords to BusWire and Symbol
 - BusWire and Symbol run through their list of components
 - Return list of components where mouse coords inside bounding box
 - Sheet sends highlight message to highlight components in those lists

*NOTE:* For the purpose of clarity, we will not go with this alternative method - we will be using the first method i.e. symbol and buswire would have functions that return bounding boxes as tuples of XYPos.

## Highlight
On mouse click down (including click and drag):
1. Sheet identifies which bounding boxes are inside selection
2. Sheet sends a `highlight` message to the selected component(s)
3. The selected component(s) are highlighted using an interface function

e.g. in Sheet:
`BusWire.highlightComponent ListOfBusWireComponents`
`Symbol.highlightComponent ListOfSymbolComponents`

Probably easiest if we only highlight when the mouse is in the bounding box?
 - As in, we don't have a separate function for unhighlighting
 - Once the mouse is out of range, we stop sending 'True'
 - Does that make sense???
 - We did discuss sending a message that says when mouse leaves an area: let me know if you still think this is a good idea
 - (Aaman Personal Opinion): I think it might be easier to send an unhighlight message to symbol and buswire - this would result in all the components being marked as unselected. When we send a highlight message, the relevant components would be set as selected - possibly through a property of symbol and buswire components.


### Highlighting Ports
Ports should become visible when mouse is hovering in range
 - Highlight message (called `Symbol.highlightPort portIdList`) sent to Symbol when in range even if not mouse click down
 - These need to highlight ports but not symbol - the highlighting for port (turning black) is different from the highlighting for symbols (turning green)

### Selecting Ports
 - A port is selected if it is highlighted and then mouse click down
 - If we select a port then drag, Sheet draws a dashed line
 - If we drag to another port, Sheet sends message to BusWire to make wire between two ports

Selecting port: on mouse click down:
Call the function `Symbol.isPort xPos yPos`

On mouse click up:
Call the function `Symbol.isPort xPos yPos`
    If yes, tell BusWire to draw new wire

`AddConnection of portA.Id portB.Id`
 - Called by Sheet on mouse click up if there is a second port
 - I will let BusWire people determine how they want to implement this
 - Initial thoughts: BusWire people can access Port IDs from Symbol, and then get Port coords


## Delete

Delete message from Sheet to BusWire and Symbol
 - If we delete a Symbol, does that delete the BusWires connected to that Symbol? YES. Symbol will need to send a message to BusWire - `Symbol.DeleteInOutput PortID`
     - Needs to be discussed more here: BusWire needs to tell Symbol to delete - Symbol cannot talk to BusWire
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
 - Returns data structure containing IDs, and bounding box coords (id: string * topleft: XYPos* bottomright XYPos)

## Messages

`Delete of wireIdList`
 - Deletes wires from model

`Add of wireInfo`
 - This still needs discussion
 - Wire info (probably just if bus or wire?)
 - Look at how ISSIE does it?

`Highlight of wireIdList`
 - Highlights wires in list (NOTE: In Issie, only the ports are highlighted but ok)
 - If not in list, should not be highlighted?
    - See discussion above
    - Look at how ISSIE does it?

`Move of wireIdList *  posX * posY`
 - Maybe this should be list of wire IDs
 - posX and posY should probably be translation vector, not new coords
 - NOTE: The above point is correct, we can calculate this vector in sheet and pass the values.
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
 - posX and posY should probably be translation vector (IT WILL), not new coords
     - This allows for moving multiple symbols in one function call

`Delete of symbolIdList`
 - Deletes symbols from model

`Add of symbolInfo`
 - This still needs discussion
 - Symbol info would probably include component type, number of inputs/outputs, etc
 - Coords (mouse click position)
 - Look at how ISSIE does it? Issie chooses an arbitrary position to put the symbol. From here we can drag. This arbitrary initial position changes each time.

`Highlight of symbolIdList`
 - Highlights symbols in list
 - If not in list, should not be highlighted?
    - See discussion above
    - Look at how ISSIE does it?

`HighlightPorts of symbolId`
 - Highlights ports of symbols
 - Maybe need further discussion?
