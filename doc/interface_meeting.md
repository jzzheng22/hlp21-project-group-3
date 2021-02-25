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
 - *For now* let's assume that Sheet has to access Symbol through BusWire, this is something that is very easy to change afterwards. **According to TC this is fine as long as we don't pass to Symbol a datatype defined in sheet (I think). It seems that no conflicts would arise at all from giving Sheet access to Symbol.**

## Messages
To BusWire:
`AddConnection of wireInfo` (see end for description)
 - Maybe BusWire should call function from Symbol to get port info?
 - Mouse down and then mouse up
 - Selecting ports: this function returns (x,y) coord of port and portId of start and end ports: can pass this into AddConnection
 - A good idea that we could start with is we pass the ids of the two ports with AddConnection as a tuple (i.e. wireInfo is a tuple of port IDs). Then, gathering the port IDs from this message, BusWire can query Symbol for the coordinates of the ports (Let's call the function getCoordsOfPort portID which is string -> XYPos).

`DeleteConnection of wireId` (see end for description)

`MoveConnection of wireIdList * posX * posY` (see discussion and the end for descriptions)
- This may not necessarily be a list of wires...this is likely to be a list of wire segments although I am not sure how Aditya is handling wires. If he's making separate entities for each wire
segment then the message should carry a list of wire segment IDs. **NEED INPUT FROM ADITYA**

`HighlightConnection of wireIdList` (see the end for a description)
- However, this should be a list of wires since when we select a wire segment the whole wire would be highlighted.

To Symbol:
`MoveSymbol of symbolIdList * posX * posY` (see the end for a description)
`DeleteSymbol of symbolIdList` (see the end for a description)
`AddSymbol of symbolInfo` (see the end for a description)
`HighlightSymbol of symbolIdList`(see later discussion and the end for description)

## Bounding Box

**OPTION 1:**
Each buswire and symbol owns its bounding box. Bounding box can be fully defined using top left and bottom right corners. It should be slightly bigger than each component to allow for negation bubble.

Sheet is able to access bounding boxes of buswires and symbols
`BusWire.getBoundingBoxes mouseCoord`
`Symbol.getBoundingBoxes mouseCoord`
These functions will return the top-left and bottom-right corners of the boxes along with the ids of the related components (id: string * topleft: XYPos * bottomright:  XYPos)
 - Sheet has coordinates of mouse/mouse click
 - Sheet calls `getBoundingBoxes`, which returns a list of the bounding box of each component for that module
 - Sheet performs exhaustive search to find which bounding box(es) are selected - hopefully we make this more efficient.

Things to consider:
 - Optimisations: We pass in mouse coordinates to `getBoundingBoxes` for now, even though it may not be needed. We can delete later of course, although we may also be able to filter down the boxes based on mouseCoord.
 - Split up canvas into sections: Identify which section the coordinates are in and return bounding boxes in that section only. Consider: do we define this split in Sheet or in submodules?. Bounding boxes which overlap across sections - what do you do? **According to TC, we should focus on a non-optimised version first i.e. searching through all bounding boxes, before going for an optimisation. We don't know for sure whether performance will be slow at all.**

 - Maybe sort list by coordinate (proximity to bounding box) - binary search.
    - Might as well just return single bounding box instead of list?

**OPTION 2**
 - Send coords to BusWire and Symbol
 - BusWire and Symbol run through their list of components
 - Return list of components where mouse coords inside bounding box
 - Sheet sends highlight message to highlight components in those lists

*NOTE:* For the purpose of clarity, we will use **option 1** - we will be using the first method i.e. symbol and buswire would have functions that return bounding boxes as tuples of XYPos.


## Highlight
On mouse click down (including click and drag):
1. Sheet identifies which bounding boxes are inside selection
2. Sheet sends a `highlight` message to the selected component(s)
3. The selected component(s) are highlighted using an interface function

e.g. in Sheet we would send these messages:
`BusWire.Highlight WireIdList`
`Symbol.Highlight SymbolIdList`

*Option 1*: We only highlight when the mouse is in the bounding box with no separate message/interface function for unhighlighting. Once the mouse is out of range, we stop sending highlight messages and Symbol would know to stop rendering the previously highlighted symbols as highlighted.

 *Option 2*: Sending a message to highlight components as outlined above. Send an unhighlight message to symbol and buswire when we want to wipe everything clean - this would result in all the components being unhighlighted. When we send a highlight message, the relevant components would be set as selected - possibly through a property of symbol and buswire components. Unhighlight message would simply "unset" these properties.

 According to Darrick Lau, option 2 seems more viable
 **UPDATE: According to TC, option 1 seems better so it is the one that we are going to go with. We repeatedly send highlight messages to symbol/buswire as long as necessary. A component will be rendered highlighted as long as it is included in the highlight message from Sheet**


### Highlighting Ports
Ports should become visible when mouse is hovering in range
 - Highlight message (called `Symbol.highlightPort symbolId`) sent to Symbol when in range of the pertinent symbol even if not mouse click down.
 - These need to highlight ports but not symbol - the highlighting for port (turning black) is different from the highlighting for symbols (turning green)

### Selecting Ports
 - A port is selected if it is highlighted and then mouse click down
 - If we select a port then drag, Sheet draws a dashed line
 - If we drag to another port, Sheet sends message to BusWire to make wire between two ports

Selecting port: on mouse click down:
Call the function `Symbol.isPort mouseCoord: XYPos`
If we get `Some (portCoord: XYPos * portID: string)`, then begin drawing dotted line. If `None` don't draw - it may be another case e.g. drag select, selecting an actual component.

On mouse click up:
Call the function `Symbol.isPort mouseCoord: XYPos`
If we get `Some (portCoord: XYPos * portID: string)`, tell BusWire to draw new wire. If `None` don't draw.
In all cases stop rendering the dotted line.

`AddConnection of wireInfo`
 - WireInfo is likely a tuple of port IDs i.e. (startPort: string * endPort: string) - this is a good thing to start with as it is close to what we will finally do.
 - Called by Sheet on mouse click up if there is a second port
 - I will let BusWire people determine how they want to implement this
 - BusWire people can access Port IDs from Symbol, and then get Port coords


## Delete

Delete message from Sheet to BusWire and Symbol
 - If we delete a Symbol, does that delete the BusWires connected to that Symbol? YES. As discussed, sheet will have to send a delete message to the relevant wires first before deleting the symbol. This will imply some sort of calculation in sheet.
 - If we delete a BusWire, the Symbol should not be deleted


## Responsibilities of Sheet
 - Click and select multiple components on sheet
 - Click and highlight
 - Draw selection box
 - Change shape of cursor
 - Send message to each component covered by selection box to indicate which are highlighted


# BusWire

Has list of wires and list of symbols
 - BusWire model WILL have the symbols without wire connections so far, since it is likely that these symbols will be given wire connections in the future.

Wires should be selectable
 - Probably an extension?
 - Wires should do autorouting initially but should be selectable and moveable in the end product

## Interface Functions

`BusWire.getBoundingBoxes (mouseCoord: XYPos)`
 - Returns list of data structure containing IDs, and bounding box coords (id: string * topleft: XYPos* bottomright XYPos) of wires (initially all the wires but hopefully we can find a way to filter the number of wires sent down using mouseCoord). FOr now, just assume that MouseCord is included - if not needed we can remove later.

## Messages

`Delete of wireIdList`
 - Deletes wires from model based on Id.

`AddConnection of wireInfo`
 - **After meeting with TC, it's not apparent that we need to change anything about this. Wireinfo only needs to contain the port ID's. The buswire people can let us know if we are going to use the actual port structures instead in the wire info tuple**
 - Wire info (We would need the port IDs for sure - then based on these we could compare the widths of the two ports at either end of the connection - if they are unequal then error. Else if they are equal we know the width of the connection)
 - Look at how ISSIE does it?

`Highlight of wireIdList`
 - Highlights wires in list
- Look at how ISSIE does it? In Issie, only the ports are highlighted when you click on a wire, but I think it's ok if we change this by thinking of something better. We can certainly highlight the wire green or something (**after discussion with TC, we found it's ok to render the wires as green or any other colour we like...it just needs to work**)

`Move of wireIdList *  posX * posY`
 - Maybe this should be list of wire IDs
 - posX and posY represent a translation vector showing how the wires should be shifted, not new coords - this vector is calculated in sheet.
 - This allows for moving multiple wires in one function call

*Note*: We are not using highlight messages.

# Symbol

Has list of symbols

Each Symbol owns its ports
 - Ports need to keep track of how many wires are connected to those ports
 - Internal implementation details: each port could have its own bounding box
    - If used, this should be invisible to other modules

## Interface Functions

`Symbol.getPortCoords (portID: string)`
 - Used by BusWire for connecting wires to ports - it queries based on ID and gets an XYPos back.

`Symbol.getBoundingBoxes (mouseCoord: XYPos)`
 - Returns (id: string * topleft: XYPos * bottomright: XYPos) list. Initially this would be the bounding boxes for ALL symbols but hopefully we can filter it down with the help of mouseCoord. The hope is that mouseCoord is useful but if not we can dispense with it.

`Symbol.getPortType portID`
 - Returns if port is input or output

`Symbol.isPort xPos yPos`
 - Returns Option type indicating if mouse has clicked down on port
 - Option of (x, y) coord of port and portID i.e. Some (portCoord: XYPos * portID: string), or None

## Messages

`Move of symbolIdList *  posX * posY`
 - Maybe this should be list of symbol IDs
 - posX and posY represent a translation vector, not new coords - calculated in sheet.
     - This allows for moving multiple symbols in one function call

`Delete of symbolIdList`
 - Deletes symbols from model based on ID.

`Add of symbolInfo`
 - This still needs discussion
 - Symbol info would be a record with fields: "componentType", "numberInputs", "numberOutputs", "position"
 - Properties like "inputPortList", "outputPortList", "highlighted", "Id", "boundingBoxCoordinates" (this is XYPos * XYPos) can be set up and calculated by Symbol in addition to what is provided.
 - Coords (mouse click position)
 - Look at how ISSIE does it? Issie chooses an arbitrary random position to put the symbol. From here we can drag. This arbitrary initial position changes each time.

`Highlight of symbolIdList`
 - Highlights all symbols in list - property in the symbols will be set that renders them highlighted.
    - See discussion above
    - Look at how ISSIE does it?

`HighlightPorts of symbolId`
 - Highlights ports of symbols
 - Maybe need further discussion? **THIS PART SEEMS FINE**


# To Discuss With Dr Clarke 24/2/21:
- Can Sheet have direct access to Symbol? What complications could arise?
- About the bounding boxes optimisation that divides the canvas into sub-areas: do we define this split in Sheet or in submodules?. Bounding boxes which overlap across sections - what do you do?
- How to add a wire connection (what info about the wire do we pass) - how will we find out if it's a bus, and how will we find the bus width?
- Can we highlight wires green on selection as opposed to what is done in Issie (which is to merely highlight the port endpoints of the wire)
- Mechanism by which we highlight ad unhighlight components and ports - what messages do we send (we can mention the 2 options) - when do we highlight symbol ports?>
- Check method of using GitHub + repo name
- *Add anything else.....*

**Post meeting update:** The relevant areas have been updated based on the discussion with Dr Clarke.

**IMPORTANT**: Although we have specified portID and symbolID as strings everywhere, it may make sense for these ID's to actually have their own type for type safety...ISSIE actually defines all ID's as strings so I think TC may have overlooked this in making his suggestions to us about this. Maybe we could make a type for IDs but as we're coming close to the deadline we probably can let it be.

*Finality*: This interface is expected to be **the final one**. No changes are desired as everyone needs to get coding. However, if a change is desired, bring it up ASAP.
