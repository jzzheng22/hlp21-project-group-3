# Interface Documentation for BusWire Module

Owners:
 - Aditya Deshpande
 - Gautham Ravichandran

Any changes must be agreed by owners then communicated to the rest of the group.

Routing algorithm

Has list of BusWires and list of Symbols

Wires should be selectable: should do autorouting initially but be selectable and moveable in end product

Might want to add in portID and symbolID and wireID as types instead of strings for type safety.


## Interface Functions

`BusWire.getBoundingBoxes (mouseCoord: XYPos) (model : Model)`
 - Returns list containing IDs, and bounding box coords of wires
 - `(id: string * topleft: XYPos * bottomright: XYPos)`
 - Initially just return all wires

 - Returns a flat list
   - Each wire has one ID but multiple bounding boxes 
   - A single WireID may appear multiple times accompanying a different bounding box of the wire

    string  | XYPos  | XYPos  |   
    |---------|--------|--------|
    | WireID 1 | TopL 1A | BotR 1A |   
    | WireID 1 | TopL 1B | BotR 1B |   
    | WireID 2 | Top L2  | Bot R2  |   
    | WireID 3 | Top L3  | Bot R3

## Messages

### <b> BusWire Receives from Sheet</b>

Note: `CommonTypes.ConnectionId` is a Wire ID

`DeleteWires of CommonTypes.ConnectionId list`
 - Deletes wires from model based on ID.

`AddWire of (CommonTypes.PortId * CommonTypes.PortId)`
 - Create connection between two ports, whose IDs are supplied in the message
 - The order of ports does not matter, as long as one of the ports is an input and the other is an output
 - BusWire's handling of this message incorporates wire validation, protecting against:
   - Port type mismatch (trying to connect input to input, output to output)
   - Wire duplication (trying to draw a wire that already exists)
   - Port width mismatch (target port has different width to source port)
   - <i> These rules are fairly easy to add so talk to Aditya if you want more </i>
 

`HighlightWires of CommonTypes.ConnectionId list`
 - Highlights wires specified in the list
   - Highlights the source and target ports of the wire
 - Same functionality as highlighting a Symbol (need to send a Highlight message with an empty list to unhighlight everything)

`MoveWires of wireIdList *  XYPos`
 - XYPos is translation vector
 - Not implemented yet, will do when incorporating manual routing

## Interface functions

`getBoundingBoxes (wModel: Model) (mouseCoord: XYPos): (CommonTypes.ConnectionId * XYPos * XYPos) list`
 - Returns all bounding boxes for all wire segments present in the wire model for Sheet to deal with.

`getWireIdsFromPortIds (wModel: Model) (portIds: CommonTypes.PortId list) : CommonTypes.ConnectionId list`
 - Takes a list of PortIds as input and returns the IDs of all the wires connected to the supplied ports.


## BusWire needs to be able to receive these:
 - Add connections (from Sheet and ISSIE)
 - Delete connections (from Sheet)
 - Move connection segment (from Sheet) -> WILL HAPPEN WHEN MANUAL ROUTING IS CONFIGURED
 - Highlight connections (from Sheet and ISSIE)
 - Init canvas (from ISSIE)

 ## Stretch Goals