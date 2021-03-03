# Interface Documentation for BusWire Module

Owners:
 - Aditya Deshpande
 - Gautham Ravichandran

Any changes must be agreed by owners then communicated to the rest of the group.

Routing algorithm.

Has list of BusWires and list of Symbols.

Wires should be selectable: should do autorouting initially but be selectable and moveable in end product.

## Messages

**From Sheet**

`AddWire of (CommonTypes.PortId * CommonTypes.PortId)`
 - Creates connection between two ports specified in message.
 - Port types cannot be the same (i.e. one must be an input, one must be an output - order does not matter)
 - BusWire's handling of this message incorporates wire validation, protecting against:
   - Port type mismatch (trying to connect input to input, output to output)
   - Wire duplication (trying to draw a wire that already exists)
   - Port width mismatch (target port has different width to source port)
 
`DeleteWires of CommonTypes.ConnectionId list`
 - Deletes wires from model based on ID.

`HighlightWires of CommonTypes.ConnectionId list`
 - Highlights wires specified in the list.
    - Highlights the source and target ports of the wire.
 - Same functionality as highlighting a Symbol (need to send a Highlight message with an empty list to unhighlight everything).

`MoveWires of wireIdList *  XYPos`
 - XYPos is translation vector.
 - Not implemented yet, will do when incorporating manual routing.

## Interface Functions (called by Sheet)

`BusWire.getBoundingBoxes (mouseCoord: XYPos) (model : Model)`
 - Returns list of `(id: CommonTypes.ComponentId * topLeft: XYPos * bottomRight: XYPos)`
 - Initially just returns all bounding boxes.

 - Returns a flat list:
   - Each wire has one ID but multiple bounding boxes 
   - A single WireID may appear multiple times accompanying a different bounding box of the wire.

    | `CommonTypes.ComponentId` |` XYPos` | `XYPos` |   
    |----------|---------|---------|
    | WireID 1 | TopL 1A | BotR 1A |   
    | WireID 1 | TopL 1B | BotR 1B |   
    | WireID 2 | Top L2  | Bot R2  |   
    | WireID 3 | Top L3  | Bot R3  |

`BusWire.getWireIdsFromPortIds (wModel: Model) (portIds: CommonTypes.PortId list) : CommonTypes.ConnectionId list`
 - Takes a list of PortIds as input and returns the IDs of all the wires connected to the supplied ports.

## Interface Functions (called by BusWire)

`Symbol.getPortCoords (symbolModel : Model) (portID: CommonTypes.PortId)`
 - Used by BusWire for connecting wires to ports.
 - Returns `XYPos`.

`Symbol.getPortType (symbolModel : Model) (portID: CommonTypes.PortId)`
 - Returns if port is input or output.

*Optional interface functions:*

These are called by BusWire and implemented in JEMerrick's Symbol:
`Symbol.getPortWidth (model : Model) (pId : CommonTypes.PortId) : int`
 - Returns width of specified port.

`Symbol.getHostId (model : Model) (pId : CommonTypes.PortId) : CommonTypes.ComponentId`
 - Returns the ComponentID which the PortID belongs to.

## BusWire needs to be able to receive these:
 - Add connections (from Sheet and ISSIE)
 - Delete connections (from Sheet)
 - Move connection segment (from Sheet) -> WILL HAPPEN WHEN MANUAL ROUTING IS CONFIGURED
 - Highlight connections (from Sheet and ISSIE)
 - Init canvas (from ISSIE)

 ## Stretch Goals
