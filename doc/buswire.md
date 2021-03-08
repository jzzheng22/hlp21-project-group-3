# Interface Documentation for BusWire Module

Owners:
 - Aditya Deshpande
 - Gautham Ravichandran

Any changes must be agreed by owners then communicated to the rest of the group.

Routing algorithm

Has list of BusWires and list of Symbols

Wires should be selectable: should do autorouting initially but be selectable and moveable in end product

Might want to add in portID and symbolID and wireID as types instead of strings for type safety.




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

`Move of wireId * Index: index *  XYPos list`
 - XYPos is translation vector
 - return wire Id, segment Index and translation vector

## Interface functions

`getBoundingBoxes (wModel: Model) (mouseCoord: XYPos): (int * CommonTypes.ConnectionId * XYPos * XYPos) list`
 - Returns all bounding boxes for all wire segments along with coressponding with Wire ID and Segment Index for Sheet to deal with.

`getWireIdsFromPortIds (wModel: Model) (portIds: CommonTypes.PortId list) : CommonTypes.ConnectionId list`
 - Takes a list of PortIds as input and returns the IDs of all the wires connected to the supplied ports.


## BusWire needs to be able to receive these:
 - Add connections (from Sheet and ISSIE)
 - Delete connections (from Sheet)
 - Move connection segment (from Sheet) -> need this to return the WireID aswell as Segment Index
 - Highlight connections (from Sheet and ISSIE)
 - Init canvas (from ISSIE)

 ## Stretch Goals