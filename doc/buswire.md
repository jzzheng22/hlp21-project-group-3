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

## Messages

`Delete of wireIdList`
 - Deletes wires from model based on ID.

`AddConnection of wireInfo`
 - Create connection between two ports
 - Need to check widths of both ports to see if they are equal/unequal
 - Look at how ISSIE does it?

`Highlight of wireIdList`
 - Highlights wires in list
 - Look at how ISSIE does it? 
 - In Issie, only the ports are highlighted when you click on a wire

`Move of wireIdList *  XYPos`
 - XYPos is translation vector

## BusWire needs to be able to receive these:
 - Add connections (from Sheet and ISSIE)
 - Delete connections (from Sheet)
 - Move connection segment (from Sheet)
 - Highlight connections (from Sheet and ISSIE)
 - Init canvas (from ISSIE)

 ## Stretch Goals
