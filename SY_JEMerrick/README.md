Joanna Merrick's Personal Project

Module: Symbol

Symbol Advisor  - Sacha Ayoun - Email not piazza

# DOCUMENTATION

## STATIC VARIABLES:
    - STD_HEIGHT - Standard Height to make Symbol from
    - HW_RATIO - Ratio of Height to Width for Symbol
    - RAD - Radius for inverters and port highlighting

## TYPES:
    
    - Portinfo
        * Port                        - From CommonTypes
        * NumWires                    - Keeps track of the number of wires connected to a port 
        * Name                        - Label to be printed for that port
        * Invert                      - A bool which = True if the port requires an inverter
        * width                       - A width indicating the wire/bus width of that port

    - Symbol
        * TopL                        - The top left coordinate of the symbol
        * BotR                        - The bottom right coordinate of the symbol
        * Id                          - The component ID associated with the symbol
        * Type                        - The component type associated with the symbol
        * Name                        - The label to be printed for the symbol
        * Highlight                   - A string indicating the colour the symbol should be (grey/blue/red)
        * PortHighlight               - A bool indicating which = True if the ports should be highlighted
        * PortMap                     - A map of <XYPos, Portinfo Option> which will be None if no port is assigned to a particular position, and Some if there is a port assigned to a position

## MESSAGES:
    - Move                            - Moves list of symbols by a translation vector XYPos
    - AddSymbol                       - Adds a new symbol to the model based on ComponentType, Position, Number of Inputs, Number of Outputs
    - DeleteSymbol                    - Removes a symbol from the model based on ComponentID
    - Highlight                       - Highlights symbols from a list of ComponentID, anything not in the list will be un-highlighted
    - HighlightPorts                  - Highlights the symbol ports of a single ComponentID 
    - HighlightError                  - Highlights symbols from a list of componentIds red, anything not in the list will be unhighlighted
    - Scale                           - Scales a symbol by an XYPos indicating the scale in the x and y directions respectively
    - Rotate                          - Rotates a symbol by an int indicating the number of degrees to rotate
    - UpdateSymbolModelWithComponent  - TODO

## INTERFACE FUNCTIONS:
    - getPortCoords                   - Inputs(model, portId), Returns the XYPos of the portID
    - getBoundingBoxes                - Returns a list of bounding boxes in the form (ComponentID, TopLeftCoordinate, BotRightCoordinate)
    - getPortType                     - Takes a model and portID, Returns whether a port is input/output
    - isPort                          - Takes a model, XYpos and returns Some(XYPos, PortID) if the position was on a port, or None otherwise
    - getPortIds                      - Inputs(model, symbolID), returns list of portIDs
    
    - As requested by Aditya for wire module:
    * getPortWidth                    - Inputs(model, portId), returns the width of that port
    * getHostId                       - Inputs(model, portId), returns the componentId associated with that port



## CHANGELOG (For me)

### 24/02/21
   
    COMPLETE: 
        Init symbol type
        Init ports
        Create rectangle
        Print I/o labels
        Print symbol type

    ADJUSTMENTS NEEDED: 
        Port labels fully inside the box - Probably involves making a seperate function that takes in string as param
        

    NEXT TASKS:
        Print inverters
        Interface funcs
        Update movement from demo listen stuff to with messages
        Check all types done

### 25/02/21

    COMPLETE:
        Print inverters
        INTERFACE FUNCS:
            Symbol.Getportcoords
            Symbol.Getboundingboxes
            Symbol.Getporttype
            Symbol.Isport
        MESSAGES:
            Addsymbol
            Deletesymbol
            Startdragging/dragging/enddragging - not move right now
            Highlight
            Highlightports
        Make pretty symbols
        Make inverters print on the outside of the box rather than the middle
        Make labels print on the outside of the box rather than the middle


    ADJUSTMENTS NEEDED:
        
        Make code prettier
    
    NEXT TASKS:
        Write up code specification explaining functions
        (group) talk about move message - im not 100% sure it will work as currently specified
        Ports - I dont think xy-position should necessarily be saved ? 
        Make ports manually adjustable
        Manual rotation
        Manual scaling

### 26/02/21
    
    COMPLETE:
        Specification writeup
        
        Manually adjustable ports & data encapsulation:
            Symbol now has a list of portmap - possible positions a port may lie in for a given symbol based on height/width/number inputs/outputs.
            Portinfo no longer has direct access to its position, now has a int 'slotpos' which indexes into a symbol's portmap to give position.
            Updated previous messages and interface functions to work with this new architecture
            Portinfo - bounding box has been removed, this functionality has been fully replaced by a helper function 'testbox' which takes a port position and xypos and returns a bool = true if xypos is in the bounding box of the port
            Rotation - currently only accessable in the constructor of symbol as no message has yet been made in sheet
            Scaling - currently only accessable in the constructor of symbol as no message has yet been made in sheet
        
        Added buswidth

        INTERFACE FUNCS:
            Getportcoords fix - previously never found port, has been fixed
            GetPortIds added
        
        TEST FILE:
            A test file has been made which contains a simplified version of the Symbol and CommonTypes types, and the interface functions to test whether they work properly.
            Currently no known issues.
        
        MESSAGES:
            Rotate added - Currently not used by sheet but a logical implementation has been completed
            Scale added - Currently not used by sheet but a logical implementation has been completed
            Move added
            Dragging, IsDragging, StopDragging - All removed
    
    ADJUSTMENTS NEEDED:
        TIDY CODE - 
            Messages: DragPort is messy and long - maybe break it up a bit into smaller funcs?
                Interface funcs: getPortCoords is has repetitive searches, can it be simplified?

    NEXT TASKS:
        IMPORTANT - Wire width stuff and Wire merge symbols need to be done.
        Write something to show port movement
        Interface to ISSIE
        
### 27/02/21
    
    COMPLETE:

        COMMON TYPES - Added PortId type.

        INTERFACE FUNCS -
            Changed all string input/outputs to relevant ComponentId or PortId types.
        
        SYMBOL -
            Added a new attribute 'SymbolType' which has generic(ish) categories for different component types to fit into
            Fixed a bug where if the inputs < ouputs, some outputs would be drawn on the left instead of the right - this has been resolved.

        PORTS -
            Added a new type 'genericPort' which determines the type of label and position that a port will be assigned based on the categories: IO/Select/Carry/Enable

        DRAWING - 
            Added most extra shapes (Inputs, Outputs, Mux, MergeWire, Splitwire, Adder )
    
    ADJUSTMENTS NEEDED:
        Ports/shape of Input/Output symbols are not quite right
        Drawing function is very long and needs splitting up

    NEXT TASKS:


### 28/02/21

    COMPLETE:
        
        PortList, and slot have been removed and PortMap is now a map of <XYPos, Option Portinfo>
        All messages, interface funcs, etc have been updated to reflect this
        Symbols width is now more accurately created such that the ports on the top/bottom are centred
        Fixed bugs in the interface funcs where the wrong helper func was being called.

