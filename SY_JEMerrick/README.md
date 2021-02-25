Joanna Merrick's Personal Project

Module: Symbol

Symbol Advisor  - Sacha Ayoun - Email not piazza

DOCUMENTATION

STATIC VARIABLES:
    STD_HEIGHT - Standard Height to make Symbol from
    HW_RATIO - Ratio of Height to Width for Symbol
    RAD - Radius for inverters and port highlighting

TYPES:
    Portinfo
        Position                    - Location of the port on the canvas
        Port                        - From CommonTypes
        NumWires                    - Keeps track of the number of wires connected to a port 
        Name                        - Label to be printed for that port
        Invert                      - A bool which = True if the port requires an inverter
        Box                         - The bounding box for the port 

    Symbol
        TopL                        - The top left coordinate of the symbol
        BotR                        - The bottom right coordinate of the symbol
        LastDragPos                 - The position where dragging last occured (DO WE NEED THIS ?)
        IsDragging                  - A bool which = True if dragging 
        Id                          - The component ID associated with the symbol
        Ports                       - A List of Portinfo ports associated with the symbol
        Type                        - The component type associated with the symbol
        Name                        - The label to be prtined for the symbol
        Highlight                   - A bool indicating which = True if the symbol should be highlighted
        PortHighlight               - A bool indicating which = True if the ports should be highlighted


MESSAGES:
    MouseMsg                        - ALTERATIONS TO COME (MOVE)
    StartDragging                   - ALTERATIONS TO COME (MOVE)
    Dragging                        - ALTERATIONS TO COME (MOVE)
    EndDragging                     - ALTERATIONS TO COME (MOVE)
    AddSymbol                       - Adds a new symbol to the model based on ComponentType, Position, Number of Inputs, Number of Outputs
    DeleteSymbol                    - Removes a symbol from the model based on ComponentID
    Highlight                       - Highlights symbols from a list of ComponentID, anything not in the list will be un-highlighted
    HighlightPorts                  - Highlights the symbol ports of a single ComponentID 
    UpdateSymbolModelWithComponent  - TODO


INTERFACE FUNCTIONS:
    getPortCoords                   - Inputs(model, portId), Returns the XYPos of the portID
    getBoundingBoxes                - Returns a list of bounding boxes in the form (ComponentID, TopLeftCoordinate, BotRightCoordinate)
    getPortType                     - Takes a model and portID, Returns whether a port is input/output
    isPort                          - Takes a model, XYpos and returns Some(XYPos, PortID) if the position was on a port, or None otherwise

INTERFACE TO ISSIE
    TODO



OBJECTIVES TIMELINE (FOR ME)

24/02/21
   
    COMPLETE: 
        INIT SYMBOL TYPE
        INIT PORTS
        CREATE RECTANGLE
        PRINT I/O LABELS
        PRINT SYMBOL TYPE

    ADJUSTMENTS NEEDED: 
        PORT LABELS FULLY INSIDE THE BOX - Probably involves making a seperate function that takes in string as param
        

    NEXT TASKS:
        PRINT INVERTERS
        INTERFACE FUNCS
        UPDATE MOVEMENT FROM DEMO LISTEN STUFF TO WITH MESSAGES
        CHECK ALL TYPES DONE

25/02/21

    COMPLETE:
        PRINT INVERTERS
        INTERFACE FUNCS:
            SYMBOL.GETPORTCOORDS
            SYMBOL.GETBOUNDINGBOXES
            SYMBOL.GETPORTTYPE
            SYMBOL.ISPORT
        MESSAGES:
            ADDSYMBOL
            DELETESYMBOL
            STARTDRAGGING/DRAGGING/ENDDRAGGING - not move right now
            HIGHLIGHT
            HIGHLIGHTPORTS
        MAKE PRETTY SYMBOLS
        MAKE INVERTERS PRINT ON THE OUTSIDE OF THE BOX RATHER THAN THE MIDDLE
        MAKE LABELS PRINT ON THE OUTSIDE OF THE BOX RATHER THAN THE MIDDLE


    ADJUSTMENTS NEEDED:
        
        MAKE CODE PRETTIER
    
    NEXT TASKS:
        WRITE UP CODE SPECIFICATION EXPLAINING FUNCTIONS
        (GROUP) TALK ABOUT MOVE MESSAGE - IM NOT 100% SURE IT WILL WORK AS CURRENTLY SPECIFIED
        PORTS - I DONT THINK XY-POSITION SHOULD NECESSARILY BE SAVED ? 
        MAKE PORTS MANUALLY ADJUSTABLE
        MANUAL ROTATION
        MANUAL SCALING