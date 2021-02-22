sheet is first point of contact for mouse events on canvas

sheet top level of data model
has data structure of symbols and buswires
sheet has buswire model
buswire model has list of wires and list of symbols

responsibilities of sheet:
    click and select multiple components on sheet
    click and highlight
    draw selection box
    change shape of cursor
    send message to each components that is covered by selecitno box to indicate which are highlighted
    wires should be selectable? - probably an extension? wires shoudl do autorouting initially but i think students should be able to move them around

getting started:
    deciding data model: in skeleton code: each module has its own model (record)
        how is sheet going to own the other two: this affects how we pass messages
        do we want wires to be selectable
        how to pass messages
        sheet needs to send messages correctly      

interfaces:
    sending 


Coordinates of click
find if coordinates are in bounding box of wire/symbol


Sheet
    Some data structure that contains every wire and symbol on the canvas

    Each wire/symbol 'owns' its bounding box
    on a mouse click:
        we have coordinate of click
        then we can use that to identify the bounding box its in (if there is one)
            Issue: this would be slow


        Wires and symbols:
            have a function that takes in coords
            identifies if click was in bounding box
            if so, returns true 

        In sheet: filter responses, if true add to list or some other structure
            Then identify which one we need to highlight

        Then send highlight message to relevant component

    When mouse hovering within range:
        Send message to symbol to highlight ports

Sheet needs a message that says when mouse leaves area



in Symbol:
    getPortCoords (used by BusWire)


Each component has bounding box
    Each component has ports
    Each port can have bounding box

In Sheet
    click and drag on port of symbol
    when within bounding box of port of another symbol
        change buswire highlighting
    release mouse: connect the ports

Highlighting:
    Sheet tells Symbol to highlight
    Symbol does the highlighting

Selecting port and dragging to another port:
    Ports are owned by Symbol
        Symbol tells Sheet if it's a port
            Sheet can draw dashed line
    Sheet tells BusWire to make wire between two ports

Ports need to keep track of how many wires are connected

Each component should keep track of its bounding box properties
    Get component ids that are selected because we have all the bounding boxes
        Send messages to those components

    Sheet will pass in coordinates and others will return bounding box (top left and bottom right corner?)

Delete
    Send from sheet to symbol + buswire

Sheet.highlightConnections
    symbol.highlight


Symbol
    symbol.highlight(