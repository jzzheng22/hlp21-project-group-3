Joanna Merrick's Personal Project

Module: Symbol

Symbol Advisor  - Sacha Ayoun - Email not piazza

TODO: Convert to FP style (?)

OOP Style Symbol class 
Symbol:
    id
    Labels
        Input
            id
            name
            position (?)
        Output
            id
            name
            position (?)
        Name

    TopLeft
    BottomRight



Interface for symbol:



Notes for me:
Grading goals/order of operation:
    
    BASIC IMPLEMENTATION REQUIRED:

    1. Identify/Abstract common parts

        a. Generic rectangle block

           DrawRect(height, width, starting_point)
                Draws a rectangle of height x width where starting_point is a point on the box - TODO: DECIDE THIS

                Thoughts:
                    Height/width would have to be determined outside the function based on logic/gate type
                    Create the bounding box?

        b. Block title

            WriteName(name)
                Write the block name in the middle centre of the symbol

                Thoughts:
                    Should it be moveable?
                    Should it be nameable?
                    
        c. Input/output Inverter

            DrawInvert(id, listLabels) =
                Takes the id of the port that requires an inverter 
                Search for which sublist id is in listLabels
                Determines which side to draw the circle on
                Draws a circle on that port

                Thoughts:
                    Ratio of box to inverter size (?)
                        Radius = CIRCLE_TO_RECT_RATIO * height * width
                    Need to update bounding box

            i. UpdateBound(radius, object_id) = 
                object_id.
                

        d. Port labels
            
            List of ports for each side in order: [[left], [top], [right], [bottom]]
            e.g. AND:
                listLabels = [[Input; Input]; []; [Output]; []]
            
            DrawLabels(listLabels) = 
                For each sublist print the labels in the relavent orientation

            Thoughts:
                I think this can be abstracted further but I'm not sure right now
    
    3. Implement the symbols listed in common types using the abstract functions made

        a. Generic logic block:

            MakeBlock(inputs, outputs, start_pos) =
                h = STANDARD_HEIGHT * max(inputs, outputs)
                w = HEIGHT_WIDTH_RATIO * h
                drawRect(h, w, start_pos)
                drawLabels([inputs]; []; [outputs]; [])

        b. Call inverter for specific ports as per ComponentType

    4. Messages
        a. What messages does Symbol get sent/send ? Discuss this with team - dont need this to write the symbol definitions though
        b. Bounding box - where the wire should not pass, should this include/disinclude the inverter circle?

    5. Implementing custom type

    6. Update function
        List of objects (id), and position


    EXTRA

    - TODO: Come back to this later
    
    6. Port movement 
        Current thoughts:

        Find selected label in the label list, move to location

        Restrictions (?)
            1. Do we need any restrictions?
            2. Cannot have inputs and outputs on the same side
            3. Max number of labels on one side = max(inputs, outputs)
            4. Would we have to work out number of labels allowed based on the symbol that has been made
                a. I.e. we cannot fit 3 labels on the bottom of an AND gate

        I think I need to talk to sheet guys about this, as they would be determining the mouse clicks and other things
        This might be simpler for me

    4. Resizing

    5. Drag

    6. Select




Pseduo functions:

type Symbol =
    {
        Pos: XYPos
        PosBot: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        Ports : Portinfo list

    }

PortInfo =
    {
        Pos: XYPos
        Port: CommonTypes.Port
        Orientation: int
        Placement: int 
        NumWires: int
    }

PortToPortInfo(i, Port, type, TopL, BotR, n) = 
    {   
        //Left, Top, Right, Bot
        match type with
        | 0 -> { X = TopL.X; Y = ((TopL.Y - BotR.Y) * i / n) }
        | 2 -> { X = BotR.X; Y = ((TopL.Y - BotR.Y) * i / n) }
        | _ -> failwithf "shouldnt happen"
        Port = x
        Orientation = 0
        Placement = i
        NumWires = 0
    }
    
CreateNewComponent(Pos, Component) =
    
    TopL = Pos
    n = max(inputs.size, outputs.size)
    Height = STD_HEIGHT * n
    Width = STD_HW_RATIO * Height
    BotR = TopL with {X -= Height; Y += Width}
    Inputs = Inputs 
            |> Enumerate 
            |> Map(fun (i, x) -> 
            |> snd
    BoundingBox = (TopL, BotR)

Depreciated functions I may need in future

///Returns an int indicating the side of the object that a port is on 0 = Left, 1 = Top, 2 = Right, 3 = Bottom
let findSide id listLabels = listLabels |> List.findIndex(List.contains id)

///Returns a tuple (side, index) indicating the index/position that the port lies in
let findPos id listLabels = 
    let i = listLabels |> findSide id 
    let j = listLabels |> List.item i |> List.findIndex((=) id)
    (i, j)

///Returns a react element for an inverter on a specific port
let drawInvert id listLabels =
    //index of outer position tells us whether port is left/top/right/bottom
    let side = findSide id listLabels
    let indx = findPos id listLabels
    //let pos = listLabels.[side].[indx]
    0


OBJECTIVES TIMELINE 

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


    ADJUSTMENTS NEEDED:
        MAKE INVERTERS PRINT ON THE OUTSIDE OF THE BOX RATHER THAN THE MIDDLE
        MAKE LABELS PRINT ON THE OUTSIDE OF THE BOX RATHER THAN THE MIDDLE
        MAKE PRETTY SYMBOLS
        MAKE CODE PRETTIER
    
    NEXT TASKS:
        WRITE MESSAGE STUFF
        TRANSFER PORTS FROM LIST TO MAP??