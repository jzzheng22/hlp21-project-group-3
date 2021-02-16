Joanna Merrick's Personal Project

Module: Symbol

Symbol Advisor  - Sacha Ayoun - Email not piazza

Symbol:
    id
    Labels
        Input
            id
        Output
            id
        Name
            id



Interface for symbol:



Notes for me:
Grading goals/order of operation:
    
    BASIC IMPLEMENTATION REQUIRED:

    1. Code explicitly some basic symbols:
        a. Logic gate (e.g. AND)
        b. Blocky component (e.g. Flip-flop)

    2. Identify/Abstract common parts

        Current thoughts: 

        a. Generic rectangle block
           DrawRect(height, width, starting_point)
                Draws a rectangle of height x width where starting_point is a point on the box - TODO: DECIDE THIS

                Thoughts:
                    Height/width would have to be determined outside the function based on logic/gate type
                    Create the bounding box? - I think this is unnecessary, the initial bounding box will be = rectangle size
            i

        b. Block title
            WriteName(name)
                Write the block name in the top centre

                Thoughts:
                    Should it be moveable?
                    Should it be nameable?
                    If we allow for movement of inputs/outputs, maybe the name of the component should go in the centre rather than top

        c. Input/output Inverter
            DrawInvert(id, listLabels) =
                Takes the id of the port that requires an inverter (e.g. NAND would be output port)
                Draws a circle on that port
                Search for which sublist id is in listLabels
                Determines which side to draw the circle on


                Thoughts:
                    Ratio of box to inverter size (?)
                        Radius = CIRCLE_TO_RECT_RATIO * height * width
                    Need to update bounding box

            i. UpdateBound(radius, object_id)
                

        d. Port labels
            
            I think if we have a list for the ports on each side so like
            [[left], [top], [right], [bottom]]
            So and gate would be like this:
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
