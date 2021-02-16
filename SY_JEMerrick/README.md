Joanna Merrick's Personal Project

Module: Symbol

Symbol Advisor  - Sacha Ayoun - Email not piazza


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
           DrawRect(symbolType) 
                Infers size from type
                All logic gates type would be some specific size

           OR:
           Think this version is better (?)

           DrawRect(height, width)
                Draws a rectangle of height x width

                Thoughts:
                    Size would have to be determined outside the function

        b. Block title
            WriteName(name)
                Write the block name in the top centre

                Thoughts:
                    Should it be moveable(?)
                    If we allow for movement of inputs/outputs, maybe the name of the component should go in the centre rather than top

        c. Input/output Inverter
            DrawInvert(id)
                Takes the id of the port that requires an inverter (e.g. NAND would be output port)
                Draws a circle on that port

                Thoughts:
                    Need to determine whether on LHS or RHS of the block so that the circle doesn't overlap with rectangle
                    TODO: Not sure how to determine that right now

        d. Port labels
            DrawLabels(labels, position)
            Writes the labels in the position e.g.
            DrawLabels([IN1; IN2], Left) 
            They would always be drawn vertically descending from top to bottom

            Thoughts:
                Should we have a seperate function for input/output, e.g. DrawInputLabels that always draws on the left rather than having to specify in a parameter?
                Would it be useful to include an orientation parameter so that it could be drawn at any side?
                    e.g. DrawLabels([IN1; IN2], Left, Vertical)
                    Or this could be inferred, Left/right will always be vertical, but Top/bottom will always be horizontal

                I think this can be abstracted further but I'm not sure right now
    
    3. Implement the symbols listed in common types using the abstract functions made
        a. First do the AND and Flip-flop to make sure the abstraction functions work
        b. Logic gates
        c. Mux
        d. Adder

    4. Messages
        a. What messages does Symbol get sent/send ? Discuss this with team - dont need this to write the symbol definitions though
        b. Bounding box - where the wire should not pass

    5. Implementing custom type

    EXTRA

    - TODO: Come back to this later
    
    6. Port movement 
        Current thoughts:

        I think if we have a list for the ports on each side so like
        [[left], [top], [right], [bottom]]
        So and gate would be like this (standard):
        [[IN1; IN2], [], [OUT1], []]

        Then we could quite easily determine which port/port label to move
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
