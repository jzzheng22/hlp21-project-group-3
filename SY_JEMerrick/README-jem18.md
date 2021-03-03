# Joanna Merrick (jem18 - CID: 01504728)
(Member of group 3)

## An Overview of the Code

The module implemented here is **Symbol**

- This code uses Aaman's Sheet, and Aditya's Buswire implementations as stubs.
- The renderer.fs has also been modified to fit with these modules and add some features specific to symbol such as 'Rotation' and 'Scaling'

The code may be run by entering the following from within this directory
```
npm run dev
```


## Demo outline

1. Different symbols
    * I will have symbols of each general category created in the init for the demo, so will be displayed on startup.
2. Symbol selection (& Mouse events can select symbols): Highlight message and getBoundingBoxes interface function
    * Click/drag symbol -> Symbol will be selected/highlighted
3. Port highlighting: Port highlight message
    * Move cursor near symbol -> Ports will be highlighted
4. Moving symbols: Move message
    * Click/drag symbol -> Symbol will move with the cursor
5. Rotating symbols: Rotate message
    * Select a symbol -> Press alt+shift+R to show rotation functionality works
6. Scaling symbols: Scale message
    * Select a symbol -> Press alt+shift+U to scale up by 1.25, alt+shift+D to scale down by 0.85 in both x and y directions, to show scale functionality works.
7. Adding symbols: Add message
    * Press the add option from the dropdown box -> an AND gate will be added to the schematic
8. Deleting symbols: Delete message
    * Select a symbol -> Press delete, symbol will be removed.
9. Showing interfaces work (getBoundingBoxes already shown in previous steps)
    * getPortWidth -> Used by buswire: I will show that wires will not be made between ports of different widths
    * isPort -> Used by sheet to determine if it has clicked on a port. Demonstrated with a dashed line when dragging from a port.
    * getPortCoords -> Used by sheet and buswire to create wires: I will show wires being created to demonstrate the function correctness
    * getPortType -> Used by buswire to prevent input-input and output-output connections, I will show the wire not being created in these cases
    * getPortIds -> Used by sheet to delete wires connected to a symbol when a symbol is deleted. I will show this in operation.
    * getPortEdge -> Used by buswire to determine wire routing correctly, I will show wires being connected from different ports to show function correctness
10. HighlightError message
    * I will show the code/message which will highlight the symbol in red when there is an error
11. DragPort message - Move ports manually
    * I will show the code/message that I have written that will allow a selected port to be moved to another location

