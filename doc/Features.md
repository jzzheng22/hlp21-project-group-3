# Features
--------

## Symbols:

### Adding Symbols
- Newly added symbol follows mouse movement and positions itself where the mouse first clicks. *Automatic snapping to grid*.
- Each new symbol *comes with its own unique name label*.

### Selecting Symbols
- May be done by: 
    - Clicking on a symbol 
    - Dragging a box over multiple symbols.

### Deleting Selected Symbols
- May be done via drop-down menu or by pressing the 'delete' key.
- Deleting a symbol deletes all connected wires and wire error messages.

### Moving Selected Symbols
- May be for a single symbol or for a group. Done by dragging a mouse (moving with left button down) so that selected symbols follow. *Automatic snapping to grid*.

### Transforming Symbols
- **Rotation** - Can be done for selected symbol via drop-down menu or via shortcuts: Ctrl-E for clockwise and Ctrl-Q for anticlockwise rotation. Wire routing automatically adjusts.
- **Scaling** - Interactive scaling by dragging from corner of selected/non-selected symbol. Selected symbol(s) can also be increased/decreased in side length by 1 grid square length at a time via drop down menu or shortcuts: ALt-M (magnify), ALt-D (diminish).
- *All transformed symbols automatically snap to grid*. 
- Ports do not automatically snap to grid after a scaling transformation, and require some user input to align correctly.
- Shrinking below a side length of one grid square length is not allowed.

### Aligning Selected Symbol Groups
- May be done via drop-down menu or shortcuts: *Alt-X* for alignment along X-direction (horizontal); *Alt-Y* along Y-direction (vertical). Horizontally aligned symbols cannot be vertically aligned and vice versa.

### Moving of ports and labels
- Ports may be moved to other positions on the periphery of the symbol as per user convenience. This is done by holding down the right mouse button at the location of the port and dragging in any direction.
- The set of postions where the port can be moved will highlight in purple. Drag the mouse towards one of these locations. Upon mouse release the selected port will be moved (and possibly swapped with) the closest point to the mouse release position. 

## Wires

### Adding Wires
- Dragging a dotted line between symbol ports creates a new wire. User friendly UI highlights port positions and possible connections.
- **Width inference** indicates an error by highlighting if input and output port widths are incompatible.

### Selecting Wire
- Done by clicking or dragging selection box. For two selected symbols, all wires joing them are also selected.

### Deleting selected Wire(s)
- Done by pressing 'delete' or by using drop down menu

### Moving Wire Segments
- Drag the mouse on a section of wire and the selected wire segment will follow.

### Routing
- Initial routing: automatic routing with symbol avoidance.
- Routing can be manually adjusted by moving wire segments that aren't connected to any symbol port.
- More segments can be added by right clicking on a wire - three additional segments will appear at the click position.

### Adding Segments to Wires
- When a user right-clicks on top of a wire, three new wire segments are generated in the wire at the click position. This allows the user to have more flexibility in manual routing.

## Overall UI

### Zooming canvas
- Via drop down menu or shortcuts: Ctrl-Z (zoom in) and Ctrl-Y (zoom out). Zooming is in steps of 1.25.

### Toggling Selection of Symbols
- On holding down control, the app enters a "toggling" state. In this state, any unselected element that is selected (via click or drag) will become selected and highlighted, while selected elements will unselect and unhighlight. This is a feature similar to Windows OS.
- This does not conflict with keyboard shortcuts.

### Error Messages and Highlighting
- When the user attempts to drag a wire between two ports of different width, the illegal connection and the two symbols on eitherr end are highlighted red.
- A popup appears in the topleft corner of the screen explaining the cause of the error.
- Both of the above changes disappear when the illegal connection/one or both of the connected symbols is deleted.

### Copy-Paste
- First highlight the symbol(s) (and wire(s)), using the various selection options (drag, holding ctrl and clicking, clicking on a single symbol)
- Press Ctrl+C to copy
- Click on the canvas where you want to paste
- To stop pasting press `esc`
