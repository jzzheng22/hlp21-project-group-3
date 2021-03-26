# Features
--------

## Symbols:

### Adding Symbols
- Newly added symbol follows mouse movement and positions itself where the mouse first clicks.  
- On clicking, the symbol automatically snaps to grid.
- Each new symbol comes with its own unique name label.
- Custom components are possible.
- Clocked symbols have the clock icon (wires cannot be connected to clocks).

### Selecting Symbols
- May be done by: 
    - Clicking on a symbol.
    - Dragging a box over multiple symbols.
    - Holding `Ctrl` and then mouse clicking/dragging to select multiple
- Upon selecting a symbol, red circles will appear at the corners to show the user where to drag from to scale the symbol.
- When clicking on overlapping symbols, only the top symbol will be selected.

### Deleting Selected Symbols
- May be done via drop-down menu or by pressing the `Delete` key.
- Deleting a symbol deletes all connected wires and related wire error messages.

### Moving Selected Symbols
- Left click and drag the mouse
- Both single and multiple objects can be dragged.
- Automatic snapping to grid is supported for when moving symbols.

### Transforming Symbols
- **Rotation** - Can be done for selected symbol via drop-down menu or via shortcuts: `Ctrl-E` for anticlockwise and `Ctrl-Q` for clockwise rotation. Wire routing automatically adjusts.
- **Scaling** 
    - Interactive scaling by dragging from corner of selected/non-selected symbol.
        - Upon selecting a symbol the corners will be highlighted with red cirlces
        - Upon selecting one of these circles, they will turn blue as feedback to the user
    - Selected symbol(s) can also be increased/decreased in side length by 1 grid square length at a time by
        - Selecting Magnify/Diminish in the drop down menu (found in Symbol dropdown options)
        - Or with shortcuts: `Alt-M` (magnify), `Alt-D` (diminish).
- All transformed symbols automatically snap to grid. 
- NOTE: Ports do not automatically snap to grid after a scaling transformation, and require some user input to align correctly.
- Shrinking below a side length of one grid square length is not allowed.

### Aligning Selected Symbol Groups
- May be done via drop-down menu or shortcuts:
    - `Alt-X` for alignment along X-direction (horizontal)
    - `Alt-Y` along Y-direction (vertical). 
- Horizontally aligned symbols cannot be vertically aligned and vice versa.

### Moving of ports and labels
- Ports may be moved to other positions along the edge of the symbol. This is done by holding down the right mouse button at the location of the port and dragging.
- The set of postions where the port can be moved will highlight in purple. Drag the mouse towards one of these locations. Upon mouse release the selected port will be moved (and possibly swapped with) the closest point to the mouse release position. 
- NOTE: Known issue - Subsequent port move operations after the initial one require two tries to work. This is likely due to not updating the model in Sheet correctly but currently unknown why this happens.

## Wires

### Adding Wires
- Dragging a dotted line between symbol ports creates a new wire.
- User friendly UI highlights port positions and possible connections.
- **Width inference** indicates an error by highlighting if input and output port widths are incompatible.
- Wires are automatically routed when first added

### Selecting Wire
- Wires can be selected by:
    - Clicking the wire.
    - Dragging selection box over the wire.
- For two selected symbols, any wires joining them are also selected.

### Deleting selected Wire(s)
- Wires can be deleted by:
    - Pressing the `Delete` key 
    - Using drop down menu (found in Edit -> Delete)

### Routing
- Initial routing: automatic routing with symbol avoidance.
- Routing can be manually adjusted by moving wire segments that aren't connected to any symbol port.
- More segments can be added by right clicking on a wire - three additional segments will appear at the click position.

### Moving Wire Segments (Manual Routing)
- To move wire segments:
    - Left click the section of the wire you wish to move
    - Holding left click, drag the mouse and the selected wire segment will follow.

### Adding Segments to Wires
- When a user right-clicks on top of a wire, three new wire segments are generated in the wire at the click position. 
- This allows the user to have more flexibility in manual routing.

## Overall UI

### Zooming canvas
- Via drop down menu or shortcuts: `Ctrl-[` (zoom out) and `Ctrl-]` (zoom in). Zooming is by factors of 1.25.

### Toggling Selection of Symbols
- On holding down `Ctrl`, the app enters a "toggling" state. In this state, any unselected element that is selected (via click or drag) will become selected and highlighted, while selected elements will unselect and unhighlight. 
- This does not conflict with keyboard shortcuts.

### Error Messages and Highlighting
- When the user attempts to drag a wire between two ports of different width, the illegal connection and the two symbols on either end are highlighted red.
- A popup appears in the topleft corner of the screen explaining the cause of the error.
- Both of the above changes disappear when the illegal connection/one or both of the connected symbols is deleted.

### Copy-Paste
- First highlight the symbol(s) (and wire(s)), using the various selection options (drag, holding ctrl and clicking, clicking on a single symbol)
- Press `Ctrl-Shift-C` to copy. A copy of the selected elements will move with the mouse.
- Click on the canvas where you want to paste. By default once the group of symbols and wires have been pasted, a new copy of these is generated that will move with the mouse.
- To stop this repetitive pasting press `Esc` or `Delete` - or "Cancel" in the Symbol drop-down menu.
- Pressing `Esc` will keep the elements that have just been copied; `Del` will remove them.

### Grid
- The grid on the canvas is currently set to 2000 by 2000
- Each square is 10 x 10