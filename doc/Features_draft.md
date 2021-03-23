# Features
--------

## Symbols:

### Adding Symbols
- Newly added symbol follows mouse movement and positions itself where the mouse first clicks. *Automatic snapping to grid*.

### Selecting Symbols
- May be done by: clicking on a symbol or dragging a box over multiple symbols.

### Deleting Selected Symbols
- May be done via drop-down menu or by pressing the 'delete' key.
- Deleting a symbol deletes all connected wires and wire error messages.

### Moving Selected Symbols
- May be for a single symbol or for a group. Done by dragging a mouse (moving with left button down) so that selected symbols follow. *Automatic snapping to grid*.

### Transforming Symbols
- **Rotation** - Can be done for selected symbol via drop-down menu or via shortcuts: Ctrl-E for clockwise and Ctrl-Q for anticlockwise rotation. Wire routing automatically adjusts.
- **Scaling** - Interactive scaling by dragging from corner of selected/non-delected symbol. Selected symbol(s) can also be scaled up/down by 1.25 via drop down menu or shortcuts: ALt-M (maginfy), ALt-D (diminish).
- *All transformed symbols automatically snap to grid*.

### Aligning Selected Symbol Groups
- May be done via drop-down menu or shortcuts: *Alt-X* for alignment along X-direction (horizontal); *Alt-Y* along Y-direction (vertical). Horizontally aligned symbols cannot be vertically aligned and vice versa.


## Wires

### Adding Wires
- Dragging a dotted line between symbol ports creates a new wire. User friendly UI highlights port positions and possible connections.
- **Width inference** indicates an error by highlighting if input and output port widths are incompatible.

### Selecting Wire
- Done by clicking or dragging selection box. For two selected symbols, all wires joing them are also selected.

### Deleting selected Wire(s)
- Done by pressing 'delete' or by using drop down menu

### Moving Wire Segments
- Dragging mouse so that selected wire segment follows.

### Routing
- Initial routing: automatic routing with symbol avoidance.
- Routing can be manually adjusted by moving wire segments that aren't connected to any symbol port.
- More segments can be added by right clicking on a wire - three additional segments will appear at the click position.

## Overall UI

### Zooming canvas
- Via drop down menu or shortcuts: Ctrl-Z (zoom in) and Ctrl-Y (zoom out). Zooming is in steps of 1.25.

### Copy-Paste
- INSERT EXPLANATION

### Undo-Redo
- INSERT EXPLANATION
