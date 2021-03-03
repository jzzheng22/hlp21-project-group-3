# Aaman Rebello (apr1718 - CID: 01488753)
(Member of group 3)


## An Overview of the Code

The module implemented here is **Sheet**.

As compared to the project demo skeleton code:
- *sheet.fs* was modified to enter code for the sheet module.
- *renderer.fs* was modified to add drop-down menu options for certain features e.g. zooming.

A documentation of how this module interfaces with the Symbol and BusWire modules may be found in the [main part](..) of the repo.

The code may be run by entering on the command line

```
npm run dev

```

from within this directory (on the first run the code has to be built).


## Sequence of Steps for the Demo

1. *Highlighting of ports* on moving mouse over symbol. Demonstrate it working for multiple symbols simultaneously i.e. when mouse is inside more than one bounding box.

2. *Selection and highlighting* of single (with click) and multiple symbols (with drag-box).

3. *Dragging and snap-to-grid*. For single and multiple symbols (the multiple symbols can be done by dragging mouse from any of the selected components). Correct behaviour in presence of scrolling.

4. *Automatic aligning* of multiple selected symbols horizontally/vertically. Illustrating utility of vertical alignment with Joanna's functionality to rotate/scale symbols (may skip this if pushed for time).

5. *Dragging wires between ports*. Wires adjust on moving symbols.

6. *Deleting* wires and symbols which automatically delete connected wires.  

7. *Adding symbols* - illustrate algorithm to initialise positions of added components.

7. *Zooming behaviour*: Points 1-7 (may not cover all) briefly demonstrated in presence of zooming in/out on the canvas.

Target time for demo: 300s.