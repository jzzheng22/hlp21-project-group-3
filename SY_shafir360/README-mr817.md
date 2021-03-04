# Overview of Symbol Code
Group 3
mr817
CID 01757005

## How to run
Use 
```
npm run dev
```
or 

```
build
```
within the directory

## Features

 - All symbols implemented - All symbols can be displayed. Most symbol except merge wire and split wire is fairly similar


 - Adding symbol - Currently the demo uses init function to add symbols.The code has the ability to allow sheet to add symbol using an add msg.

 - Deleting symbol- Can be deleted by selecting symbol and pressing del key (functionality comes from sheet through the use of messages to symbol)

 - Selection - Can be selected through invidiual symbol clicks or multiple symbol can be highlighted using drag and drop of an area. Functionality implemented using messages from sheets.

 - Moving symbol-Symbol can be moved by dragging symbol and can show indication of drag state through colour change. Functionality using message from sheet. Sheet sends a message instructing a symbol or mutiple symbol with new position coordinate.

 - Port highlighting- Ports are highlighted when hovering near symbol. 

 - Symbol can be used to draw wires from one symbol to another using buswire. Can show port selection working though phantom wires.
