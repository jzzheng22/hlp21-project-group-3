# Team Phase: Checklist

1. [ ] Combine individual module code into single file
    - Meet with module member and submit PR for `src/MODULE.fs` file by 10 March
    - [x] Symbol
    - [ ] BusWire
    - [ ] Sheet

2. Symbol
    * Move ports
        - [x] Add interface for Sheet: `isLabel (model: Model) (pos: XYPos) (sId : CommonTypes.ComponentId) : (XYPos * CommonTypes.PortId) Option`
    * Symbol appearances
        - Wire merge/split suck when rotated
        - [ ] Check all symbols done - Wire label etc
    * Symbol creation needs to be more versatile
        - [x] Change symbol creation to complete custom components etc
    * ISSIE interface
4. BusWire
    * [ ] Integrate collision avoidance
    * [ ] Handle edge cases for autorouting
    * Manual routing
        - Waiting for TC input on indexing
    * Error Highlighting
        - Symbol needs to know if wrong number of wires connected to port
5. Sheet
    * Rotating/scaling user interface
    * Move port user interface
    * Buswire clicking segment interface
    * Error highlighting
6. Other things to think about?
    * Search algorithm efficiency
    * ???