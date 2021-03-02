# Joanna Merrick (jem18 - CID: 01504728)
(Member of group 3)

## An Overview of the Code

The module implemented here is **Symbol**

-This code uses Aaman's Sheet, and Aditya's Buswire implementations as stubs.
-The renderer.fs has also been modified to fit with these modules and add some features specific to symbol such as 'Rotation' and 'Scaling'

The code may be run by entering the following from within this directory
```
npm run dev
```

## Demo outline

1. Different symbols
    * I will have symbols of each general category created in the init for the demo, so will be displayed on startup.
2. Highlight message - Symbol selection (& Mouse events can select symbols)
    * Click/drag symbol -> Symbol will be selected/highlighted
3. Port highlight message
    * Move cursor near symbol -> Ports will be highlighted
4. Move message
    * Click/drag symbol -> Symbol will move with the cursor
4. Rotate message
    * Select a symbol -> Press alt+shift+R to show rotation functionality works
5. Scale message
    * Select a symbol -> Press alt+shift+U to scale up by 1.25, alt+shift+D to scale down by 0.85 in both x and y directions, to show scale functionality works.
6. Delete message
    * Select a symbol -> Press delete, symbol will be removed.
7. HighlightError message
    * I will show the code/message which will highlight the symbol in red when there is an error
7. DragPort message - Move ports manually
    * I will show the code/message that I have written that will allow a selected port to be moved to another location

