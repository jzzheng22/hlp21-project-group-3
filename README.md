# HLP21 Group 3 Main Repo

This repo contains our final deliverable code for the group project of HLP (ELEC 96019) 2021. It is an F# Elmish MVU implementation of the Draw2D JavaScript library used in [ISSIE](https://github.com/tomcl/ISSIE), which implements many advanced features in addition to those found in the current ISSIE application. 

Code may be run by entrering on the command line `npm run dev` from within this root directory.

Code written by the group members for their individual modules may be found by navigating to the `indiv_code` branch (link [here](https://github.com/jzzheng22/hlp21-project-group-3/tree/indiv_code)) and entering these directories:


## External Interface 

Documentation may be found in the [*doc*](doc) folder:
- [*Features.md*](doc/features.md) lists out the different features of our implementation of the Draw2D library as well as the user interface for each feature. **This is the single page summary of features desired for the assessment**.
- *External Interfaces.md* is useful for future integration with ISSIE/ further development of code. It explains how the program could integrate with the existing ISSIE code e.g. the widthInferrer.
- The folder *Internal Interfaces* contains the interfaces between the modules *Symbol.fs*, *BusWire.fs* and *Sheet.fs*. This is for reference.
