#include "Appevent.ch"

// user-defined pushbutton
CLASS MyPushbutton FROM XbpPushbutton
    EXPORTED:
    METHOD init, activate
ENDCLASS

// initialize superclass and self
METHOD MyPushbutton:init( cCaption, aPos, aSize )
    ::XbpPushButton:init(,, aPos, aSize )
    ::caption := cCaption
RETURN self

// callback method for the event xbeP_Activate
METHOD MyPushbutton:activate
    QOut( "Pushbutton ", ::caption )
RETURN self

PROCEDURE Main
    LOCAL nEvent, mp1, mp2, oXbp
    SetColor("N/W")
    CLS

    // get application window
    oXbp := SetAppWindow()

    // display character corresponding to key pressed
    oXbp:KeyBoard := {|mp1| QQOut( Chr(mp1) ) }

    // create user-defined pushbuttons
    MyPushButton():new( "A", { 10,20}, {100,40} ):create()
    MyPushButton():new( "B", {150,20}, {100,40} ):create()

    // Event loop
    nEvent := 0
    DO WHILE nEvent <> xbeP_Close
        nEvent := AppEvent( @mp1, @mp2, @oXbp )
        oXbp:HandleEvent( nEvent, mp1, mp2 )
    ENDDO
RETURN