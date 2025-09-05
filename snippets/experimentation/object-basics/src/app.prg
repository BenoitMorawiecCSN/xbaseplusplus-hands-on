PROCEDURE MAIN
    oJohn = Client():New("John", "Doe")
    PrintString(                                                    ;
        oJohn:cFirstname + " " + Upper(oJohn:cLastname),            ;
        "name: "                                                    ;
    )
    PrintString(oJohn:cId, "uuid:")
    PrintNumeric(oJohn:GetMemberSinceInDays(), "member for:", "days")
    PrintString("")

    oJane = Client():New("Jane", "Doe")
    PrintString(                                                    ;
        oJane:cFirstname + " " + Upper(oJane:cLastname),            ;
        "name: "                                                    ;
    )
    PrintString(oJane:cId, "uid:")
    PrintNumeric(oJane:GetMemberSinceInDays(), "member for:", "days")
    PrintString("")

    oBobby = Client():New("Bobby", "Doe")
    PrintString(                                                    ;
        oBobby:cFirstname + " " + Upper(obobby:cLastname),          ;
        "name: "                                                    ;
    )
    PrintString(oBobby:cId, "uid:")
    PrintNumeric(oBobby:GetMemberSinceInDays(), "member for", "days")
    PrintString("")
RETURN