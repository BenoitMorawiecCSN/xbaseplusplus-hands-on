function PrintString(cText, cTextBeforeValue, cTextAfterValue)
    cOutput = ""

    if ((cTextBeforeValue != NIL) .AND. (Len(cTextBeforeValue) > 0))
        cOutput = cTextBeforeValue + " "
    endif

    cOutput = cOutput + cText
    if (cTextAfterValue != NIL) .AND. (Len(cTextAfterValue) > 0)
        cOutput = cOutput + " " + cTextAfterValue
    endif
    ? cOutput
    return

function PrintNumeric(nValue, cTextBeforeValue, cTextAfterValue)
    PrintString(AllTrim(Str(nValue)), cTextBeforeValue, cTextAfterValue)
    return