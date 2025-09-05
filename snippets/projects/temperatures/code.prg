/*
    CodingGame: Temperatures
    Goal: input a list of temperatures and find the one closest to 0
*/

function NumericInput(cText)
    if cText != NIL
        ? cText
    endif
    cInput = ""
    ACCEPT ">> " TO cInput
    return Val(cInput)

function PrintNumeric(cText, nValue)
    ? cText + Str(nValue)
    return

function GetBigest(nA, nB) 
    IF nA > nB
        return nA
    ELSE
        return nB
    ENDIF

function GetClosestToZero(nA, nB)
    IF Abs(nA) > Abs(nB)
        return nB
    ELSEIF Abs(nA) < Abs(nB)
        return nA
    ELSE
        ** in case of tie, select the one above 0
        return GetBigest(nA, nB)
    ENDIF


PROCEDURE main
    nTemperatureNumbers = NumericInput("Number of Temperatures")
    ? "Enter the temperatures"
    nClosestToZero = NIL
    FOR nIndex := 1 TO nTemperatureNumbers
        nTemperaturePrompt = NumericInput()
        if nIndex == 1
            nClosestToZero = nTemperaturePrompt
        else
            nClosestToZero = GetClosestToZero(nClosestToZero, nTemperaturePrompt)
        endif
    NEXT

    PrintNumeric("closest to Zero: ", nClosestToZero)
RETURN