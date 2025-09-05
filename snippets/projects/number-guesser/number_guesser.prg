function canBeInterpretedAsNumeric(sLiteral)
    isValidNumeric = .T.

    /* toDO. implement the verification */

    return isValidNumeric


function toInt(sLiteral) 
    /* 
        verify that the number indeed contains a numeric value  
        so when it is not the case the returned value is NIL and not  
        the default return of Val 0
    */
    nValue = NIL
    if canBeInterpretedAsNumeric(sLiteral)
        nValue = Int(Val(sLiteral))
        ? nValue
    endif

    return nValue


PROCEDURE MAIN
    ? "/*** guess the number ***/"
    ? "ALT + C = quit"                
    random = randomInt(100)
    guess = 0
    do while guess != random
        cGuess = ""
        ACCEPT ">> " TO cGuess

        nGuess = toInt(cGuess)
        if nGuess == NIL
            ? "invalid input !"
        else
            if random > guess
                ? "guess is smaller than actual value"
            elseif random < guess
                ? "guess is bigger than actual value"
            endif 
        endif
    enddo

    if guess == random
        ? "bravo, you found the random value"
    endif
RETURN