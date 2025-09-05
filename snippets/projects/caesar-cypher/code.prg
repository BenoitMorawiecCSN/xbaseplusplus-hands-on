function KeepChar(cAlphabet, cChar)
    lContained = cChar $ cAlphabet
    RETURN lContained

function CharIndex(cString, cChar)
    nPosition = -1
    for nIndex := 1 to Len(cString)
        if cString[nIndex] == cChar
            nPosition = nIndex
            exit
        endif
    next
    return nPosition

function EncryptChar(cAlphabet, nKey, cChar)
    nCharModulo = CharIndex(cAlphabet, cChar) - 1
    nNewCharModulo = (nCharModulo + nKey) % Len(cAlphabet)
    nNewChar = cAlphabet[nNewCharModulo + 1]
    return nNewChar

function EncryptMessage(cAlphabet, nKey, cMessage)
    cCypherText = ""
    FOR i := 1 TO Len(cMessage)
        cChar = cMessage[i]
        IF KeepChar(cAlphabet, cChar)
           cCypherText = cCypherText + EncryptChar(cAlphabet, nKey, cChar)
        ENDIF
    NEXT
    return cCypherText

function InteractiveEncrypt(cAlphabet, nKey)
    cMessage = ""
    ACCEPT "message: " TO cMessage
    cCypher = EncryptMessage(cAlphabet, nKey, cMessage)
    ? "cypher text: " + cCypher
    return

function InteractiveDecrypt(cAlphabet, nKey)
    cCypher = ""
    ACCEPT "cyphertext: " TO cCypher
    nDecodeKey = Len(cAlphabet) - nKey
    cMessage = EncryptMessage(cAlphabet, nDecodeKey, cCypher)
    ? "message: " + cMessage
    return

PROCEDURE main 
    cAlphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345689*/+-."
    nKey = 3

    ? "  ___ __ _  ___  ___  __ _ _ __( )___    ___ _   _ _ __ | |__   ___ _ __ "
    ? " / __/ _` |/ _ \/ __|/ _` | '__|// __|  / __| | | | '_ \| '_ \ / _ \ '__| "
    ? "| (_| (_| |  __/\__ \ (_| | |    \__ \ | (__| |_| | |_) | | | |  __/ | " 
    ? " \___\__,_|\___||___/\__,_|_|    |___/  \___|\__, | .__/|_| |_|\___|_|" 
    ? "                                             |___/|_|"
    ? "Select an option"
    ? "1 to encrypt"
    ? "2 to decrypt"
    ? "q to quit"
    sInput = ""
    lRun = .T.
    DO WHILE lRun
        ACCEPT ">> " TO sInput
        DO CASE
        CASE sInput == "1"
            InteractiveEncrypt(cAlphabet, nKey)
        CASE sInput == "2"
            InteractiveDecrypt(cAlphabet, nKey)
        CASE sInput == "q"
            lRun = .F.
            ? "Bye"
        OTHERWISE
            ? "Not a valid option yet"
        ENDCASE
    ENDDO 
RETURN