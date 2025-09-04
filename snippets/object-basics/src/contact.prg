CLASS Client
    CLASS VAR nClientNumbers

    EXPORTED:
        VAR cLastname
        VAR cFirstname
        VAR dRegistrationDate READONLY
        Var cId READONLY

    EXPORTED:
        CLASS METHOD InitClass
        METHOD Init
        METHOD GetMemberSinceInDays
ENDCLASS

CLASS METHOD Client:InitClass()
    self:nClientNumbers = 0

METHOD Client:Init(cFirstname, cLastname)
    ::cFirstname = cFirstname
    ::cLastname = cLastname
    ::dRegistrationDate = Date() - randomInt(3, 5)
    Client():nClientNumbers += 1
    ::cId = Lower(cFirstName[1] + cLastname[1]) + Ltrim(Str(Client():nClientNumbers))
    RETURN self

METHOD Client:GetMemberSinceInDays()
    return Date() - ::dRegistrationDate