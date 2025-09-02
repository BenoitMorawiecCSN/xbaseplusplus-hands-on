PROCEDURE MAIN
    ? "/* INSERTION SORT */"
    clients = { 1, 5, 4, 3, 7 }
    ? clients

    for i := 2 to Len(clients)
        ? clients[i]
        for j := i to 2 step -1
            if clients[j] > clients[j-1]
                ? "         no swap"
                exit
            else
                ? "     " + Str(clients[j-1]) + "       <->" + Str(clients[j])
                tmp = clients[j-1]
                clients[j-1] =  clients[j]
                clients[j] = tmp
            endif
        next
    next

    ? clients
RETURN