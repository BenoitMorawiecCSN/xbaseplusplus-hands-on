PROCEDURE main
    aGrid = {                               ;
        {8, 0, 0, 0, 5, 0, 0, 0, 0},        ;
        {4, 0, 0, 0, 0, 0, 9, 1, 0},        ;
        {0, 7, 0, 1, 0, 0, 0, 0, 0},        ;
        {0, 0, 0, 0, 0, 8, 7, 0, 4},        ;
        {0, 0, 0, 5, 3, 2, 0, 0, 0},        ;
        {0, 0, 0, 0, 0, 7, 6, 0, 0},        ;
        {0, 0, 5, 8, 0, 0, 0, 0, 2},        ;
        {0, 0, 0, 0, 0, 0, 0, 0, 3},        ;
        {0, 3, 2, 0, 0, 0, 0, 7, 0},        ;
    }
    oGrid = Grid():new(aGrid)
    oSolver = Solver():new(oGrid)
    lIsSolved = oSolver:solveGrid()
    ? lIsSolved
RETURN