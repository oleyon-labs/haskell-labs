quadraticEq a b c =
    let
        x1 = (- b - d) / a2
        x2 = (- b + d) / a2
        d = sqrt $ b ^ 2 - 4 * a * c
        a2 = 2 * a
    in (x1, x2)

recurrentSolve 0 = 1
recurrentSolve 1 = 2
recurrentSolve 2 = 3
recurrentSolve k = recurrentSolve (k - 2) + recurrentSolve (k - 1) - 2 * recurrentSolve (k - 3)


recurrentSolve' k = helper k 1 2 3
    where
        helper 0 k3 k2 k1 = k3
        helper 1 k3 k2 k1 = k2
        helper 2 k3 k2 k1 = k1
        helper k k3 k2 k1 = helper (k - 1) k2 k1 (k2 + k1 - 2 * k3)

