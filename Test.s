const n m = n

id n = n

flip f = \x.\y.(f y x)

succ n = \f.\x.(f ((n f) x))

+ = \m.\n.\f.\x.((m f) ((n f) x))

* = \m.\n.\f.(m (n f))

^ = \m.\n. (n m)

main = ((^ 4) 2)
