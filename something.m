list3 (Cons a (Cons b (Cons c Nil))) = True
list3 x                              = False, x==2
                                     = True, x==3
                                     = False
list3 asdf = asdf

constantone = 2-1
main = let a = [1, 2, 3] in list3 a
