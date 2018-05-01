

{--
    hey alex!  taylor series for euler^x (exp(x)) approx
    as the nth partial series term for sum(
--}
tayloredE :: n -> float
tayloredE x n = fst . take n $ partialSums x 0
    where
        
        


exTerms _ 0 = 1
exTerms x n = (x ** n / n) : exTerms x (n + 1)   



partialSums 