# -*- coding: utf-8 -*-

# %%
# Generate a factorial
def fact(n):
    res = 1
    
    for i in range(2, n+1):
        res = res * i
    return res

# %%
# Generate a binomial coefficient
def binomcoef(n,k):
    return(round(fact(n)/(fact(k)*fact(n-k))))

# %%
# Pascals Triangle
def pascals_triangle(n):
    from collections import defaultdict
    ptriangle = defaultdict(list)
    for i in range(n+1):
        lst = []    
        lst = [binomcoef(x, y) for x in [i] for y in range(i+1)]
        ptriangle[i].append(lst)
        print(lst)
    return(ptriangle)
 

# %%
# Generate a centralbinomial number
def centralbinom(x):
    return(fact(2* x) / (fact(x)**2))

# %%
# Check whether a number is central binmial
def iscentralbinom(x):
    
    start = 0
    cbnm = centralbinom(start)

   
    while cbnm < x:                   
                     
        cbnm = centralbinom(start)
        start += 1
        
        
    if x == cbnm:
        return True
    else:
        return False
       

# %%

pt = pascals_triangle(10)

iscentralbinom(6)
iscentralbinom(centralbinom(10))
iscentralbinom(5)
iscentralbinom(21)


for i in range(10):
    print(centralbinom(i))



