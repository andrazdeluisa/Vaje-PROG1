def najvec_jabolk(matrika, n, jabolka=0, i=0):
    if n == 0:
        return jabolka
    elif n == 1:
        return jabolka 
    else: 
        jabolka += matrika[0][i]
    i += 1
    if i < len(matrika[0])-1:
        desno = najvec_jabolk(matrika, n-1, jabolka + matrika[0][i], i)
    else:
        desno = najvec_jabolk(matrika[1:], n-1, jabolka, i=0)
    dol = najvec_jabolk(matrika[1:], n-1, jabolka, i=0)
    return max(desno, dol)

    
test = [[2,4,1,1,],[3,2,0,5],[8,0,2,7]]
