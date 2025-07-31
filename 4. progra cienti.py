# Formula de Leibniz para el cálculo de pi
# Lo compararemos con el producto de Wallis

def approachLeibniz(n):
    serie = []
    for i in range(n):
        a = (-1)** i
        b = 2*i + 1
        serie.append(a/b)
    return 4 * sum(serie)

approachLeibniz(100000)


