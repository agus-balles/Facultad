import numpy

def pertenece(lista:[int],x:int):
    for i in lista:
        if(x==i): return True
    return False

def divideATodos(lista:[int],x:int):
    for num in lista:
        if(num % x):
            continue
        else:
            return False
    return True

def ordenados(lista:[int]) -> bool:
    for i in range(len(lista)-1):
        if(lista[i] <= lista[i+1]):
            continue
        else:
            return False
    return True


def fortaleza(contra:str)->str:
    if (len(contra)<=5):
        return "ROJO"
    if (len(contra)>=8):
        cantMin:int=0
        cantMay:int=0
        cantNum:int=0
        for letra in contra:
            if "a"<= letra <="z":
                cantMin +=1
            if "A"<=letra<="Z":
                    cantMay +=1
            if "1"<=letra<="9":
                        cantNum +=1
        if (cantMay >=1 and cantMin>=1 and cantNum >=1):
            return "VERDE"
    return "AMARILLA"

def cuenta_banco(movimientos:[(str,int)])->int:
    saldo=0
    for i in range(len(movimientos)):
        if (movimientos[i][0]=="I"):
            saldo += movimientos[i][1]
        elif (movimientos[i][0]=="R"):
            saldo -= movimientos[i][1]
    return saldo

def tres_vocales(palabra:str)->bool:
    vocales = ['a','e','i','o','u']
    contadorVocal = 0
    for vocal in vocales:
        if (vocal in palabra):
            contadorVocal +=1
    if contadorVocal >= 3 :
        return True
    else:
        return False
    
#2)
def cero_por_pares(lista:[int])-> [int]:
    for i in range(len(lista)):
        if (i % 2) == 0:
            lista[i] = 0
    return lista

def cero_por_pares_in(lista:[int])-> [int]:
    lista_mod = lista.copy()
    for i in range(len(lista_mod)):
        if (i % 2) == 0:
            lista_mod[i] = 0

def sacar_vocales(palabra:str)->[str]:
    vocales = ['a','e','i','o','u','A','E','I','O','U']
    palabraNueva:str = ""
    for i in range(len(palabra)):
        if (palabra[i] not in vocales):
            palabraNueva += palabra[i]
    return palabraNueva

def reemplazaVocales(palabra:str)->[str]:
    vocales = ['a','e','i','o','u','A','E','I','O','U']
    palabraNueva:str = ""
    for i in range(len(palabra)):
        if (palabra[i] not in vocales):
            palabraNueva += palabra[i]
        else:
            palabraNueva += "_"
    return palabraNueva

def daVueltaStr(palabra:[chr])-> [chr]:
    palabraNueva:[chr] =""
    for i in range(len(palabra)):
        palabraNueva+=palabra[len(palabra)-i-1]
    return palabraNueva

def eliminarRepetidos(palabra:[chr]) -> [chr]:
    palabraNueva:[chr] = []
    for c in palabra:
        if c not in palabraNueva:
            palabraNueva.append(c)
    return palabraNueva

def aprobado(notas:[int])->int:
    mayora4:int = 0
    sumaNotas=0
    for nota in notas:
        if nota < 4:
            return 3
        if 4<= nota:
            mayora4 +=1
        sumaNotas += nota
    promedio = sumaNotas / len(notas)
    if promedio >= 7:
        return 1
    else:
        return 2

def estudiantes()-> [str]:
    listaEstudiantes:[str] =[]
    entrada = input("ingrese nombre de estudiante: ")
    while entrada != "listo":
        listaEstudiantes.append(entrada)
        entrada = input("ingrese nombre de estudiante: ")
    return listaEstudiantes

#5)
def perteneceACadaUno(listas:[[int]],num:int,res:bool):
    for lista in listas:
        if num in lista:
            continue
        else:
            return False
    return True

def esMatriz(listas:[[int]])->bool:
    if len(listas==0): return False
    for i in range(len(listas)):
        if len(listas[i]) == len(listas[i+1]):
            continue
        else:
            return False 
    return True

def filasOrdenadas(listas:[[int]])->[bool]:
    for lista in listas:
        if ordenados(lista):
            continue
        else:
            return False
    return True

def matriz_al_azar_potencia(d, p):
    # Genera una matriz cuadrada aleatoria de tamaño d
    matriz = numpy.random.rand(d, d)

    # Realiza la multiplicación de la matriz por sí misma p veces
    resultado = matriz
    for _ in range(p - 1):
        resultado = numpy.dot(resultado, matriz)

    return resultado