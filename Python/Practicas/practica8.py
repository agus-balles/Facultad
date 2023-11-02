from queue import LifoQueue as Pila
from queue import Queue as Cola
import random

def printColaOrPila(fifoLifo):
    while (not fifoLifo.empty()):
        print(fifoLifo.get())

#1)
#a
def contarLineas(archivo: str) -> int:
    buffer = open(archivo,"r")
    contenido:[str] = buffer.readlines()
    return len(contenido)

#b)
def existePalabra(archivo:str,palabra:str)-> bool:
    buffer = open(archivo,"r")
    contenido = buffer.read()
    return (palabra in contenido)

#c)
def cantidad_apariciones(archivo:str,palabra:str) -> int:
    buffer = open(archivo,"r")
    contenido = buffer.read()
    palabras = contenido.split()
    apariciones:int = 0 
    for palabraAver in palabras:
        if palabraAver == palabra:
            apariciones +=1
    return apariciones

#2)
def clonarSinComentarios(archivo:str)-> None:
    buffer = open(archivo,"r")
    nuevoarchivo = open("sinComentario.txt","w")
    contenido:str = buffer.readlines()
    for line in contenido:
        if line.strip()[0] == "#":
            continue
        else:
            nuevoarchivo.write(line)
    buffer.close()
    nuevoarchivo.close()

#3)
def archivoAlReves(archivo:str)-> None:
    buffer = open(archivo,"r")
    nuevoarchivo = open("reverso.txt","w")
    contenido:str = buffer.readlines()
    for i in range(len(contenido)-1,-1,-1):
        nuevoarchivo.write(contenido[i])
    buffer.close()
    nuevoarchivo.close()

#4)
def agregar_frase(archivo:str,frase:str) -> None:
    buffer = open(archivo,"a")
    buffer.write(frase)
    buffer.close()

#5)
def agregar_frase_principio(archivo:str,frase:str) -> None:
    buffer = open(archivo,"r")
    contenido = buffer.readlines()
    buffer.close()
    buffer = open(archivo,"w")
    contenido.insert(0,frase+"\n")
    for linea in contenido:
        buffer.write(linea)
    buffer.close()

#8)
def generar_nros_al_azar(n:int ,desde:int,hasta:int):
    p=Pila()
    for i in range(n):
        p.put(random.randint(desde,hasta))
   
    return p
#9)
def cantidad_elementos(p:Pila)->int:
    pila2:Pila = Pila()
    elementos:int = 0
    while (not p.empty()):
        pila2.put(p.get())
        elementos +=1
    while (not pila2.empty()):
        p.put(pila2.get())
    return elementos

#10)
def buscar_el_maximo(p:Pila)-> int:
    pila2:Pila = Pila()
    max=p.get()
    pila2.put(max)
    while (not p.empty()):
        num=p.get()
        pila2.put(num)
        if (num>= max):
            max=num
    while (not pila2.empty()):
        p.put(pila2.get())
    return max

#11)
def esta_bien_balanceada(s:str)->bool:
    balance:int = 0
    for char in s:
        if (char == "("):
            balance +=1
        elif (char == ")"):
            balance -=1
        if balance <0:
            return False
    return (balance == 0)

#12
def postfix(s:str):
    pila1:Pila = Pila()
    elementos:[str] = s.split()
    for elemento in elementos:
        if (elemento not in "+-*/"):
            pila1.put(elemento)
        else:
            num1 = pila1.get()
            num2 = pila1.get()
            if (elemento=="+"):
                pila1.put(num1+num2)
            elif(elemento=="-"):
                pila1.put(num1 - num2)
            elif(elemento == "*"):
                pila1.put(num1 * num2)
            elif(elemento == "/"):
                pila1.put(num1 / num2)

#13)
def colaAlAzar(cantidadDeNumeros:int,desde:int,hasta:int):
    cola:Cola = Cola()
    numerosAlAzar = generar_nros_al_azar(cantidadDeNumeros,desde,hasta)
    while (not numerosAlAzar.empty()):
        cola.put(numerosAlAzar.get())
    return cola

#14)
def cantidadElementos(cola:Cola)->int:
    colaCopy: Cola = Cola()
    elems:int = 0
    while (not cola.empty()):
        colaCopy.put(cola.get())
        elems +=1
    while (not colaCopy.empty()):
        cola.put(colaCopy.get())
    return elems

#15)
def buscarElMaximo(cola:Cola)->int:
    colaCopy:Cola = Cola()
    max = cola.get()
    colaCopy.put(max)
    while (not cola.empty()):
        num = cola.get()
        colaCopy.put(num)
        if (num >= max):
            max = num
    return max

#16)
def secuenciaBingo()->Cola[int]:
    numeros:[int] = []
    cola:Cola = Cola()
    for i in range(100):
        numeros.append(i)
    random.shuffle(numeros)
    for numero in numeros:
        cola.put(numero)
    return cola

#def jugarBingo(carton:[int],bolillero:Cola[int])->int:
#Como se juega al bingo???

#17)
def pacientesUrgentes(cola:Cola[(int,str,str)])->int:
    pacientesUrgentes:int = 0
    while (not cola.empty()):
        if (cola.get()[0] <= 3):
            pacientesUrgentes +=1
    return pacientesUrgentes

#18)
def aClientes(cola:Cola[(str, int, bool, bool)]) -> Cola[(str, int, bool, bool)]:
    prioridad:Cola=Cola()
    preferencial:Cola= Cola()
    normal:Cola = Cola()
    colaCompleta:Cola = Cola()
    while (not cola.empty()):
        persona:(str, int, bool, bool)=cola.get()
        if (persona[3]):
            prioridad.put(persona)
        elif (persona[2]):
            preferencial.put(persona)
        else:
            normal.put(persona)
    while (not prioridad.empty()):
        colaCompleta.put(prioridad.get())
    while (not preferencial.empty()):
        colaCompleta.put(preferencial.get())
    while (not normal.empty()):
        colaCompleta.put(normal.get())
    return colaCompleta

#19)
def agruparPorLongitud(archivo:str)->dict:
    buffer = open(archivo,"r")
    diccionario:dict ={}
    palabras:[str] = buffer.read().split()
    for palabra in palabras:
        lenght = len(palabra)
        if lenght in diccionario:
            diccionario[lenght] +=1
        else:
            diccionario[lenght] =1
    return diccionario
#21)
def palabraMasFrequente(archivo:str)->str:
    buffer = open(archivo,"r")
    diccionario:dict ={}
    palabras:[str] = buffer.read().split()
    for palabra in palabras:
        if palabra in diccionario:
            diccionario[palabra] +=1
        else:
            diccionario[palabra]= 1
    max:str = list(diccionario.keys())[0]
    for key in diccionario:
        if diccionario[key]>diccionario[max]:
            max =key
    
    return max

#22)
def visitarSitio(historiales:{str:Pila},usuario:str,sitio:str):
    historiales[usuario].put(sitio)

def navegarAtras(historiales:{str:Pila},usuario:str):
    global cachedSite
    cachedSite = historiales[usuario].get()

def navegarAdelante(historiales:{str:Pila},usuario:str):
    historiales[usuario].put(cachedSite)


#23)
inventario:{str:dict} = {}

def agregarProducto(inventario:dict,nombre:str,precio:int,cantidad:int):
    inventario[nombre]={"precio":precio,"cantidad":cantidad}

def actualizarStock(inventario:dict,nombre:str,cantidad:int):
    inventario[nombre]["cantidad"]=cantidad

def actualizarPrecios(inventario:dict,nombre:str,precio:int):
    inventario[nombre]["precio"]=precio

def calcularValorInventario(inventario:dict)->int:
    ValorTotal:int = 0
    for item in inventario:
        ValorTotal += item["cantidad"] * item["precio"] 
    return ValorTotal