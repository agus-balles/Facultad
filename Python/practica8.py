from queue import LifoQueue as Pila
import random
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
   
    while (not p.empty()):
        print(p.get())
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
    