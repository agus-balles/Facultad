import math

#Practica6
#1)
def imprimir_hola_mundo() -> None:
        print("Hola Mundo")

def imprimir_un_verso(verso) -> None:
        print(verso)

def raizDe2() -> int:
        print(round(math.sqrt(2),2))

def factorial_de_dos() -> int:
        math.factorial(2)

def perimetro() -> float:
        return (2 * math.pi)

def es_multiplo_de(n: int, m) -> bool:
        return n % m ==0

def imprimir_saludo(nombre: str) -> str:
        print(f"Hola {nombre}")

def raiz_cuadrada_de(x: float) -> float:
        return math.sqrt(x)

def fahrenheit_a_celsius(x:float)->float:
        return ((x-32)*5/9)

def imprimir_dos_veces(estribillo:str) -> str:
        return estribillo * 2

def es_par(x:int) -> int:
        return es_multiplo_de(x,2)

def cantidad_de_pizzas(comensales:int, min_cant_porcion:int)->int:
        porciones_necesarias: int = comensales * min_cant_porcion
        if (es_multiplo_de(porciones_necesarias,8)):
                return porciones_necesarias//8
        else:
                return (porciones_necesarias//8) +1

def alguno_es_0(num1:int,num2:int)->int:
        return (num1==0 or num2==0)

def alguno_es_0(num1:int,num2:int)->int:
        return (num1==0 and num2==0)

def es_nombre_largo(nombre:str) -> bool:
        largo = len(nombre)
        return (largo>=3 and largo <=8)

def es_bisiesto(año:int)->bool:
        return (es_multiplo_de(año,4) and (not (es_multiplo_de(año,100)))) or (es_multiplo_de(año,400))

def peso_pino(altura:int)->int:
        if altura <= 3:
                return altura * 300
        else:
                return 900 + (altura-3)*200

def es_peso_util(peso:int) -> bool:
        return peso>=400 and peso<=1000

def sirve_pino(altura:int)->bool:
        return es_peso_util(peso_pino(altura))

#5)
def devolver_el_doble_si_es_par(x:int ) -> int:
        if (x % 2 == 0):
                return 2*x
        else:
                return x
        
def lindo_nombre(nombre:str)->str:
        if (len(nombre)<5):
                return "Tu nombre tiene menos de 5 caracteres"
        else:
                return "Tu nombre tiene muchas letras!"

def elRango(num:int) -> None:
        if (num<5): 
                print("Menor a 5")
        elif (10<num and num<20):
                print("Entre 10 y 20")
        elif (num>20): 
                print("Mayor a 20")

def anda_a_laburar(sexo: chr,edad:int)-> bool:
        if (sexo == "M") and (edad<18 or edad>=65):
                print("Anda de vacaciones")
        elif (sexo=="F") and (edad<18 or edad>=60):
                print("Anda de vacaciones")
        else:
                print ("Te toca trabajar")

#6

def uno_al_diez()->int:
        i:int=0
        while i<=10:
                print(i)
                i+=1

def pares_diez_a_cuarenta()->None:
        i:int=2
        while i<=40:
                if (i % 2==0):
                        print(i)
                i+=1

