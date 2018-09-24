import Data.List
import System.IO
import Data.Time.Clock
import Data.Time.Calendar
import System.IO.Unsafe

fechaValida :: Int -> Int -> Int -> Bool

fechaValida day month year --Algorimto para validar una fecha
    | (day < 1) || (month < 1) || (month > 12) || (year < 1582) = False --1582, fecha en la que empieza el calendario gregoriano
    | (month == 2) && (year `mod` 4 == 0) && (day <= 29) = True
    | (month == 2) && not(year `mod` 4 == 0) && (day <= 28) = True
    | (month < 8)&&(mod month 2 == 1)&&(day <= 31) = True
    | (month < 8)&&(month /= 2)&&(mod month 2 == 0)&&(day <= 30) = True
    | (month >= 8)&&(mod month 2 == 1)&&(day <= 30) = True
    | (month >= 8)&&(month /= 2)&&(mod month 2 == 0)&&(day <= 31) = True
    | otherwise = False


--a
getPosicionSemana month year --Algoritmo para normalizar el algoritmo de zeller a una semana empezando el domingo
    |(zeller 1 month year) == 0 = 7
    |otherwise = (zeller 1 month year)

monthAndYear year result -- Algoitmo para generar el encabezado de un calendario con un mes y un anho
    | (length result) + 4 < 20 = monthAndYear year (result ++ "_")
    | otherwise = (result ++ (show year) ++ "|\n|_D|_L|_K|_M|_J|_V|_S|\n")  

generarEncabezado month year = " ____________________\n|" ++ (monthAndYear year (mesNumToString month)) --Algorimo para generar el encabezado de un calendario

inicialesVacias cantidad resultado --Algoritmo que genera la cantidad de casillas vacias iniciales
    | cantidad == 0 = resultado 
    | otherwise = inicialesVacias (cantidad - 1) (resultado ++ "|__")


generarCuerpo fecha maximo posicion resultado --Funcion para crear un calendario recursivamente
    | (fecha > maximo) &&  (posicion > 7) = resultado ++ "|"
    | (fecha > maximo) = (generarCuerpo fecha maximo (posicion + 1) (resultado ++ "|__"))
    | posicion > 7 = (generarCuerpo (fecha + 1) maximo 1 (resultado ++ "|\n"))
    | fecha < 10 = (generarCuerpo (fecha + 1) maximo (posicion + 1) (resultado ++ "|_" ++ (show fecha)))
    | otherwise = (generarCuerpo (fecha + 1) maximo (posicion + 1) (resultado ++ "|" ++ (show fecha)))

crearCalendario month year = (generarEncabezado month year) ++ (inicialesVacias ((getPosicionSemana month year)-1) "") ++ (generarCuerpo 1 (getCandidadDiasMes month year) (getPosicionSemana month year) "")
--Funcion que ensambla un calendario y todas sus partes

--Funcion que genera un calendario con un anho y un mes en particular y lo guarda en un archivo txt
generarCalendario month year = do
    archivo <- openFile((mesNumToString month)++ (show year) ++ ".txt") WriteMode
    hPutStrLn archivo (crearCalendario month year)
    hClose archivo

--b Averiguar un dia de la semana particular

zeller :: Int -> Int -> Int -> Int

zeller day month year = (day + (13 * (month + 1)`quot`5) + (year `mod` 100) + ((year `mod` 100)`quot`4) + ((year `quot` 100) `quot` 4) - (2 * (year `quot` 100))) `mod` 7 
--Algorimo de zeller para calcular que dia de la semana cae una fecha determinada

diaNumToString :: Int -> String

diaNumToString dia -- Conseguir el string del dia de la semana basado en el numero
    | dia == 0 = "sabado"
    | dia == 1 = "domingo"
    | dia == 2 = "lunes"
    | dia == 3 = "martes"
    | dia == 4 = "miercoles"
    | dia == 5 = "jueves"
    | dia == 6 = "viernes"
    | otherwise = "error"

mesNumToString mes -- Conseguir el String de mes basado en el numero
    | mes == 1 = "enero"
    | mes == 2 = "febrero"
    | mes == 3 = "marzo"
    | mes == 4 = "abril"
    | mes == 5 = "mayo"
    | mes == 6 = "junio"
    | mes == 7 = "julio"
    | mes == 8 = "agosto"
    | mes == 9 = "septiembre"
    | mes == 10 = "octubre"
    | mes == 11 = "noviembre"
    | mes == 12 = "diciembre"


averiguarDia :: Int -> Int -> Int -> String

averiguarDia day month year --Determina el dia de la semana a partir de una fecha
    | not (fechaValida day month year) = "Error de fecha"
    | (month == 1) = diaNumToString (zeller 13 month (year - 1))
    | (month == 2) = diaNumToString (zeller 14 month (year -1))
    | otherwise = diaNumToString (zeller day month year)

--c Restar dos fechas y dar la respuesta en dias, meses, anhos o dias meses y anhos


diferenciaYears :: Int -> Int -> Int -> Int -> Int -> Int -> Int

diferenciaYears day1 month1 year1 day2 month2 year2 --Diferencia en anhos entre dos fechas
    | not(fechaValida day1 month1 year1) || not(fechaValida day2 month2 year2) = 0
    | year2 > year1 = diferenciaYearsAux day1 month1 year1 day2 month2 year2
    | year1 > year2 = diferenciaYearsAux day2 month2 year2 day1 month1 year1
    | otherwise = 0

diferenciaYearsAux :: Int -> Int -> Int -> Int -> Int -> Int -> Int

diferenciaYearsAux day1 month1 year1 day2 month2 year2 --Funcion auxiliar recursiva para obtener la diferencia en anhos entre dos fechas
    | (year1 == (year2-1)) && ((month1 < month2) || (month1 == month2) && (day1 <= day2)) = 1
    | (year1 < (year2 -1)) =  1 + (diferenciaYearsAux day1 month1 year1 day2 month2 (year2 - 1))
    | otherwise = 0


diferenciaMonths :: Int -> Int -> Int -> Int -> Int -> Int -> Int

diferenciaMonths day1 month1 year1 day2 month2 year2  --Diferencia en meses entre dos fechas
    | not(fechaValida day1 month1 year1) || not(fechaValida day2 month2 year2) = 0
    | year2 > year1 = diferenciaMonthsAux day1 month1 year1 day2 month2 year2
    | year1 > year2 = diferenciaMonthsAux day2 month2 year2 day1 month1 year1
    | month2 > month1 = diferenciaMonthsAux day1 month1 year1 day2 month2 year2
    | month1 > month2 = diferenciaMonthsAux day2 month2 year2 day1 month1 year1
    | otherwise = 0

diferenciaMonthsAux :: Int -> Int -> Int -> Int -> Int -> Int -> Int

diferenciaMonthsAux day1 month1 year1 day2 month2 year2 --Funcion auxiliar recursiva para obtener la diferencia en meses entre dos fechas

    | (year1 < (year2 - 1)) =  diferenciaMonthsAux day1 month1 year1 day2 month2 (year2 - (diferenciaYearsAux day1 month1 year1 day2 month2 year2)) + ((diferenciaYearsAux day1 month1 year1 day2 month2 year2 ) * 12)
    | (year1 == (year2 - 1)) = month2 + (diferenciaMonthsAux day1 month1 year1 day2 12 (year2-1))
    | (month1 < (month2 - 1))|| ((month1 == (month2 -1)) && (day1 < day2)) =  1 + (diferenciaMonthsAux day1 month1 year1 day2 (month2 -1) year2)
    | otherwise = 0


convertirFechaANumero :: Int -> Int -> Int -> Int --Convierte una fecha a un unico numero entero
convertirFechaANumero day month year= 365*(year - ((month + 9)`mod` 12)`quot`10) + (year - ((month + 9)`mod` 12)`quot`10)`quot`4 - (year - ((month + 9)`mod` 12)`quot`10)`quot`100 + (year - ((month + 9)`mod` 12)`quot`10)`quot`400 + (((month + 9) `mod` 12)*306 + 5)`quot`10 + (day - 1)

diferenciaDays :: Int -> Int -> Int -> Int -> Int -> Int -> Int --Encuentra la diferencia en dias entre dos fechas

diferenciaDays day1 month1 year1 day2 month2 year2
    | not(fechaValida day1 month1 year1) || not(fechaValida day2 month2 year2) = 0
    | otherwise = abs ((convertirFechaANumero day1 month1 year1) -(convertirFechaANumero day2 month2 year2))


diferenciaExacta :: Int -> Int -> Int -> Int -> Int -> Int -> String
diferenciaExacta day1 month1 year1 day2 month2 year2 --Encuentra la diferencia en dias meses y anhos de dos fechas dadas
    | (convertirFechaANumero day1 month1 year1) < (convertirFechaANumero day2 month2 year2) = diferenciaExactaAux day1 month1 year1 day2 month2 year2
    | (convertirFechaANumero day1 month1 year1) > (convertirFechaANumero day2 month2 year2) = diferenciaExactaAux day2 month2 year2 day1 month1 year1
    | otherwise = "Son la misma fecha."

diferenciaExactaAux :: Int -> Int -> Int -> Int -> Int -> Int -> String

diferenciaExactaAux day1 month1 year1 day2 month2 year2 --Funcion auxiliar recursiva para obtener la diferencia en anhos meses y dias entre dos fechas
    | (month2 > month1) = "La diferencia entre las fechas es de " ++ (show (diferenciaYears day1 month1 year1 day2 month2 year2)) ++ " anho(s) " ++ (show (diferenciaMonths day1 month1 year1 day2 month2 year1)) ++ " mes(es) y " ++ (show (abs(day1 - day2))) ++ " dia(s)."
    | (month1 == month2) && (day1 > day2) ="La diferencia entre las fechas es de " ++ (show (diferenciaYears day1 month1 year1 day2 month2 year2)) ++ " anho(s) " ++ (show (diferenciaMonths day1 month1 year1 day2 month2 (year1 + 1))) ++ " mes(es) y " ++ (show (diferenciaDays day1 (month1 -1) year1 day2 month2 year1)) ++ " dia(s)."
    | otherwise = "La diferencia entre las fechas es de " ++ (show (diferenciaYears day1 month1 year1 day2 month2 year2)) ++ " anho(s) " ++ (show (diferenciaMonths day1 month1 year1 day2 month2 year1)) ++ " mes(es) y " ++ (show (abs(day1 - day2))) ++ " dia(s)."

--d Obtener una fecha normal a partir de una fecha base

getCandidadDiasMes mes year --Obtiene la cantidad de meses de un anho
    | (mes == 2) && ((year `mod` 4) == 0) = 29
    | (mes == 2) = 28
    | (mes <8)&&((mes `mod` 2) == 0) = 30
    | (mes < 8) = 31
    | (mes >= 8) && ((mes `mod` 2) == 0) = 31
    | otherwise = 31

fechaNormalAFechaBase :: Int -> Int -> Int -> Int -> Int -> String

fechaNormalAFechaBase day1 month1 year1 month2 year2 --Convierte de una fecha normal a una fecha basada en un mes y un anho
    |(month2 > 12) || (month2 < 1) || not(fechaValida day1 month1 year1) = "Fecha invalida"
    | otherwise = "La fecha basada es: "++ (show ((getCandidadDiasMes month2 year2) + (diferenciaDays day1 month1 year1 (getCandidadDiasMes month2 year2) month2 year2))) ++ "/" ++(show month2) ++ "/" ++ (show year2)

fechaBaseAFechaNormal :: Int -> Int -> Int -> String

fechaBaseAFechaNormal day month year --Convierte una fecha basada en un mes y un anho a una fecha normal
    | (fechaValida day month year) = "La fecha normal es: "++ (show day) ++ "/" ++(show month) ++ "/" ++ (show year)
    |otherwise = fechaBaseAFechaNormalAux day month year (getCandidadDiasMes month year) month year


fechaBaseAFechaNormalAux day month year rday rmonth ryear --Funcion auxiliar recursiva que convierte una fecha basada a una fecha normal
    | (day <= (getCandidadDiasMes month year)) =  "La fecha normal es: "++ (show rday) ++ "/" ++(show rmonth) ++ "/" ++ (show ryear)
    | (fechaValida (rday + 1) rmonth ryear) = fechaBaseAFechaNormalAux (day - 1) month year (rday + 1) rmonth ryear
    | (fechaValida 1 (rmonth + 1) ryear) = fechaBaseAFechaNormalAux (day - 1) month year 1 (rmonth + 1) ryear
    | otherwise = fechaBaseAFechaNormalAux (day - 1) month year 1  1 (ryear + 1)


--e Obtener la edad de una persona a partir de una fecha
--Funcion para obtener la fecha actual en el calendario gregoriano
fechaActual :: IO (Integer, Int, Int)
fechaActual = fmap (toGregorian . utctDay) getCurrentTime

getFirst(a,_,_)= a --Obtener el primer elemento de una tupla de 3 elementos
getSecond(_,a,_)= a --Obtener el segundo elemento de una tupla de 3 elementos
getThird(_,_,a)= a  --Obtener el tercer elemento de una tupla de 3 elementos

getYearActual = getFirst $unsafePerformIO$ fechaActual --Obtener el anho actual

getMonthActual = getSecond $unsafePerformIO$ fechaActual --Obtener el mes actual

getDayActual = getThird $unsafePerformIO$ fechaActual --Obtener el dia actual

edadExacta :: Int -> Int -> Int -> String --Funcion que encuentra la edad en dia meses y anhos de una  persona

edadDays :: Int -> Int -> Int -> Int --Funcion que encuentra la edad en dias de una persona con su fecha de nacimiento

edadMonths :: Int -> Int -> Int -> Int --Funcion que encuentra la edad en meses de una persona con su fecha de nacimiento

edadYears :: Int -> Int -> Int -> Int --Funcion que encuentra la edad en anhos de una persona con su fecha de nacimiento

edadExacta day month year = (diferenciaExacta day month year getDayActual getMonthActual (fromIntegral(getYearActual)))

edadDays day month year = (diferenciaDays day month year getDayActual getMonthActual (fromIntegral(getYearActual)))

edadMonths day month year = (diferenciaMonths day month year getDayActual getMonthActual (fromIntegral(getYearActual)))

edadYears day month year = (diferenciaYears day month year getDayActual getMonthActual (fromIntegral(getYearActual)))
