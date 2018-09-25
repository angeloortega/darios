
(*Funciones auxiliares*)
(*Algorimto para validar una fecha*) 
let fechaValida = fun day month year -> if (day < 1) || (month < 1) || (month > 12) || (year < 1582) then false (*1582, fecha en la que empieza el calendario gregoriano*)
	else if (month == 2) && (year mod 4 == 0) && (day <= 29) then true
    else if (month == 2) && not(year mod 4 == 0) && (day <= 28) then true
    else if (month < 8)&&((month mod 2) == 1)&&(day <= 31) then true
    else if (month < 8)&&(month != 2)&&((month mod 2) == 0)&&(day <= 30) then true
    else if (month >= 8)&&((month mod 2) == 1)&&(day <= 30) then true
    else if (month >= 8)&&(month != 2)&&((month mod 2) == 0)&&(day <= 31) then true
    else false;;
(*Obtiene la cantidad de meses de un anho*)
let getCandidadDiasMes mes = function year -> 
    if (mes == 2) && ((year mod 4) == 0) then 29
    else if (mes == 2) then 28
    else if (mes <8)&&((mes mod 2) == 0) then 30
    else if (mes < 8) then 31
    else if (mes >= 8) && ((mes mod 2) == 0) then 31
    else if (mes > 8) then 30
    else 31;;

let diaNumToString = function 0 -> "sabado"
	|1 -> "domingo"
	|2 -> "lunes"
	|3 -> "martes"
	|4 -> "miercoles"
	|5 -> "jueves"
	|6 -> "viernes"
	|_ -> "error";;

(*Conseguir el String de mes basado en el numero*)
let mesNumToString = function 1 -> "enero"
    | 2 -> "febrero"
    | 3 -> "marzo"
    | 4 -> "abril"
    | 5 -> "mayo"
    | 6 -> "junio"
    | 7 -> "julio"
    | 8 -> "agosto"
    | 9 -> "septiembre"
    | 10 -> "octubre"
    | 11 -> "noviembre"
    | 12 -> "diciembre"
	| _ -> "error";;


let zeller day = function month -> function year -> (day + (13 * ((month + 1) quo 5)) + (year mod 100) + ((year mod 100) quo 4) + ((year quo 100) quo 4) - (2 * (year quo 100))) mod 7;; 

(*A- Generar un calendario dado un anho y un mes*)

(*Algoritmo para normalizar el algoritmo de zeller a una semana empezando el domingo*)
let getPosicionSemana month = function year -> 
    if (zeller 1 month year) == 0 then 7
    else (zeller 1 month year);;

(*Algoitmo para generar el encabezado de un calendario con un mes y un anho*)
let rec monthAndYear year = function result -> 
    if (string_length result) + 4 < 20 then monthAndYear year (result ^ "_")
    else (result ^ (string_of_int year) ^ "|\n|_D|_L|_K|_M|_J|_V|_S|\n");;

(*Algorimo para generar el encabezado de un calendario*)
let generarEncabezado month = function year -> " ____________________\n|" ^ (monthAndYear year (mesNumToString month));;

(*Algoritmo que genera la cantidad de casillas vacias iniciales*)
let rec inicialesVacias cantidad = function resultado -> 
    if cantidad == 0 then resultado 
    else inicialesVacias (cantidad - 1) (resultado ^ "|__");;

(*Funcion para crear un calendario recursivamente*)
let rec generarCuerpo fecha = function maximo -> function posicion -> function resultado ->
    if (fecha > maximo) &&  (posicion > 7) then resultado ^ "|"
    else if (fecha > maximo) then (generarCuerpo fecha maximo (posicion + 1) (resultado ^ "|__"))
    else if ((posicion > 7) && (fecha < 10)) then (generarCuerpo (fecha + 1) maximo 2 (resultado ^ "|\n|_" ^ (string_of_int fecha)))
    else if ((posicion > 7) && (fecha > 10)) then (generarCuerpo (fecha + 1) maximo 2 (resultado ^ "|\n|" ^ (string_of_int fecha)))
    else if fecha < 10 then (generarCuerpo (fecha + 1) maximo (posicion + 1) (resultado ^ "|_" ^ (string_of_int fecha)))
    else (generarCuerpo (fecha + 1) maximo (posicion + 1) (resultado ^ "|" ^ (string_of_int fecha)));;

(*Funcion que ensambla un calendario y todas sus partes*)
let crearCalendario month = function year -> (generarEncabezado month year) ^ (inicialesVacias ((getPosicionSemana month year)-1) "") ^ (generarCuerpo 1 (getCandidadDiasMes month year) (getPosicionSemana month year) "");;

(*Funcion que genera un calendario con un anho y un mes en particular y lo guarda en un archivo txt*)
let generarCalendario month = function year ->
	let archivo = open_out ((mesNumToString month)^ (string_of_int year) ^ ".txt") in output_string archivo (crearCalendario month year); close_out archivo;;

(*B- Averiguar que dia de la semana es para una fecha en particular*)


let averiguarDia day  = function month -> function year ->diaNumToString (zeller day month year);;(*Determina el dia de la semana a partir de una fecha*)

(*C- Restar dos fechas y dar la respuesta en dias, meses, anhos o dias meses y anhos*)
let rec diferenciaYearsAux day1 = function month1 -> function year1 -> function day2-> function month2 -> function year2 -> (*Funcion auxiliar recursiva para obtener la diferencia en anhos entre dos fechas*)
    if (year1 == (year2-1)) && ((month1 < month2) || (month1 == month2) && (day1 <= day2)) then 1
    else if (year1 < (year2 -1)) then  1 + (diferenciaYearsAux day1 month1 year1 day2 month2 (year2 - 1))
    else 0;;


let diferenciaYears day1 = function month1 -> function year1 -> function day2-> function month2 -> function year2 ->(*Diferencia en anhos entre dos fechas*)
    if not(fechaValida day1 month1 year1) || not(fechaValida day2 month2 year2) then 0
    else if year2 > year1 then diferenciaYearsAux day1 month1 year1 day2 month2 year2
    else if year1 > year2 then diferenciaYearsAux day2 month2 year2 day1 month1 year1
    else 0;;

let rec diferenciaMonthsAux day1 = function month1 -> function year1 -> function day2-> function month2 -> function year2 -> (*Funcion auxiliar recursiva para obtener la diferencia en meses entre dos fechas*)
    if (year1 < (year2 - 1)) then diferenciaMonthsAux day1 month1 year1 day2 month2 (year2 - (diferenciaYearsAux day1 month1 year1 day2 month2 year2)) + ((diferenciaYearsAux day1 month1 year1 day2 month2 year2 ) * 12)
    else if (year1 == (year2 - 1)) then month2 + (diferenciaMonthsAux day1 month1 year1 day2 12 (year2-1))
    else if (month1 < (month2 - 1))|| ((month1 == (month2 -1)) && (day1 < day2)) then  1 + (diferenciaMonthsAux day1 month1 year1 day2 (month2 -1) year2)
    else 0;;


let diferenciaMonths day1 = function month1 -> function year1 -> function day2-> function month2 -> function year2 ->  (*Diferencia en meses entre dos fechas*)
    if not(fechaValida day1 month1 year1) || not(fechaValida day2 month2 year2) then 0
    else if year2 > year1 then diferenciaMonthsAux day1 month1 year1 day2 month2 year2
    else if year1 > year2 then diferenciaMonthsAux day2 month2 year2 day1 month1 year1
    else if month2 > month1 then diferenciaMonthsAux day1 month1 year1 day2 month2 year2
    else if month1 > month2 then diferenciaMonthsAux day2 month2 year2 day1 month1 year1
    else 0;;


(*Convierte una fecha a un unico numero entero*)

let convertirFechaANumero day = function month -> function year -> (365*(year - ((month + 9) mod 12) quo 10) + ((year - ((month + 9) mod   12) quo 10) quo 4) - ((year - ((month + 9) mod   12) quo 10) quo 100) + ((year - ((month + 9) mod   12) quo 10) quo 400) + ((((month + 9)  mod   12)*306 + 5) quo 10) + (day - 1));;
(*Encuentra la diferencia en dias entre dos fechas*)
let diferenciaDays day1 = function month1 -> function year1 -> function day2-> function month2 -> function year2 ->
    if not(fechaValida day1 month1 year1) || not(fechaValida day2 month2 year2) then 0
    else abs ((convertirFechaANumero day1 month1 year1) - (convertirFechaANumero day2 month2 year2));;

(*Funcion auxiliar recursiva para obtener la diferencia en anhos meses y dias entre dos fechas*)
let rec diferenciaExactaAux day1 = function month1 -> function year1 -> function day2-> function month2 -> function year2 ->
    if (month2 > month1) then "La diferencia entre las fechas es de " ^ (string_of_int (diferenciaYears day1 month1 year1 day2 month2 year2)) ^ " anho(s) " ^ (string_of_int (diferenciaMonths day1 month1 year1 day2 month2 year1)) ^ " mes(es) y " ^ (string_of_int (abs(day1 - day2))) ^ " dia(s)."
    else if (month1 == month2) && (day1 > day2) then "La diferencia entre las fechas es de " ^ (string_of_int (diferenciaYears day1 month1 year1 day2 month2 year2)) ^ " anho(s) " ^ (string_of_int (diferenciaMonths day1 month1 year1 day2 month2 (year1 + 1))) ^ " mes(es) y " ^ (string_of_int (diferenciaDays day1 (month1 -1) year1 day2 month2 year1)) ^ " dia(s)."
    else "La diferencia entre las fechas es de " ^ (string_of_int (diferenciaYears day1 month1 year1 day2 month2 year2)) ^ " anho(s) " ^ (string_of_int (diferenciaMonths day1 month1 year1 day2 month2 year1)) ^ " mes(es) y " ^ (string_of_int (abs(day1 - day2))) ^ " dia(s).";;

(*Encuentra la diferencia en dias meses y anhos de dos fechas dadas*)
let diferenciaExacta day1 = function month1 -> function year1 -> function day2-> function month2 -> function year2 ->
    if (convertirFechaANumero day1 month1 year1) < (convertirFechaANumero day2 month2 year2) then diferenciaExactaAux day1 month1 year1 day2 month2 year2
    else if (convertirFechaANumero day1 month1 year1) > (convertirFechaANumero day2 month2 year2) then  diferenciaExactaAux day2 month2 year2 day1 month1 year1
    else "Son la misma fecha.";;

(*D- Obtener una fecha normal a partir de una fecha base*)


(*Convierte de una fecha normal a una fecha basada en un mes y un anho*)
let fechaNormalAFechaBase  day1 = function month1 -> function year1 -> function month2 -> function year2 ->
    if (month2 > 12) || (month2 < 1) || not(fechaValida day1 month1 year1) then "Fecha invalida"
   	else "La fecha basada es: "^ (string_of_int ((getCandidadDiasMes month2 year2) + (diferenciaDays day1 month1 year1 (getCandidadDiasMes month2 year2) month2 year2))) ^ "/" ^(string_of_int month2) ^ "/" ^ (string_of_int year2);;


(*Funcion auxiliar recursiva que convierte una fecha basada a una fecha normal*)
let rec fechaBaseAFechaNormalAux day = function month -> function year -> function rday -> function rmonth -> function ryear ->
    if (day <= (getCandidadDiasMes month year)) then "La fecha normal es: "^ (string_of_int rday) ^ "/" ^(string_of_int rmonth) ^ "/" ^ (string_of_int ryear)
    else if (fechaValida (rday + 1) rmonth ryear) then  fechaBaseAFechaNormalAux (day - 1) month year (rday + 1) rmonth ryear
    else if (fechaValida 1 (rmonth + 1) ryear) then fechaBaseAFechaNormalAux (day - 1) month year 1 (rmonth + 1) ryear
    else fechaBaseAFechaNormalAux (day - 1) month year 1  1 (ryear + 1);;

(*Convierte una fecha basada en un mes y un anho a una fecha normal*)
let fechaBaseAFechaNormal day = function month -> function year -> 
    if (fechaValida day month year) then "La fecha normal es: "^ (string_of_int day) ^ "/" ^(string_of_int month) ^ "/" ^ (string_of_int year)
    else fechaBaseAFechaNormalAux day month year (getCandidadDiasMes month year) month year;;

(*E Obtener la edad de una persona a partir de una fecha*)
(*Funcion para obtener la fecha actual en el calendario gregoriano*)

let edadExacta day = function month -> function year -> (diferenciaExacta day month year ((unix__localtime(unix__time())).unix__tm_mday) (((unix__localtime(unix__time())).unix__tm_mon)+1) (((unix__localtime(unix__time())).unix__tm_year)+1900));;

let edadDays day = function month -> function year ->(diferenciaDays day month year ((unix__localtime(unix__time())).unix__tm_mday) (((unix__localtime(unix__time())).unix__tm_mon)+1) (((unix__localtime(unix__time())).unix__tm_year)+1900));;

let edadMonths day = function month -> function year -> (diferenciaMonths day month year ((unix__localtime(unix__time())).unix__tm_mday) (((unix__localtime(unix__time())).unix__tm_mon)+1) (((unix__localtime(unix__time())).unix__tm_year)+1900));;

let edadYears day = function month -> function year ->(diferenciaYears day month year  ((unix__localtime(unix__time())).unix__tm_mday) (((unix__localtime(unix__time())).unix__tm_mon)+1) (((unix__localtime(unix__time())).unix__tm_year)+1900));;