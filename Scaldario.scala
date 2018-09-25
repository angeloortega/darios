import java.util.Calendar
import java.io.PrintWriter
def fechaValida(day:Int, month:Int, year:Int):Boolean ={//Algorimto para validar una fecha
    if((day < 1) || (month < 1) || (month > 12) || (year < 1582)){return false} //1582, fecha en la que empieza el calendario gregoriano
    else if((month == 2) && (year % 4 == 0) && (day <= 29)){return true}
    else if((month == 2) && !(year % 4 == 0) && (day <= 28)){return true}
    else if((month < 8)&&(month % 2 == 1)&&(day <= 31)){return true}
    else if((month < 8)&&(month != 2)&&(month % 2 == 0)&&(day <= 30)){return true}
    else if((month >= 8)&&(month % 2 == 1)&&(day <= 30)){return true}
    else if((month >= 8)&&(month != 2)&&(month % 2 == 0)&&(day <= 31)){return true}
    else{return false}
	}



def diaNumToString(dia:Int):String = dia match{// Conseguir el string del dia de la semana basado en el numero
    case 0 => "sabado"
    case 1 => "domingo"
    case 2 => "lunes"
    case 3 => "martes"
    case 4 => "miercoles"
    case 5 => "jueves"
    case 6 => "viernes"
    case _ => "error"
    }

def mesNumToString(mes:Int):String = mes match{ // Conseguir el String de mes basado en el numero
    case 1 => "enero"
    case 2 => "febrero"
    case 3 => "marzo"
    case 4 => "abril"
    case 5 => "mayo"
    case 6 => "junio"
    case 7 => "julio"
    case 8 => "agosto"
    case 9 => "septiembre"
    case 10 => "octubre"
    case 11 => "noviembre"
    case 12 => "diciembre"
    case _ => "mes invalido"
}

//a
def getPosicionSemana(month:Int, year:Int):Int ={ //Algoritmo para normalizar el algoritmo de zeller a una semana empezando el domingo
    if(zeller(1,month,year) == 0){return 7}
    else{ return zeller(1,month,year)}
}

def monthAndYear(year:Int, result:String):String ={ // Algoitmo para generar el encabezado de un calendario con un mes y un anho
    if (result.length + 4 < 20 ) {return monthAndYear(year, (result.concat("_")))}
    else{ return result.concat(year.toString).concat("|\n|_D|_L|_K|_M|_J|_V|_S|\n")}
}

//Algorimo para generar el encabezado de un calendario
def generarEncabezado(month:Int, year:Int):String ={ return " ____________________\n|".concat(monthAndYear(year, (mesNumToString(month))))} 

def inicialesVacias(cantidad:Int, resultado:String):String ={ //Algoritmo que genera la cantidad de casillas vacias iniciales
    if(cantidad == 0) {return resultado} 
    else{ inicialesVacias((cantidad - 1), (resultado.concat("|__")))}
}


def generarCuerpo(fecha:Int, maximo:Int, posicion:Int, resultado:String):String = { //Funcion para crear un calendario recursivamente
    if((fecha > maximo) &&  (posicion > 7)) {return resultado.concat("|")}
    else if(fecha > maximo) {return generarCuerpo(fecha,maximo, (posicion + 1), (resultado.concat("|__")))}
    else if ((posicion > 7) && (fecha > 10)) {return generarCuerpo((fecha + 1),maximo, 2, (resultado.concat("|\n|").concat(fecha.toString)))}
    else if ((posicion > 7) && (fecha < 10)) {return generarCuerpo((fecha + 1),maximo, 2, (resultado.concat("|\n|_").concat(fecha.toString)))}
    else if(fecha < 10) {return generarCuerpo((fecha + 1), maximo, (posicion + 1), (resultado.concat("|_").concat(fecha.toString)))}
    else{return generarCuerpo((fecha + 1), maximo, (posicion + 1), (resultado.concat("|").concat(fecha.toString)))}
}

//Funcion que ensambla un calendario y todas sus partes
def crearCalendario(month:Int, year:Int):String = {generarEncabezado(month,year).concat(inicialesVacias(((getPosicionSemana(month,year))-1), "")).concat(generarCuerpo(1,(getCandidadDiasMes(month, year)), (getPosicionSemana(month, year)), ""))}

//Funcion que genera un calendario con un anho y un mes en particular y lo guarda en un archivo txt
def generarCalendario(month:Int, year:Int) = {
    var writer = new PrintWriter((mesNumToString(month)).concat(year.toString).concat(".txt"))
    writer.write(crearCalendario(month,year))
    writer.close()
}

//b Averiguar un dia de la semana particular


def zeller(day:Int, month:Int, year:Int):Int ={return (day + (13 * (month + 1)/5) + (year % 100) + ((year % 100)/4) + ((year / 100) / 4) - (2 * (year / 100))) % 7}
//Algorimo de zeller para calcular que dia de la semana cae una fecha determinada

def averiguarDia (day:Int, month:Int, year:Int):String ={ //Determina el dia de la semana a partir de una fecha
    if(!(fechaValida(day,month,year))){return "Error de fecha"}
    else if(month == 1) {return diaNumToString(zeller(13, month ,(year - 1)))}
    else if(month == 2) {return diaNumToString(zeller(14, month, (year -1)))}
    else{diaNumToString(zeller(day,month,year))}
	}

//c Restar dos fechas y dar la respuesta en dias, meses, anhos o dias meses y anhos


def diferenciaYearsAux(day1:Int, month1:Int, year1:Int, day2:Int, month2:Int, year2:Int):Int = {  //Funcion auxiliar recursiva para obtener la diferencia en anhos entre dos fechas
    if((year1 == (year2-1)) && ((month1 < month2) || (month1 == month2) && (day1 <= day2))){return 1}
    else if(year1 < (year2 -1)) { return 1 + (diferenciaYearsAux (day1,month1,year1,day2,month2,(year2 - 1)))}
    else{return 0}
}
def diferenciaYears(day1:Int, month1:Int, year1:Int, day2:Int, month2:Int, year2:Int):Int = { //Diferencia en anhos entre dos fechas
    if(!(fechaValida(day1, month1, year1)) || (!(fechaValida(day2,month2,year2)))){return 0}
    else if(year2 > year1){return diferenciaYearsAux(day1,month1,year1,day2,month2,year2)}
    else if(year1 > year2) {return diferenciaYearsAux(day2,month2,year2,day1,month1,year1)}
    else{return 0}
	}

def diferenciaMonthsAux(day1:Int, month1:Int, year1:Int, day2:Int, month2:Int, year2:Int):Int ={ //Funcion auxiliar recursiva para obtener la diferencia en meses entre dos fechas

    if(year1 < (year2 - 1)){return  diferenciaMonthsAux(day1, month1, year1, day2, month2, (year2 - (diferenciaYearsAux(day1, month1, year1, day2, month2, year2)))) + ((diferenciaYearsAux(day1, month1, year1, day2, month2, year2 )* 12))}
    else if(year1 == (year2 - 1)){ return month2 + (diferenciaMonthsAux(day1,month1,year1,day2,12,(year2-1)))}
    else if ((month1 < (month2 - 1))|| ((month1 == (month2 -1)) && (day1 < day2))) {return 1 + (diferenciaMonthsAux(day1,month1,year1,day2,(month2 -1),year2))}
    else{return 0}
	}


def diferenciaMonths(day1:Int, month1:Int, year1:Int, day2:Int, month2:Int, year2:Int):Int ={   //Diferencia en meses entre dos fechas
    if(!(fechaValida(day1, month1, year1)) || (!(fechaValida(day2,month2,year2)))){return 0}
    else if(year2 > year1){return diferenciaMonthsAux(day1,month1,year1,day2,month2,year2)}
    else if(year1 > year2) {return diferenciaMonthsAux(day2,month2,year2,day1,month1,year1)}
    else if (month2 > month1){return diferenciaMonthsAux(day1,month1,year1,day2,month2,year2)}
	else if (month1 > month2){return diferenciaMonthsAux(day2,month2,year2,day1,month1,year1)}
    else{return 0}
	}



//Convierte una fecha a un unico numero entero
def convertirFechaANumero(day:Int, month:Int, year:Int):Int={return 365*(year - ((month + 9)% 12)/10) + (year - ((month + 9)% 12)/10)/4 - (year - ((month + 9)% 12)/10)/100 + (year - ((month + 9)% 12)/10)/400 + (((month + 9) % 12)*306 + 5)/10 + (day - 1)}

 //Encuentra la diferencia en dias entre dos fechas
def diferenciaDays(day1:Int, month1:Int, year1:Int, day2:Int, month2:Int, year2:Int):Int ={
    if(!(fechaValida(day1, month1, year1)) || (!(fechaValida(day2,month2,year2)))){return 0}
    else{ return Math.abs((convertirFechaANumero(day1,month1,year1))-(convertirFechaANumero(day2,month2,year2)))}
	}

def diferenciaExactaAux(day1:Int, month1:Int, year1:Int, day2:Int, month2:Int, year2:Int):String ={ //Funcion auxiliar recursiva para obtener la diferencia en anhos meses y dias entre dos fechas
    if (month2 > month1) {return "La diferencia entre las fechas es de ".concat((diferenciaYears (day1,month1,year1,day2,month2,year2)).toString).concat(" anho(s) ").concat((diferenciaMonths(day1,month1,year1,day2,month2,year1)).toString).concat(" mes(es) y ").concat((Math.abs(day1 - day2)).toString).concat(" dia(s).")}
    else if((month1 == month2) && (day1 > day2)) {return "La diferencia entre las fechas es de ".concat((diferenciaYears(day1, month1, year1, day2, month2, year2)).toString).concat(" anho(s) ").concat((diferenciaMonths(day1, month1, year1,day2, month2, (year1 + 1))).toString).concat(" mes(es) y ").concat((diferenciaDays(day1, (month1 -1), year1, day2, month2, year1)).toString).concat(" dia(s).")}
    else{ return "La diferencia entre las fechas es de ".concat((diferenciaYears(day1,month1,year1,day2,month2,year2)).toString).concat(" anho(s) ").concat((diferenciaMonths(day1,month1,year1,day2,month2,year1)).toString).concat(" mes(es) y ").concat((Math.abs(day1 - day2)).toString).concat(" dia(s).")}
}
def diferenciaExacta(day1:Int, month1:Int, year1:Int, day2:Int, month2:Int, year2:Int):String ={ //Encuentra la diferencia en dias meses y anhos de dos fechas dadas
    if((convertirFechaANumero(day1,month1,year1)) < (convertirFechaANumero(day2,month2,year2))) {return diferenciaExactaAux(day1,month1,year1,day2,month2,year2)}
    else if((convertirFechaANumero(day1,month1,year1)) > (convertirFechaANumero(day2,month2,year2))){return diferenciaExactaAux(day2,month2,year2,day1,month1,year1)}
    else{return "Son la misma fecha."}
}

//d Obtener una fecha normal a partir de una fecha base

def getCandidadDiasMes(mes:Int, year:Int):Int = mes match{ //Obtiene la cantidad de meses de un anho
    case 2 if((year % 4) == 0) => 29
    case 2 => 28
    case _ if((mes < 8)&&((mes % 2)) == 0) => 30
    case _ if(mes < 8) => 31
    case _ if((mes >= 8) && ((mes % 2) == 0)) => 31
    case _ if(mes > 8) => 30
    case _ => 31
}

def fechaNormalAFechaBase(day1:Int, month1:Int, year1:Int, month2:Int, year2:Int):String ={ //Convierte de una fecha normal a una fecha basada en un mes y un anho
    if((month2 > 12) || (month2 < 1) || !(fechaValida(day1,month1,year1))) { return "Fecha invalida"}
    else{ return "La fecha basada es: ".concat(((getCandidadDiasMes(month2,year2)) + (diferenciaDays(day1, month1, year1, (getCandidadDiasMes(month2, year2)), month2, year2))).toString).concat("/").concat(month2.toString).concat("/").concat(year2.toString)}
}

def fechaBaseAFechaNormalAux(day:Int,month:Int, year:Int, rday:Int,rmonth:Int, ryear:Int):String ={//Funcion auxiliar recursiva que convierte una fecha basada a una fecha normal
    if(day <= (getCandidadDiasMes(month, year))) {return  "La fecha normal es: ".concat(rday.toString).concat("/").concat(rmonth.toString).concat("/").concat(ryear.toString)}
    else if(fechaValida((rday + 1), rmonth, ryear)) {return fechaBaseAFechaNormalAux((day - 1), month, year, (rday + 1), rmonth, ryear)}
    else if(fechaValida(1, (rmonth + 1), ryear)) {return fechaBaseAFechaNormalAux((day - 1),month,year, 1, (rmonth + 1),ryear)}
    else { return fechaBaseAFechaNormalAux((day - 1), month, year, 1,  1, (ryear + 1))}
 }

def fechaBaseAFechaNormal(day:Int, month:Int, year:Int):String ={ //Convierte una fecha basada en un mes y un anho a una fecha normal
    if(fechaValida(day, month, year)) {return "La fecha normal es: ".concat(day.toString).concat("/").concat(month.toString).concat("/").concat(year.toString)}
    else{ return fechaBaseAFechaNormalAux(day, month, year, (getCandidadDiasMes(month, year)), month, year)}
}


//e Obtener la edad de una persona a partir de una fecha
val cal = Calendar.getInstance()
//Obtener el anho actual
def getYearActual():Int = {return cal.get(Calendar.YEAR)}

 //Obtener el mes actual
def getMonthActual():Int = {return cal.get(Calendar.MONTH) + 1}

//Obtener el dia actual
def getDayActual():Int = {return cal.get(Calendar.DATE)}

 //Funcion que encuentra la edad en dia meses y anhos de una  persona

def edadExacta(day:Int, month:Int, year:Int):String= {return diferenciaExacta(day, month, year, getDayActual(), getMonthActual(), getYearActual())}

//Funcion que encuentra la edad en dias de una persona con su fecha de nacimiento

def edadDays(day:Int, month:Int, year:Int):Int = {return diferenciaDays(day, month, year, getDayActual(), getMonthActual(), getYearActual())}

 //Funcion que encuentra la edad en meses de una persona con su fecha de nacimiento

def edadMonths(day:Int, month:Int, year:Int):Int = {return diferenciaMonths(day, month, year, getDayActual(), getMonthActual(), getYearActual())}

//Funcion que encuentra la edad en anhos de una persona con su fecha de nacimiento

def edadYears(day:Int, month:Int, year:Int):Int = {return diferenciaYears(day, month, year, getDayActual(), getMonthActual(), getYearActual())}
