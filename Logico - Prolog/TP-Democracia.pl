%TP INTEGRADOR
%https://docs.google.com/document/d/1bmljMbzGNfL5GyO_UC4QgqwcB-GfUAecW_ifXO36ltM/edit

%candidatoDe(CANDIDATO,PARTIDO_DEL_CANDIDATO)
candidatoDe(frank,rojo).
candidatoDe(claire,rojo).
candidatoDe(garrett,azul).
candidatoDe(linda,azul).
candidatoDe(catherine,rojo).
candidatoDe(jackie,amarillo).
candidatoDe(seth,amarillo).
candidatoDe(heather,amarillo).

%edad(PERSONA, EDAD_PERSONA).
edad(frank, 50).
edad(claire, 52).
edad(garrett, 64).
edad(peter, 26).
edad(jackie, 38).
edad(linda, 30).
edad(catherine, 59).
edad(heather , 52). % considerando que ya los cumplio , sino serian 51


%partidoSePostulaEn(COLORPARTIDO,PROVINCIA).
partidoSePostulaEn(azul,buenosAires).
partidoSePostulaEn(azul,chaco).
partidoSePostulaEn(azul,tierraDelFuego).
partidoSePostulaEn(azul,sanLuis).
partidoSePostulaEn(azul,neuquen).

partidoSePostulaEn(rojo,buenosAires).
partidoSePostulaEn(rojo,santaFe).
partidoSePostulaEn(rojo,cordoba).
partidoSePostulaEn(rojo,chubut).
partidoSePostulaEn(rojo,tierraDelFuego).
partidoSePostulaEn(rojo,sanLuis).

partidoSePostulaEn(amarillo,chaco).
partidoSePostulaEn(amarillo,formosa).
partidoSePostulaEn(amarillo,tucuman).
partidoSePostulaEn(amarillo,salta).
partidoSePostulaEn(amarillo,santaCruz).
partidoSePostulaEn(amarillo,laPampa).
partidoSePostulaEn(amarillo,corrientes).
partidoSePostulaEn(amarillo,misiones).
partidoSePostulaEn(amarillo,buenosAires).

%cantidadHabitantesProvincia(PROVINCIA,CANTIDAD HABITANTES).
cantidadHabitantesProvincia(buenosAires,15355000).
cantidadHabitantesProvincia(chaco,1143201).
cantidadHabitantesProvincia(tierraDelFuego,160720).
cantidadHabitantesProvincia(sanLuis,489255).
cantidadHabitantesProvincia(neuquen,637913).
cantidadHabitantesProvincia(santaFe,3397532).
cantidadHabitantesProvincia(cordoba,3567654).
cantidadHabitantesProvincia(chubut,577466).
cantidadHabitantesProvincia(formosa,527895).
cantidadHabitantesProvincia(tucuman,1687305).
cantidadHabitantesProvincia(salta,1333365).
cantidadHabitantesProvincia(santaCruz,273964).
cantidadHabitantesProvincia(laPampa,349299).
cantidadHabitantesProvincia(corrientes,992595).
cantidadHabitantesProvincia(misiones,1189446).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*     

JUSTIFICACION DE POR QUE NO SE COLOCAN CIERTOS DATOS:
Por universo cerrado, todo dato considerado "falso" no figura en la base de conocimientos. Por ejemplo:
-El partido violeta no tiene candidatos.
-Peter no es candidato del partido Amarillo...
-El partido rojo ... Finalmente no se presentará en Formosa.


2) Provincia picante
Queremos saber si una provincia esPicante/1. Esto se da cuando al menos dos partidos presentan candidatos 
en dicha provincia y además esta tiene más de 1 millón de habitantes.

-Buenos Aires es picante por tener 15 millones de habitantes y los partidos azul, rojo y amarillo presentan candidatos.
-Santa fé no es picante, tiene 1 millón de habitantes pero solo un partido presenta candidatos      
-San Luis no es picante. Varios presentan candidatos pero tiene 400 mil habitantes. 

*/

tieneMasDeMillonDeHabitantes(Provincia):-                         
    cantidadHabitantesProvincia(Provincia,CantidadHabitantes),
    CantidadHabitantes>1000000.

esPicante(Provincia):-                                                   % FUNCIONA ES PICANTE                   
    tieneMasDeMillonDeHabitantes(Provincia),
    partidoSePostulaEn(PartidoGanador,Provincia),       
    partidoSePostulaEn(PartidoPerdedor,Provincia),
    PartidoGanador \= PartidoPerdedor.

% intencionDeVotoEn(Provincia, Partido, Porcentaje)
intencionDeVotoEn(buenosAires, rojo, 40).
intencionDeVotoEn(buenosAires, azul, 30).
intencionDeVotoEn(buenosAires, amarillo, 30).

intencionDeVotoEn(chaco, rojo, 50).
intencionDeVotoEn(chaco, azul, 20).
intencionDeVotoEn(chaco, amarillo, 0).

intencionDeVotoEn(tierraDelFuego, rojo, 40).
intencionDeVotoEn(tierraDelFuego, azul, 20).
intencionDeVotoEn(tierraDelFuego, amarillo, 10).

intencionDeVotoEn(sanLuis, rojo, 50).
intencionDeVotoEn(sanLuis, azul, 20).
intencionDeVotoEn(sanLuis, amarillo, 0).

intencionDeVotoEn(neuquen, rojo, 80).
intencionDeVotoEn(neuquen, azul, 10).
intencionDeVotoEn(neuquen, amarillo, 0).

intencionDeVotoEn(santaFe, rojo, 20).
intencionDeVotoEn(santaFe, azul, 40).
intencionDeVotoEn(santaFe, amarillo, 40).

intencionDeVotoEn(cordoba, rojo, 10).
intencionDeVotoEn(cordoba, azul, 60).
intencionDeVotoEn(cordoba, amarillo, 20).

intencionDeVotoEn(chubut, rojo, 15).
intencionDeVotoEn(chubut, azul, 15).
intencionDeVotoEn(chubut, amarillo, 15).

intencionDeVotoEn(formosa, rojo, 0).
intencionDeVotoEn(formosa, azul, 0).
intencionDeVotoEn(formosa, amarillo, 0).

intencionDeVotoEn(tucuman, rojo, 40).
intencionDeVotoEn(tucuman, azul, 40).
intencionDeVotoEn(tucuman, amarillo, 20).

intencionDeVotoEn(salta, rojo, 30).
intencionDeVotoEn(salta, azul, 60).
intencionDeVotoEn(salta, amarillo, 10).

intencionDeVotoEn(santaCruz, rojo, 10).
intencionDeVotoEn(santaCruz, azul, 20).
intencionDeVotoEn(santaCruz, amarillo, 30).

intencionDeVotoEn(laPampa, rojo, 25).
intencionDeVotoEn(laPampa, azul, 25).
intencionDeVotoEn(laPampa, amarillo, 40).

intencionDeVotoEn(corrientes, rojo, 30).
intencionDeVotoEn(corrientes, azul, 30).
intencionDeVotoEn(corrientes, amarillo, 10).

intencionDeVotoEn(misiones, rojo, 90).
intencionDeVotoEn(misiones, azul, 0).
intencionDeVotoEn(misiones, amarillo, 0).
 
/* 
Modelar el predicado leGanaA/3 el cual relaciona a dos candidatos y una provincia, y nos dice si un candidato 
le ganaría a otro en una provincia. Para ello:

El partido del ganador debe competir en la provincia. 
Si el partido del perdedor también compite en la provincia, se evalúa el que tenga el mayor porcentaje de 
votos en la provincia. Si hay empate, no se cumple la relación.

Si ambos candidatos pertenecen al mismo partido, la relación se cumple si el partido compite en la provincia. 
*/

%leGanaA(CandidatoGanador,CandidatoPerdedor,Provincia)
leGanaA(CandidatoGanador,CandidatoPerdedor,Provincia):-
    candidatoDe(CandidatoGanador,PartidoGanador),     
    candidatoDe(CandidatoPerdedor,PartidoPerdedor),
    partidoLeGanaA(PartidoGanador, PartidoPerdedor, Provincia).

partidoLeGanaA(PartidoGanador, PartidoPerdedor, Provincia):-
    ambosSePostulanEn(PartidoGanador, PartidoPerdedor, Provincia),
    tieneMasVotosQue(PartidoGanador,PartidoPerdedor,Provincia).

partidoLeGanaA(PartidoGanador, PartidoGanador, Provincia):-
    partidoSePostulaEn(PartidoGanador,Provincia).

partidoLeGanaA(PartidoGanador, PartidoPerdedor, Provincia):-
    unoSePostulaYelOtroNo(PartidoGanador, PartidoPerdedor, Provincia).

unoSePostulaYelOtroNo(PartidoGanador, PartidoPerdedor, Provincia):-
    partidoSePostulaEn(PartidoGanador,Provincia),
    not(partidoSePostulaEn(PartidoPerdedor,Provincia)).

tieneMasVotosQue(PartidoGanador,PartidoPerdedor,Provincia):-
    intencionDeVotoEn(Provincia, PartidoGanador, PuntajeGanador),
    intencionDeVotoEn(Provincia, PartidoPerdedor, PuntajePerdedor),
    PuntajeGanador>PuntajePerdedor.
    




%%%%%%%%%%%%%% version sin delegar que funciona!!!! %%%%%%%%%%%%%%%%%%%%%%% NO TOCAR , NO ROMPER

/* 
4) Se pide realizar elGranCandidato/1. Un candidato es el gran candidato si se cumple:

Para todas las provincias en las cuales su partido compite, el mismo gana.
Es el candidato más joven de su partido

El único gran candidato es Frank. ¿Cómo podemos estar seguros de esto? ¿Qué tipo de consulta deberíamos realizar? ¿Con qué concepto está relacionado?
*/

elGranCandidato(Candidato):-
    suPartidoGanaTodo(Candidato),
    esElCandidatoMasJovenDeSuPartido(Candidato).

suPartidoGanaTodo(Candidato):-
    candidatoDe(Candidato, Partido),
    forall(ambosSePostulanEn(Partido, OtroPartido, Provincia), tieneMasVotosQue(Partido, OtroPartido, Provincia)).

ambosSePostulanEn(Partido, OtroPartido, Provincia):-
    partidoSePostulaEn(Partido, Provincia),
    partidoSePostulaEn(OtroPartido, Provincia),
    Partido \= OtroPartido.

esElCandidatoMasJovenDeSuPartido(CandidatoMasJoven):-
    candidatoDe(CandidatoMasJoven, _),
    forall(sonDiferentesCandidatosDeMismoPartido(CandidatoMasJoven,OtroCandidato), esMasJoven(CandidatoMasJoven, OtroCandidato)).
    
sonDiferentesCandidatosDeMismoPartido(UnCandidato, OtroCandidato):-
    candidatoDe(UnCandidato, Partido),
    candidatoDe(OtroCandidato, Partido),
    UnCandidato \= OtroCandidato.

esMasJoven(CandidatoMasJoven, OtroCandidato):-
    edad(CandidatoMasJoven,EdadCandidatoMasJoven),
    edad(OtroCandidato, EdadOtroCandidato),
    EdadCandidatoMasJoven<EdadOtroCandidato.

/*  Estamos seguros que es solo frank porque la consulta
    "?- elGranCandidato(Alguien)." retorna solamente a frank.
    Esto es gracias a que la funcion es inversible.


5 Malas consultoras

La consultora cometió un error al pasarnos los resultados de las encuestas. Para eso, realizaremos el predicado ajusteConsultora/3, el cual relaciona un partido, una provincia y el verdadero porcentaje de votos, los cuales se ajustarán de la siguiente manera:
Si el partido, según la intención de voto, ganaba en la provincia, se le resta 20%.
En otro caso, se le suma 5%

Por ejemplo
La intención de voto del partido rojo en Buenos Aires quedaría en 20
La intención de voto del partido azul en Neuquén quedaría en 15

Si ahora quisiéramos evaluar todos los predicados con los valores reales de votos, ¿Qué cambios deberíamos hacer? ¿Cuántos predicados deberíamos modificar?
*/

ajusteConsultora(Partido, Provincia, VotosReales):-
    intencionDeVotoEn(Provincia, Partido, Votos),
    calculoVotos(Partido, Provincia, VotosReales, Votos).

calculoVotos(Partido, Provincia, VotosReales, Votos):-
    not(ganaEnLaProvincia(Partido, Provincia)),
    VotosReales is Votos + 5.
calculoVotos(Partido, Provincia, VotosReales, Votos):-
    ganaEnLaProvincia(Partido, Provincia),
    VotosReales is Votos - 20.

ganaEnLaProvincia(Partido, Provincia):-
    intencionDeVotoEn(Provincia, Partido, _),
    forall(ambosTienenIntencionDeVotoEn(Partido, OtroPartido, Provincia), tieneMasVotosQue(Partido, OtroPartido, Provincia)).

ambosTienenIntencionDeVotoEn(Partido, OtroPartido, Provincia):-
    intencionDeVotoEn(Provincia, Partido, _),
    intencionDeVotoEn(Provincia, OtroPartido, _),
    Partido \= OtroPartido.

/* Habria que cambiar solamente a intencionDeVotoEn para reflejar los nuevos porcentajes de votos */

%%%%%%%%%%%%%%%%%%%%%%%%%% punto 6 %%%%%%%%%%%%%%%%%%%%%%%%%%
promete(azul, construir([edilicio(hospital, 1000), edilicio(jardin, 100), edilicio(escuela, 5)])).
promete(amarillo, construir([edilicio(hospital, 100), edilicio(universidad, 1), edilicio(comisaria, 200)])).
promete(rojo, nuevosPuestosDeTrabajo(800000)).
promete(amarillo, nuevosPuestosDeTrabajo(10000)).
promete(azul, inflacion(2, 4)).
promete(amarillo, inflacion(1, 15)).
promete(rojo, inflacion(10, 30)).
%%%%%%%%%%%%%%%%%%%%%%%%%% punto 6 %%%%%%%%%%%%%%%%%%%%%%%%%%

influenciaDePromesas(inflacion(CotaInferior, CotaSuperior), VariacionDeIntencion):-
    VariacionDeIntencion is -(CotaInferior + CotaSuperior) // 2.

influenciaDePromesas(nuevosPuestosDeTrabajo(Cantidad), 3):-
    Cantidad >= 50000.

influenciaDePromesas(construir(Edilicios), VariacionDeIntencion):-
    findall(VariacionesParciales, variacionParcial(Edilicios, VariacionesParciales), ListaVariacionesParciales),
    sum_list(ListaVariacionesParciales, VariacionDeIntencion).

variacionParcial(Edilicios, 2):-
    member(edilicio(hospital, _), Edilicios).

variacionParcial(Edilicios, 2):-
    member(edilicio(comisaria,200), Edilicios).

variacionParcial(Edilicios, VariacionParcial):-
    cantidadDeJardinesYEscuelas(Edilicios, Cantidad),
    VariacionParcial is (0.1) * Cantidad.

variacionParcial(Edilicios, VariacionParcial):-
    cantidadDeEdiliciosInnecesarios(Edilicios, Cantidad),
    VariacionParcial is (-1) * Cantidad.

cantidadDeJardinesYEscuelas(Edilicios, Cantidad):-
    member(edilicio(jardin, CantidadJardines), Edilicios),
    member(edilicio(escuela, CantidadEscuelas), Edilicios),
    Cantidad is CantidadJardines + CantidadEscuelas.

cantidadDeEdiliciosInnecesarios(Edilicios, Cantidad):-
    findall(CantidadDeUnInnecesario, cantidadDeUnInnecesario(Edilicios, CantidadDeUnInnecesario), ListaCantidadInnecesarios),
    sum_list(ListaCantidadInnecesarios, Cantidad).

cantidadDeUnInnecesario(Edilicios, CantidadDeUnInnecesario):-
    member(edilicio(Edilicio, CantidadDeUnInnecesario), Edilicios),
    not(esNecesario(edilicio(Edilicio, CantidadDeUnInnecesario))).

esNecesario(edilicio(hospital,_)).
esNecesario(edilicio(comisaria,_)).
esNecesario(edilicio(universidad,_)).
esNecesario(edilicio(jardin,_)).
esNecesario(edilicio(escuela,_)).

/* 8)
A partir de sus promesas, queremos realizar el predicado promedioDeCrecimiento/2 en donde
relacionamos un partido con la sumatoria de crecimiento brindado por cada promesa. 
*/

promedioDeCrecimiento(UnPartido, SumatoriaDeCrecimiento):-
    promete(UnPartido, _),
    findall(CrecimientoParcial, influenciaDePromesaDePartido(UnPartido, CrecimientoParcial), ListaCrecimientosParciales),
    sum_list(ListaCrecimientosParciales, SumatoriaDeCrecimiento).

influenciaDePromesaDePartido(UnPartido, VariacionDeIntencion):-
    promete(UnPartido, UnaPromesa),
    influenciaDePromesas(UnaPromesa, VariacionDeIntencion).
