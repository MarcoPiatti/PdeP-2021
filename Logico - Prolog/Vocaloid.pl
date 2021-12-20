% A)
sabeCantar(megurineLuka, cancion(nightFever, 4)).
sabeCantar(megurineLuka, cancion(foreverYoung, 5)).

sabeCantar(hatsuneMiku, cancion(tellYourWorld, 4)).

sabeCantar(gumi, cancion(foreverYoung, 4)).
sabeCantar(gumi, cancion(foreverYoung, 5)).

sabeCantar(seeU, cancion(novemberRain, 6)).
sabeCantar(seeU, cancion(nightFever, 5)).

%%%%%%%%%%%%%%%%%%%%%%% AUXILIARES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sabeAlMenosNCanciones(Vocaloid, CancionesMinimas):-
    cantidadDeCanciones(Vocaloid, Cantidad),
    Cantidad >= CancionesMinimas.

cantidadDeCanciones(Vocaloid, Cantidad):-
    findall(Cancion, sabeCantar(Vocaloid, cancion(Cancion, _)), ListaCanciones),
    length(ListaCanciones, Cantidad).

duracionTotalDeCancionesMenorA(Vocaloid, TiempoMaximo):-
    tiempoTotalDeCanciones(Vocaloid, TiempoTotal),
    TiempoTotal < TiempoMaximo.

duracionTotalDeCancionesMayorA(Vocaloid, TiempoMinimo):-
    tiempoTotalDeCanciones(Vocaloid, TiempoTotal),
    TiempoTotal > TiempoMinimo.

tiempoTotalDeCanciones(Vocaloid, TiempoTotal):-
    findall(TiempoCancion, sabeCantar(Vocaloid, cancion(_, TiempoCancion)), ListaTiempos),
    sum_list(ListaTiempos, TiempoTotal).

algunaCancionDuraMasQue(Vocaloid, TiempoMinimo):-
    sabeCantar(Vocaloid, cancion(_, Duracion)),
    Duracion > TiempoMinimo.
%%%%%%%%%%%%%%%%%%%%%%% AUXILIARES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1)
esNovedoso(Vocaloid):-
    sabeAlMenosNCanciones(Vocaloid, 2),
    duracionTotalDeCancionesMenorA(Vocaloid, 15).

% 2)
esAcelerado(Vocaloid):-
    sabeCantar(Vocaloid, _),
    not((sabeCantar(Vocaloid, cancion(_, Duracion)), Duracion > 4)).

% 1)
concierto(mikuExpo, estadosUnidos, 2000, gigante(3, 5)).
concierto(magicalMirai, japon, 3000, gigante(4, 9)).
concierto(vocalektVisions, estadosUnidos, 1000, mediano(10)).
concierto(mikuFest, argentina, 100, pequenio(4)).

% 2)
puedeParticipar(hatsuneMiku, concierto(_, _, _, _)).

puedeParticipar(Vocaloid, Concierto):-
    sabeCantar(Vocaloid, _),
    concierto(Concierto, _, _, Tipo),
    cumpleConTipoDeConcierto(Vocaloid, Tipo).

cumpleConTipoDeConcierto(Vocaloid, gigante(CancionesMinimas, TiempoMinimo)):-
    sabeAlMenosNCanciones(Vocaloid, CancionesMinimas),
    duracionTotalDeCancionesMayorA(Vocaloid, TiempoMinimo).

cumpleConTipoDeConcierto(Vocaloid, mediano(TiempoMaximo)):-
    duracionTotalDeCancionesMenorA(Vocaloid, TiempoMaximo).

cumpleConTipoDeConcierto(Vocaloid, pequenio(TiempoMinimo)):-
    algunaCancionDuraMasQue(Vocaloid, TiempoMinimo).

% 3)
vocaloidMasFamoso(Vocaloid):-
    sabeCantar(Vocaloid, _),
    forall((sabeCantar(OtroVocaloid, _), OtroVocaloid \= Vocaloid), tieneMasFama(Vocaloid, OtroVocaloid)).

tieneMasFama(UnVocaloid, OtroVocaloid):-
    nivelDeFama(UnVocaloid, UnaFama),
    nivelDeFama(OtroVocaloid, OtraFama),
    UnaFama > OtraFama.

nivelDeFama(Vocaloid, FamaTotal):-
    sabeCantar(Vocaloid, _),
    famaDeConciertos(Vocaloid, FamaDeConciertos),
    cantidadDeCanciones(Vocaloid, Canciones),
    FamaTotal is FamaDeConciertos * Canciones.

famaDeConciertos(Vocaloid, FamaDeConciertos):-
    findall(Fama, (puedeParticipar(Vocaloid, Concierto), concierto(Concierto, _, Fama, _)), ListaFamas),
    sum_list(ListaFamas, FamaDeConciertos).

conoceA(megurineLuka, hatsuneMiku).
conoceA(megurineLuka, gumi).
conoceA(gumi, seeU).
conoceA(seeU, kaito).

unicoParticipante(Vocaloid, Concierto):-
    puedeParticipar(Vocaloid, Concierto),
    forall((puedeParticipar(OtroVocaloid, Concierto), OtroVocaloid \= Vocaloid), not(conoceEnGeneralA(Vocaloid, OtroVocaloid))).

conoceIndirectamenteA(Vocaloid, OtroVocaloid):-
    conoceA(Vocaloid, VocaloidIntermedio),
    conoceEnGeneralA(VocaloidIntermedio, OtroVocaloid).

conoceEnGeneralA(Vocaloid, OtroVocaloid):-
    conoceA(Vocaloid, OtroVocaloid).

conoceEnGeneralA(Vocaloid, OtroVocaloid):-
    conoceIndirectamenteA(Vocaloid, OtroVocaloid).