%quedaEn(Boliche, Localidad)
quedaEn(pachuli, generalLasHeras).
quedaEn(why, generalLasHeras).
quedaEn(chaplin, generalLasHeras).
quedaEn(masDe40, sanLuis).
quedaEn(qma, caba).

%entran(Boliche, CapacidadDePersonas)
entran(pachuli, 500).
entran(why, 1000).
entran(chaplin, 700).
entran(masDe40, 1200).
entran(qma, 800).

%sirveComida(Boliche)
sirveComida(chaplin).
sirveComida(qma).

%Functores
%tematico(tematica)
%cachengue(listaDeCancionesHabituales)
%electronico(djDeLaCasa, horaQueEmpieza, horaQueTermina)

%esDeTipo(Boliche, Tipo)
esDeTipo(why, cachengue([elYYo, prrrram, biodiesel, buenComportamiento])).
esDeTipo(masDe40, tematico(ochentoso)).
esDeTipo(qma, electronico(djFenich, 2, 5)).

% 1)
esPiola(Boliche):-
    sirveComida(Boliche),
    esMedioPiola(Boliche).

esMedioPiola(Boliche):-
    quedaEn(Boliche, generalLasHeras).

esMedioPiola(Boliche):-
    esGrande(Boliche).

esGrande(Boliche):-
    entran(Boliche, Gente),
    Gente > 700.

% 2)
soloParaBailar(Boliche):-
    quedaEn(Boliche, _),
    not(sirveComida(Boliche)).

% 3)
podemosIrConEsa(Localidad):-
    quedaEn(_, Localidad),
    forall(quedaEn(Boliche, Localidad), esPiola(Boliche)).

% 4)
puntaje(Boliche, Puntaje):-
    esDeTipo(Boliche, Tipo),
    puntajeTipo(Tipo, Puntaje).

puntajeTipo(tematico(ochentoso), 9).

puntajeTipo(tematico(Tematica), 7):-
    Tematica \= ochentoso.

puntajeTipo(electronico(_, HoraInicio, HoraFin), Puntaje):-
    Puntaje is HoraInicio + HoraFin.

puntajeTipo(cachengue(Canciones), 10):-
    member(biodiesel, Canciones),
    member(buenComportamiento, Canciones).

% 5)  
elMasGrande(Boliche, Localidad):-
    quedaEn(Boliche, Localidad),
    forall((quedaEn(OtroBoliche, Localidad), OtroBoliche \= Boliche), tieneMasCapacidad(Boliche, OtroBoliche)).

tieneMasCapacidad(BolicheGrande, BolicheChico):-
    entran(BolicheGrande, CantidadBolicheGrande),
    entran(BolicheChico, CantidadBolicheChico),
    CantidadBolicheGrande > CantidadBolicheChico.

% 6)
puedeAbastecer(Localidad, Cantidad):-
    quedaEn(_, Localidad),
    findall(Cantidad, (quedaEn(Boliche, Localidad), entran(Boliche, Cantidad)), ListaCantidades),
    sum_list(ListaCantidades, CantidadTotal),
    CantidadTotal >= Cantidad.

% 7)
quedaEn(trabajamosYNosDivertimos, concordia).
entran(trabajamosYNosDivertimos, 500).
esDeTipo(trabajamosYNosDivertimos, tematico(oficina)).
sirveComida(trabajamosYNosDivertimos).

quedaEn(elFinDelMundo, ushuaia).
entran(elFinDelMundo, 1500).
esDeTipo(elFinDelMundo, electronico(djLuis, 0, 6)).

entran(misterio, 1000000).
sirveComida(misterio).