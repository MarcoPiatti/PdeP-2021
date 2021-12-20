atiende(dodain, lunes, rango(9, 15)).
atiende(dodain, miercoles, rango(9, 15)).
atiende(dodain, viernes, rango(9, 15)).
atiende(lucas, martes, rango(10, 20)).
atiende(juanC, sabado, rango(18, 22)).
atiende(juanC, domingo, rango(18, 22)).
atiende(juanFdS, jueves, rango(10, 20)).
atiende(juanFdS, viernes, rango(12, 20)).
atiende(leoC, lunes, rango(14, 18)).
atiende(leoC, miercoles, rango(14, 18)).
atiende(martu, miercoles, rango(23, 24)).

% 1)
atiende(vale, Dia, RangoHorario):-
    atiende(dodain, Dia, RangoHorario).

atiende(vale, Dia, RangoHorario):-
    atiende(juanC, Dia, RangoHorario).

% Si nadie hace el mismo horario que leoC no es necesario agregarlo, universo cerrado
% Si maiu esta pensando en hacerlo, no es un hecho, no lo podemos agregar.

% 2)
atiendePuntual(Persona, Dia, Hora):-
    atiende(Persona, Dia, rango(HoraEntrada, HoraSalida)),
    between(HoraEntrada, HoraSalida, Hora).

% 3)
foreverAlone(Persona, Dia, Hora):-
    atiendePuntual(Persona, Dia, Hora),
    not(atiendenJuntos(Persona, _, Dia, Hora)).

atiendenJuntos(UnaPersona, OtraPersona, Dia, Hora):-
    atiendePuntual(UnaPersona, Dia, Hora),
    atiendePuntual(OtraPersona, Dia, Hora),
    UnaPersona \= OtraPersona.

% 4)
posibilidadDeAtencion(Dia, PosibleGrupo):-
    findall(Persona, atiende(Persona, Dia, _), PersonasQueAtienden),
    subConjunto(PersonasQueAtienden, PosibleGrupo).

subConjunto([], []).

subConjunto([Elemento | ColaListaOriginal], [Elemento | ColaSubconjunto]) :-
    subConjunto(ColaListaOriginal, ColaSubconjunto).

subConjunto([_ | ColaListaOriginal], Subconjunto) :-
    subConjunto(ColaListaOriginal, Subconjunto).

