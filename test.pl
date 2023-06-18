% Tipos de bebidas:
tipo_de_bebida(gaseosa).
tipo_de_bebida(alcohol).

% Bebidas:
bebida(coca_cola).
bebida(pepsi).
bebida(fanta).
bebida(sprite).
bebida(seven_up).

bebida(fernet_branca).
bebida(smirnoff).
bebida(gancia).
bebida(bombay).
bebida(campari).

% Declaramos el tipos de bebida de cada una
bebida_es_de_tipo(coca_cola, gaseosa).
bebida_es_de_tipo(pepsi, gaseosa).
bebida_es_de_tipo(fanta, gaseosa).
bebida_es_de_tipo(sprite, gaseosa).
bebida_es_de_tipo(seven_up, gaseosa).
bebida_es_de_tipo(fernet_branca, alcohol).
bebida_es_de_tipo(smirnoff, alcohol).
bebida_es_de_tipo(gancia, alcohol).
bebida_es_de_tipo(bombay, alcohol).
bebida_es_de_tipo(campari, alcohol).

% Ventas por bebida:
venta(coca_cola, 7).
venta(pepsi, 5).
venta(coca_cola, 2).
venta(seven_up, 1).
venta(smirnoff, 3).
venta(campari, 3).
venta(seven_up, 1).

% Predicados_______________________________________________
% Si son del mismo tipo:
mismo_tipo_bebida(X, Y) :- bebida_es_de_tipo(X, Z),
 bebida_es_de_tipo(Y, Z),
dif(X, Y).


:- dynamic venta_total/2. % Declaramos un hecho dinámico para almacenar la venta total de cada producto

calcular_venta_total_minima :-
    retractall(venta_total(_, _)), % Eliminamos cualquier venta total anterior
    
    venta(Bebida, Cantidad), % Obtenemos una venta de la base de conocimientos
    actualizar_venta_total(Bebida, Cantidad), % Actualizamos la venta total
    
    fail. % Fallamos para procesar todas las ventas posibles

calcular_venta_total_minima :- % Cuando no hay más ventas posibles, encontramos el producto menos vendido
    min_venta_total(BebidaMin, CantidadMin),
    write('El producto menos vendido es: '),
    write(BebidaMin),
    write(' - Total de ventas: '),
    write(CantidadMin).

calcular_venta_total_maxima :- % Predicado para calcular la venta total máxima
    retractall(venta_total(_, _)), % Eliminamos cualquier venta total anterior
    
    venta(Bebida, Cantidad), % Obtenemos una venta de la base de conocimientos
    actualizar_venta_total(Bebida, Cantidad), % Actualizamos la venta total
    
    fail. % Fallamos para procesar todas las ventas posibles

calcular_venta_total_maxima :- % Cuando no hay más ventas posibles, encontramos el producto más vendido
    max_venta_total(BebidaMax, CantidadMax),
    write('El producto más vendido es: '),
    write(BebidaMax),
    write(' - Total de ventas: '),
    write(CantidadMax).

actualizar_venta_total(Bebida, Cantidad) :-
    (venta_total(Bebida, Total) -> % Verificamos si ya existe una venta total para la bebida
        NuevaCantidad is Total + Cantidad,
        retract(venta_total(Bebida, _))
    ;
        NuevaCantidad = Cantidad
    ),
    assert(venta_total(Bebida, NuevaCantidad)).

min_venta_total(BebidaMin, CantidadMin) :-
    findall(Cantidad, venta_total(_, Cantidad), Cantidades),
    min_list(Cantidades, CantidadMin),
    venta_total(BebidaMin, CantidadMin).

max_venta_total(BebidaMax, CantidadMax) :-
    findall(Cantidad, venta_total(_, Cantidad), Cantidades),
    max_list(Cantidades, CantidadMax),
    venta_total(BebidaMax, CantidadMax).
