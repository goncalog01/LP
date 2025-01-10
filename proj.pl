% Goncalo Filipe Costa Guerreiro - 95581

:- [codigo_comum].

%-----------------------------------------------------------------------------------
% obtem_letras_palavras(Lst_Pals, Letras) significa que Letras e' a lista ordenada
% cujos elementos sao listas com as letras de cada palavra de Lst_Pals.
%
% obtem_letras_palavras_aux(Lst_Pals, Ac, Letras) percorre cada palavra em Lst_Pals
% e junta a lista com as suas letras a Ac, sendo Letras o resultado de, no final,
% ordenar Ac alfabeticamente.
%-----------------------------------------------------------------------------------

obtem_letras_palavras(Lst_Pals, Letras) :-
    obtem_letras_palavras_aux(Lst_Pals, [], Letras).

obtem_letras_palavras_aux([], Ac, Letras) :-
    sort(Ac, Letras).

obtem_letras_palavras_aux([P_Pal|R_Pals], Ac, Letras) :-
    atom_chars(P_Pal, Letras_P_Pal),
    append(Ac, [Letras_P_Pal], N_Ac),
    obtem_letras_palavras_aux(R_Pals, N_Ac, Letras).

%---------------------------------------------------------------------------------
% espaco_fila(Fila, Esp) significa que Esp e' um espaco de Fila.
%
% prefixo(L) significa que a lista L e' um prefixo, ou seja, que e' a lista vazia
% ou o seu ultimo elemento e' #.
%
% sufixo(L) significa que a lista L e' um sufixo, ou seja, que e' a lista vazia
% ou o seu primeiro elemento e' #.
%
% pertence(El, L) significa que o elemento El pertence 'a lista L.
%---------------------------------------------------------------------------------

espaco_fila(Fila, Esp) :-
    append([Pref, Esp, Suf], Fila),
    length(Esp, Comp_Esp),
    Comp_Esp >= 3,
    prefixo(Pref),
    sufixo(Suf),
    \+ pertence(#, Esp).

% se o ultimo elemento de L nao for # devolve 'falso'
prefixo(L) :-
    last(L, Ult),
    Ult \== #,
    !,
    fail.

% caso contrario devolve 'verdadeiro'
prefixo(_).

% se o primeiro elemento de L nao for # devolve 'falso'
sufixo([P_El|_]) :-
    P_El \== #,
    !,
    fail.

% caso contrario devolve 'verdadeiro'
sufixo(_).

pertence(P, [Q | _]) :- P == Q.

pertence(P, [_ | R]) :- pertence(P, R).

%----------------------------------------------------------------------------------
% espacos_fila(Fila, Espacos) significa que Espacos e' a lista de todos os espacos
% de Fila, da esquerda para a direita.
%----------------------------------------------------------------------------------

% quando a fila nao tem espacos, o resultado e' a lista vazia
espacos_fila(Fila, []) :-
    \+ bagof(Esp, espaco_fila(Fila, Esp), _).

espacos_fila(Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp), Espacos).

%-----------------------------------------------------------------------------------------
% espacos_puzzle(Grelha, Espacos) significa que Espacos e' a lista de espacos de Grelha.
%
% espacos_puzzle(Grelha, Ac, Espacos) e' um predicado auxiliar que percorre cada
% linha de Grelha e adiciona a Ac os seus espacos, sendo Espacos o resultado final de Ac.
%-----------------------------------------------------------------------------------------

% a matriz e' transposta para verificar os espacos das colunas
espacos_puzzle(Grelha, Espacos) :-
    mat_transposta(Grelha, Grelha_Trans),
    espacos_puzzle(Grelha, [], Esp_Linhas),
    espacos_puzzle(Grelha_Trans, [], Esp_Col),
    append(Esp_Linhas, Esp_Col, Espacos).

espacos_puzzle([], Espacos, Espacos) :- !.

espacos_puzzle([P_Linha|R_Linhas], Ac, Espacos) :-
    espacos_fila(P_Linha, Esps_P_Linha),
    append(Ac, Esps_P_Linha, N_Ac),
    espacos_puzzle(R_Linhas, N_Ac, Espacos).

%--------------------------------------------------------------------------------------------------
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) significa que Esps_com e'
% a lista de espacos com variaveis em comum com Esp, exceptuando Esp.
%
% espacos_com_posicoes_comuns_aux(Espacos, Esp, Ac, Esps_com) percorre cada espaco em
% Espacos e, caso tenha posicoes em comum com Esp, e' adicionado a Ac, sendo
% Esps_com o resultado final de Ac.
%
% elimina(El, L1, L2) significa que L2 e' a lista resultante de remover o elemento El da lista L1.
%
% el_comuns(L1, L2) significa que as listas L1 e L2 tem elementos em comum.
%--------------------------------------------------------------------------------------------------

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    elimina(Esp, Espacos, Espacos_menos_Esp),
    espacos_com_posicoes_comuns_aux(Espacos_menos_Esp, Esp, [], Esps_com).

espacos_com_posicoes_comuns_aux([], _, Esps_com, Esps_com) :- !.

espacos_com_posicoes_comuns_aux([P_Esp|R_Esp], Esp, Ac, Esps_com) :-
    el_comuns(P_Esp, Esp),
    !,
    append(Ac, [P_Esp], N_Ac),
    espacos_com_posicoes_comuns_aux(R_Esp, Esp, N_Ac, Esps_com).

espacos_com_posicoes_comuns_aux([_|R_Esp], Esp, Ac, Esps_com) :-
    espacos_com_posicoes_comuns_aux(R_Esp, Esp, Ac, Esps_com).

elimina(El, [P|R], R) :-
    El == P,
    !.

elimina(El, [P|R1], [P|R2]) :-
    elimina(El, R1, R2).

el_comuns([P|_], L2) :-
    pertence(P, L2),
    !.

el_comuns([_|R], L2) :-
    el_comuns(R, L2).

%------------------------------------------------------------------------------------
% palavra_possivel_esp(Pal, Esp, Espacos, Letras) significa que Pal e' uma
% palavra possivel para o espaco Esp.
%
% possiveis(Espacos, Letras) percorre cada espaco de Espacos e verifica se existe
% pelo menos uma palavara de Letras que unifique com esse espaco.
%
% possivel(Esp, Letras) percorre cada palavra em Letras e verifica se existe
% pelo menos uma que unifique com Esp.
%
% pal_possivel(Esp, Palavra) significa que Esp unifica com Palavra (nao altera Esp).
%
% copia(Esp, Copia) significa que Copia e' uma copia de Esp.
%
% copia_el(El, C) significa que C e' uma copia do elemento El.
%------------------------------------------------------------------------------------

palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-
    Esp = Pal,
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    possiveis(Esps_com, Letras).

possiveis([], _) :- !.

possiveis([P_Esp|R_Esps], Letras) :-
    possivel(P_Esp, Letras),
    possiveis(R_Esps, Letras).

possivel(Esp, [P_Pal|_]) :-
    pal_possivel(Esp, P_Pal),
    !.

possivel(Esp, [_|R_Pals]) :-
    possivel(Esp, R_Pals).

pal_possivel(Esp, Palavra) :-
    copia(Esp, Copia),
    Copia = Palavra.

copia(Esp, Copia) :-
    maplist(copia_el, Esp, Copia).

copia_el(El, _) :-
    var(El),
    !.

copia_el(El, El).

%-----------------------------------------------------------------------------
% palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) significa que
% Pals_Possiveis e' a lista ordenada de palavras possiveis para o espaco Esp.
%-----------------------------------------------------------------------------

palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) :-
    findall(Pal, (member(Pal, Letras), palavra_possivel_esp(Pal, Esp, Espacos, Letras)), Pals_Possiveis).

%----------------------------------------------------------------------------------
% palavras_possiveis(Letras, Espacos, Pals_Possiveis) significa que Pals_Possiveis
% e' a lista de palavras possiveis.
%----------------------------------------------------------------------------------

palavras_possiveis(Letras, Espacos, Pals_Possiveis) :-
    bagof([Esp, Lst_Pals], (member(Esp, Espacos), palavras_possiveis_esp(Letras, Espacos, Esp, Lst_Pals)), Pals_Possiveis).

%--------------------------------------------------------------------------------------
% letras_comuns(Lst_Pals, Letras_comuns) significa que Letras_comuns e' uma lista
% de pares (pos, letra), significando que todas as listas de Lst_Pals contem a letra
% letra na posicao pos.
%
% letras_comuns_aux(Lst_Pals, Ac, Letras_comuns) percorre as palavras em Lst_Pals,
% verificando as letras que tem em comum duas a duas, e comparando com Ac, que contem
% as letras em comum ate ao momento, sendo Letras_comuns o resultado final de Ac.
%
% letras_comuns_pals(Pal1, Pal2, N, Letras_comuns) significa que Letras_comuns e'
% uma lista de pares (pos, letra) significando que tanto Pal1 como Pal2 contem a
% letra letra na posicao pos (o N vai acompanhando o indice que esta a ser comparado).
%--------------------------------------------------------------------------------------

% se Lst_Pals so tiver uma palavra, Letras_comuns resulta de comparar a palavra consigo propria
letras_comuns([Pal], Letras_comuns) :-
    length([Pal], Num_Pals),
    Num_Pals =:= 1,
    !,
    letras_comuns_pals(Pal, Pal, 1, Letras_comuns).

letras_comuns([P_Pal, S_Pal|R_Pals], Letras_comuns) :-
    letras_comuns_pals(P_Pal, S_Pal, 1, Letras_comuns_act),
    !,
    letras_comuns_aux([S_Pal|R_Pals], Letras_comuns_act, Letras_comuns).

% esta clausula, embora nao necessaria, evita que, se em algum momento Ac
% for a lista vazia, o programa continue a percorrer Lst_Pals, uma vez que
% nesse caso ja se sabe que Letras_comuns sera tambem a lista vazia
letras_comuns_aux(_, [], []) :- !.

% quando Lst_Pals so tiver uma palavra, Letras_comuns unifica com Ac
letras_comuns_aux([_], Letras_comuns, Letras_comuns) :- !.

letras_comuns_aux([P_Pal, S_Pal|R_Pals], Ac, Letras_comuns) :-
    letras_comuns_pals(P_Pal, S_Pal, 1, Letras_comuns_act),
    intersection(Ac, Letras_comuns_act, N_Ac),
    !,
    letras_comuns_aux([S_Pal|R_Pals], N_Ac, Letras_comuns).

letras_comuns_pals([], _, _, []) :- !.

letras_comuns_pals(_, [], _, []) :- !.

letras_comuns_pals([P_L1|R_L1], [P_L2|R_L2], Cont, Letras_comuns) :-
    P_L1 == P_L2,
    !,
    Cont_mais_1 is Cont + 1,
    letras_comuns_pals(R_L1, R_L2, Cont_mais_1, Res_Letras_comuns),
    append([(Cont, P_L1)], Res_Letras_comuns, Letras_comuns).

letras_comuns_pals([_|R_L1], [_|R_L2], Cont, Letras_comuns) :-
    Cont_mais_1 is Cont + 1,
    letras_comuns_pals(R_L1, R_L2, Cont_mais_1, Letras_comuns).

%----------------------------------------------------------------------------------
% atribui_comuns(Pals_Possiveis) actualiza Pals_Possiveis atribuindo a cada espaco
% as letras comuns a todas as palavras possiveis para esse espaco.
%
% atribui(Esp, Letras_comuns) atribui a Esp cada uma das letras de Letras_comuns 
% na respetiva posicao.
%----------------------------------------------------------------------------------

atribui_comuns([]) :- !.

atribui_comuns([[Esp, Lst_Palavras]|R_Esps]) :-
    letras_comuns(Lst_Palavras, Letras_comuns),
    atribui(Esp, Letras_comuns),
    !,
    atribui_comuns(R_Esps).

atribui(_, []) :- !.

atribui(Esp, [(Pos, Letra)|R_L]) :-
    nth1(Pos, Esp, Letra),
    atribui(Esp, R_L).

%---------------------------------------------------------------------------------------------
% retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) significa que Novas_Pals_Possiveis
% e' o resultado de tirar palavras impossiveis de Pals_Possiveis.
%
% retira_impossiveis(Esp, Ac, Novo_Esp) percorre a lista de palavras possiveis de Esp
% e adiciona a Ac aquelas que ainda sao possiveis, sendo Novo_Esp o resultado final.
%---------------------------------------------------------------------------------------------

retira_impossiveis([], []) :- !.

retira_impossiveis([P_Esp|R_Esps], Novas_Pals_Possiveis) :-
    retira_impossiveis_esp(P_Esp, [], Novo_P_Esp),
    retira_impossiveis(R_Esps, Novo_R_Esps),
    append([Novo_P_Esp], Novo_R_Esps, Novas_Pals_Possiveis).

retira_impossiveis_esp([Esp, []], Pals_Possiveis, [Esp, Pals_Possiveis]) :- !.

retira_impossiveis_esp([Esp, [P_Pal|R_Pals]], Ac, Novo_Esp) :-
    pal_possivel(Esp, P_Pal),
    !,
    append(Ac, [P_Pal], N_Ac),
    retira_impossiveis_esp([Esp, R_Pals], N_Ac, Novo_Esp).

retira_impossiveis_esp([Esp, [_|R_Pals]], Ac, Novo_Esp) :-
    retira_impossiveis_esp([Esp, R_Pals], Ac, Novo_Esp).

%----------------------------------------------------------------------------------
% obtem_unicas(Pals_Possiveis, Unicas) significa que Unicas e' a lista de palavras
% unicas de Pals_Possiveis.
%----------------------------------------------------------------------------------

obtem_unicas([], []) :- !.

% se o numero de palavras possiveis para o espaco for 1, entao essa palavra e' adicionada a Unicas
obtem_unicas([[_, Lst_Pals]|R_Esps], Unicas) :-
    length(Lst_Pals, Num_Pals),
    Num_Pals =:= 1,
    !,
    obtem_unicas(R_Esps, Unicas_R_Esps),
    append(Lst_Pals, Unicas_R_Esps, Unicas).

obtem_unicas([_|R_Esps], Unicas) :-
    obtem_unicas(R_Esps, Unicas).

%----------------------------------------------------------------------------------------
% retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) significa que Novas_Pals_Possiveis
% e' o resultado de retirar de Pals_Possiveis as palavras unicas.
%
% retira_unicas_aux(Pals_Possiveis, Unicas, Novas_Pals_Possiveis) percorre cada
% espaco de Pals_Possiveis, eliminando da lista de palavras possiveis aquelas que
% estiverem em Unicas, sendo Novas_Pals_Possiveis o resultado final.
%----------------------------------------------------------------------------------------

retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) :-
    obtem_unicas(Pals_Possiveis, Unicas),
    retira_unicas_aux(Pals_Possiveis, Unicas, Novas_Pals_Possiveis).

retira_unicas_aux([], _, []) :- !.

% se a lista de palavras possiveis para o espaco so tiver 1 palavra, entao nao e' alterado
retira_unicas_aux([[Esp, Lst_Pals]|R_Esps], Unicas, Novas_Pals_Possiveis) :-
    length(Lst_Pals, Num_Pals),
    Num_Pals =:= 1,
    !,
    retira_unicas_aux(R_Esps, Unicas, Novo_R_Esps),
    append([[Esp, Lst_Pals]], Novo_R_Esps, Novas_Pals_Possiveis).

% caso contrario, as palavras de Lst_Pal que estiverem em Unicas sao eliminadas
retira_unicas_aux([[Esp, Lst_Pals]|R_Esps], Unicas, Novas_Pals_Possiveis) :-
    intersection(Lst_Pals, Unicas, Pals_Comuns),
    subtract(Lst_Pals, Pals_Comuns, Nova_Lst_Pals),
    retira_unicas_aux(R_Esps, Unicas, Novo_R_Esps),
    append([[Esp, Nova_Lst_Pals]], Novo_R_Esps, Novas_Pals_Possiveis).

%----------------------------------------------------------------------------------------
% simplifica(Pals_Possiveis, Novas_Pals_Possiveis) significa que Novas_Pals_Possiveis e'
% o resultado de simplificar Pals_Possiveis.
%----------------------------------------------------------------------------------------

% se nao houverem alteracoes, termina
simplifica(Pals_Possiveis, Novas_Pals_Possiveis) :-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis, Pals_Possiveis_menos_Imp),
    retira_unicas(Pals_Possiveis_menos_Imp, Novas_Pals_Possiveis),
    Novas_Pals_Possiveis == Pals_Possiveis,
    !.

% caso contrario, continua a simplificar
simplifica(Pals_Possiveis, Novas_Pals_Possiveis) :-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis, Pals_Possiveis_menos_Imp),
    retira_unicas(Pals_Possiveis_menos_Imp, Pals_Possiveis_menos_Unicas),
    simplifica(Pals_Possiveis_menos_Unicas, Novas_Pals_Possiveis),
    !.

%-------------------------------------------------------------------------------------
% inicializa(Puz, Pals_Possiveis) significa que Pals_Possiveis e' a lista de palavras
% possiveis simplificada para Puz.
%-------------------------------------------------------------------------------------

inicializa([Palavras, Grelha], Pals_Possiveis) :-
    obtem_letras_palavras(Palavras, Lst_Pals),
    espacos_puzzle(Grelha, Espacos),
    palavras_possiveis(Lst_Pals, Espacos, Pals_Possiveis_nao_Simpl),
    simplifica(Pals_Possiveis_nao_Simpl, Pals_Possiveis).

%--------------------------------------------------------------------------------------------------
% escolhe_menos_alternativas(Pals_Possiveis, Escolha) significa que Escolha e' o
% elemento de Pals_Possiveis com menor numero de palavras possiveis para o seu espaco.
%
% escolhe_menos_alternativas_maior_1(Pals_Possiveis, Pals_Possiveis_maior_1) significa que
% Pals_Possiveis_maior_1 e' a lista cujos elementos sao os espacos de Pals_Possiveis com
% um numero de palavras possiveis maior do que 1.
%
% escolhe_menos_alternativas_possibilidades(Pals_Possiveis, Escolha) verifica qual das
% possibilidades se verifica: se Pals_Possiveis for a lista vazia devolve 'falso', caso
% contrario invoca o escolhe_menos_alternativas_aux para determinar a Escolha.
%
% escolhe_menos_alternativas_aux(Pals_Possiveis, Comp_Min, Escolha_act, Escolha) percorre todos
% os espacos de Pals_Possiveis, sendo Escolha aquele que tiver menor numero de palavras possiveis.
%--------------------------------------------------------------------------------------------------

escolhe_menos_alternativas(Pals_Possiveis, Escolha) :-
    escolhe_menos_alternativas_maior_1(Pals_Possiveis, Pals_Possiveis_maior_1),
    escolhe_menos_alternativas_possibilidades(Pals_Possiveis_maior_1, Escolha).

escolhe_menos_alternativas_maior_1([], []) :- !.

% se o espaco tiver mais do que uma palavra possivel e' adicionado a Pals_Possiveis_maior_1
escolhe_menos_alternativas_maior_1([[Esp, Lst_Pals]|R_Esps], [[Esp, Lst_Pals]|R_maior_1]) :-
    length(Lst_Pals, Num_Pals),
    Num_Pals > 1,
    !,
    escolhe_menos_alternativas_maior_1(R_Esps, R_maior_1).

escolhe_menos_alternativas_maior_1([_|R_Esps], Pals_Possiveis_maior_1) :-
    escolhe_menos_alternativas_maior_1(R_Esps, Pals_Possiveis_maior_1).

% se Pals_Possiveis for a lista vazia quer dizer que todos os 
% espacos tem apenas 1 palavra possivel, logo devolve 'falso'
escolhe_menos_alternativas_possibilidades(Pals_Possiveis, _) :-
    Pals_Possiveis == [],
    !,
    fail.

% caso contrario, invoca o escolhe_menos_alternativas_aux para encontrar Escolha
escolhe_menos_alternativas_possibilidades([[Esp, Lst_Pals]|R_Esps], Escolha) :-
    length(Lst_Pals, Num_Pals),
    escolhe_menos_alternativas_aux(R_Esps, Num_Pals, [Esp, Lst_Pals], Escolha).

% quando chegar ao fim de Pals_Possiveis, Escolha unifica com Escolha_act
escolhe_menos_alternativas_aux([], _, Escolha, Escolha) :- !.

% se Lst_Pal do espaco atual tiver comprimento menor do que Comp_Min,
% entao o espaco passa a ser a Escolha_act e o seu comprimento o Comp_Min
escolhe_menos_alternativas_aux([[Esp, Lst_Pals]|R_Esps], Comp_Min, _, Escolha) :-
    length(Lst_Pals, Num_Pals),
    Num_Pals < Comp_Min,
    !,
    escolhe_menos_alternativas_aux(R_Esps, Num_Pals, [Esp, Lst_Pals], Escolha).

escolhe_menos_alternativas_aux([_|R_Esps], Comp_Min, Escolha_act, Escolha) :-
    escolhe_menos_alternativas_aux(R_Esps, Comp_Min, Escolha_act, Escolha).

%----------------------------------------------------------------------------------
% experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis) escolhe uma
% palavra Pal de Escolha e unifica o espaco Esp de Escolha com essa palavra, sendo
% Novas_Pals_Possiveis o resultado de substituir, em Pals_Possiveis, o elemento
% Escolha pelo elemento [Esp, [Pal]].
%----------------------------------------------------------------------------------

% vai percorrendo Pals_Possiveis ate encontrar o elemento Escolha
experimenta_pal([Esp, Lst_Pals], [P_Esp|R_Esps], [Novo_P_Esp|R_Esps]) :-
    [Esp, Lst_Pals] == P_Esp,
    !,
    member(Pal, Lst_Pals),
    Esp = Pal,
    append([Esp], [[Pal]], Novo_P_Esp).

experimenta_pal(Escolha, [P_Esp|R_Esps], [P_Esp|R]) :-
    experimenta_pal(Escolha, R_Esps, R).

%---------------------------------------------------------------------------------------
% resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) significa que Novas_Pals_Possiveis
% e' o resultado de aplicar sucessivamente os predicados escolhe_menos_alternativas,
% experimenta_pal e simplifica a Pals_Possiveis ate nao existirem espacos com um numero
% de palavras possiveis superior a um.
%---------------------------------------------------------------------------------------

% se o escolhe_menos_alternativas devolver 'falso' significa que
% todos os espacos tem apenas 1 palavra possivel, logo termina
resolve_aux(Pals_Possiveis, Pals_Possiveis) :-
    \+ escolhe_menos_alternativas(Pals_Possiveis, _).

% caso contrario, continua a resolver
resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) :-
    escolhe_menos_alternativas(Pals_Possiveis, Escolha),
    experimenta_pal(Escolha, Pals_Possiveis, Pals_Possiveis_Exp),
    simplifica(Pals_Possiveis_Exp, Novas_Pals_Possiveis_Exp),
    resolve_aux(Novas_Pals_Possiveis_Exp, Novas_Pals_Possiveis),
    !.

%--------------------------------------------------------------------------------
% resolve(Puz) resolve o puzzle Puz, isto e', apos a invocacao deste predicado a
% grelha de Puz tem todas as variaveis substituidas por letras que constituem as
% palavras da lista de palavras de Puz.
%
% preenche_grelha(Grelha, Res_Puz) obtem os espacos de Grelha e invoca o
% predicado preenche_espacos para preenche-los com Res_Puz.
%
% preenche_espacos(Espacos, Res_Puz) percorre todos os espacos em Espacos
% e preenche-os, unificando-os com o respetivo espaco em Res_Puz.
%--------------------------------------------------------------------------------

resolve([Lst_Pals, Grelha]) :-
    inicializa([Lst_Pals, Grelha], Pals_Possiveis),
    resolve_aux(Pals_Possiveis, Res_Puz),
    preenche_grelha(Grelha, Res_Puz).

preenche_grelha(Grelha, Res_Puz) :-
    espacos_puzzle(Grelha, Espacos),
    preenche_espacos(Espacos, Res_Puz).

preenche_espacos([], _) :- !.

preenche_espacos([P_Esp|R_Esps], [[Esp|_]|R_Pals]) :-
    P_Esp = Esp,
    preenche_espacos(R_Esps, R_Pals).