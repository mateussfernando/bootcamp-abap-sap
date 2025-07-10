*STRINGS> 
*Criar e exibir uma variável com seu nome.
*1 METODO declarando variavel:
DATA nome(30) TYPE C.
nome = 'mateus'.
WRITE '1 - Nome informado;' , nome.

*2 METODO usando parameter para 1 valor:
PARAMETERS: p_nome TYPE c LENGTH 30.
START-OF-SELECTION.
WRITE: '2 - Nome informado:' , p_nome.

*3 METODO usando parameter para vários valores ou intervalos:
DATA: nome TYPE c LENGTH 30.
SELECT-OPTIONS: s_nome FOR nome.
START-OF-SELECTION.
LOOP AT s_nome.
  WRITE: / '3- Nome informado:', s_nome-low.
ENDLOOP.
