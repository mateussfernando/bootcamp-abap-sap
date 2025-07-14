*&---------------------------------------------------------------------*
*& Report ZALUNO17_EXER5
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaluno17_exer5.

*1 - Crie um tipo de estrutura com os campo de pedido, item do pedido e a quantidade do item do pedido,
*a referencia pra isso es´ta na Ekpo, pra pedido e EKPO pra item do pedido. Usem a instrução TYPES.
TYPES: BEGIN OF tipo_estrutura,
         numero_pedido     TYPE ebeln, "ebeln = numero do pedido
         numero_item       TYPE ebelp, "ebelp = numero do item
         quantidade_pedido TYPE bstmg, "bstmg = quantidade de item
       END OF tipo_estrutura.

*2 - Definam 2 variaveis 1 do tipo tabela e outra do tipo estrutura a partir desse tipo previamente criado.
DATA: tabela_interna TYPE TABLE OF tipo_estrutura,
      tipo_tabela    TYPE tipo_estrutura.

*3 - Define duas entradas de tela uma para o campo pedido, sendo a mesma no formato de range e
*uma pra o item do pedido também com o range. vamos usar os comandos de criação de campos na tela.
*Entradas de tela para número do pedido ebeln e item do pedido ebelp
SELECT-OPTIONS: i_ebeln FOR tipo_tabela-numero_pedido OBLIGATORY,
                i_ebelp FOR tipo_tabela-numero_item.

*4 - Faça uma rotina de validação para o conjunto pedido/item caso nao exista
*vamos emitir uma mensagem de erro usando comando message e vamo parar a execução.
*O código faz select de itens de pedidos na tabela EKPO com base em números de pedido e item fornecidos.
*Se não encontrar nenhum registro, exibe uma mensagem de erro.
START-OF-SELECTION.
  SELECT ebeln, ebelp
    INTO TABLE @tabela_interna
    FROM ekpo
    WHERE ebeln IN @i_ebeln
      AND ebelp IN @i_ebelp.
  IF sy-subrc <> 0.
    MESSAGE ' erro não tem registros' TYPE 'String'.
    EXIT.
  ENDIF.

*5 - colete os dados de pedido e itme de pedido, os definidos anteriormente, pra dentro de uma tabela interna.
*pode usar comandos sql ou função que vcs ja conhecem
  LOOP AT tabela_interna INTO tipo_tabela.
    WRITE: / 'Impessão dos dados de pedido e item de pedido:',
             'Pedido:', tipo_tabela-numero_pedido,
             'Item:', tipo_tabela-numero_item,
             'Quantidade:', tipo_tabela-quantidade_pedido.
  ENDLOOP.
  
    