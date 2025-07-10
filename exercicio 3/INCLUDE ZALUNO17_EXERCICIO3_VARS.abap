*&---------------------------------------------------------------------*
*& Include ZALUNO17_EXERCICIO3_VARS
*&---------------------------------------------------------------------*

INCLUDE ZALUNO17_EXERCICIO3_VARS

*Define uma estrutura do tipo_pedido
*para armazenar dados de um pedido
TYPES: BEGIN OF tipo_pedido,
         numero_pedido      TYPE vbak-vbeln,
         vendedor           TYPE vbak-ernam,
         canal_venda        TYPE vbak-vtweg,
       END OF tipo_pedido.

*Declara uma variavel
*para armazenar um registro do tipo_pedido
DATA: tabela_pedidos TYPE TABLE OF tipo_pedido,
      registro_pedido TYPE tipo_pedido.

*Cria um campo de seleção para o usuário informar um ou mais
*números de pedido sem permitir intervalos
SELECT-OPTIONS: filtro_pedidos FOR vbak-vbeln NO INTERVALS.