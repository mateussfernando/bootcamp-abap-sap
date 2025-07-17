*&---------------------------------------------------------------------*
*& Report ZALUNO17_EXER5V3
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZALUNO17_EXER5V3.

* 1 - Cria tipo de estrutura e tipo tabela
TYPES: BEGIN OF tipo_estrutura,
         numero_pedido     TYPE ebeln,      " Nº do documento de compras
         numero_item       TYPE ebelp,      " Nº item do documento de compra
         quantidade_pedido TYPE bstmg,      " Quantidade do item
         texto_item        TYPE char50,     " Texto do cabeçalho do pedido
         desc_item         TYPE char50,     " Descrição curta do item
       END OF tipo_estrutura,
       tipo_estrutura_tab TYPE STANDARD TABLE OF tipo_estrutura WITH DEFAULT KEY.

* 2 - Variáveis internas
DATA: tabela_interna TYPE tipo_estrutura_tab,
      tipo_tabela    TYPE tipo_estrutura.

DATA: lt_item       TYPE TABLE OF bapiekpo,
      lt_desc_item  TYPE TABLE OF bapiekpotx,   " estrutura correta para textos do item
      lt_txt_pedido TYPE TABLE OF bapiekkotx.

* 3 - Campos de seleção (range)
SELECT-OPTIONS: i_ebeln FOR tipo_tabela-numero_pedido OBLIGATORY,
                i_ebelp FOR tipo_tabela-numero_item.

* 4 - Busca os pedidos
START-OF-SELECTION.

  SELECT ebeln,
         ebelp,
         menge
    INTO TABLE @tabela_interna
    FROM ekpo
    WHERE ebeln IN @i_ebeln
      AND ebelp IN @i_ebelp.

  IF sy-subrc <> 0.
    MESSAGE 'Nenhum registro encontrado para os filtros informados.' TYPE 'E'.
    EXIT.
  ENDIF.

* 5 - Enriquecer os dados com BAPI usando lógica de APPEND
  DATA lv_flag TYPE flag.

  " Cria uma tabela temporária para armazenar os registros enriquecidos
  DATA(tabela_final) = VALUE tipo_estrutura_tab( ).

  LOOP AT tabela_interna INTO tipo_tabela.

    CLEAR: lt_item, lt_desc_item, lt_txt_pedido.
    lv_flag = 0.

    CALL FUNCTION 'BAPI_PO_GETDETAIL'
      EXPORTING
        purchaseorder   = tipo_tabela-numero_pedido
        items           = 'X'
        item_texts      = 'X'
        header_texts    = 'X'
      TABLES
        po_items        = lt_item
        po_item_texts   = lt_desc_item
        po_header_texts = lt_txt_pedido.

    LOOP AT lt_item INTO DATA(ls_item).
      " Preenche os campos básicos
      DATA(ls_aux) = tipo_tabela.
      ls_aux-numero_item       = ls_item-po_item.
      ls_aux-quantidade_pedido = ls_item-quantity.

      " Primeiro loop: textos do item
      LOOP AT lt_desc_item INTO DATA(ls_texto_item)
           WHERE po_number = ls_aux-numero_pedido
             AND po_item   = ls_aux-numero_item.
        ls_aux-texto_item = ls_texto_item-text_line.
        APPEND ls_aux TO tabela_final.
        lv_flag = 1.
      ENDLOOP.

      " Segundo loop: textos de cabeçalho do pedido
      LOOP AT lt_txt_pedido INTO DATA(ls_txt_cabec).
        IF lv_flag = 1.
          LOOP AT tabela_final ASSIGNING FIELD-SYMBOL(<fs_final>)
               WHERE numero_pedido = ls_aux-numero_pedido
                 AND numero_item   = ls_aux-numero_item.
            <fs_final>-desc_item = ls_txt_cabec-text_line.
          ENDLOOP.
        ELSE.
          ls_aux-desc_item = ls_txt_cabec-text_line.
          APPEND ls_aux TO tabela_final.
          lv_flag = 1.
        ENDIF.
      ENDLOOP.

      " Se não encontrou nenhum texto, ainda assim inclui o item
      IF lv_flag <> 1.
        APPEND ls_aux TO tabela_final.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

  tabela_interna = tabela_final.

* 6 - Exibir em ALV
CLASS cl_salv_table DEFINITION LOAD.

DATA: alv           TYPE REF TO cl_salv_table,
      alv_functions TYPE REF TO cl_salv_functions_list.

cl_salv_table=>factory(
  IMPORTING
    r_salv_table = alv
  CHANGING
    t_table      = tabela_interna ).

alv_functions = alv->get_functions( ).
alv_functions->set_all( abap_true ).

alv->display( ).