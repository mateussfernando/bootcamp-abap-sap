*&---------------------------------------------------------------------*
*& Report ZALUNO18_RVENDAS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaluno18_rvendas.

TABLES: vbak, vbap, vbpa.

SELECT-OPTIONS: so_venda FOR vbak-vbeln OBLIGATORY,
                so_item FOR vbap-posnr.

" 1º Versão - Versão mais 'pura'.
" INCLUDE zaluno18_rvendas_class.

" 2º Versão utilizando a Call Function personalizada
" responsável por armazenar a lógica encapsulada do SELECT.
INCLUDE ZALUNO18_RVENDAS_CLASS2.

START-OF-SELECTION.
  DATA(lo_rel_vendas) = NEW lclass_rel_vendas( ).
  lo_rel_vendas->buscar_rel_vendas( ).
  lo_rel_vendas->exibir_rel_vendas( ).