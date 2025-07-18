REPORT zaluno17_exercicio3_principal.

* Declaração da tabela VBAK que será usada para acessar os dados do SAP
TABLES: vbak.

* Cria um campo de seleção na tela para o usuário informar o número do pedido (vbeln)
* É como se fosse um input ou prompt no JavaScript
SELECT-OPTIONS dc_vend FOR vbak-vbeln NO INTERVALS.

* Inclui o arquivo que contém as definições de tipos e variáveis (tipo importar um "type" no JS)
INCLUDE zaluno17_exercio3v2_vars.
* Inclui o arquivo que contém a classe (tipo importar uma classe em JS)
INCLUDE zaluno17_exercio3v3_class.

* Declara uma variável para armazenar o objeto da classe 'pedido'
* Em JS seria algo como: let meuPedido = new Pedido();
DATA: meu_pedido TYPE REF TO pedido.

* Ponto inicial da execução do programa (tipo o que fica dentro do main() ou do index.js)
START-OF-SELECTION.

* Cria o objeto da classe 'pedido' (em JS seria: meuPedido = new Pedido();)
CREATE OBJECT meu_pedido.
* Chama o método que busca os dados do banco e guarda na memória (igual a buscar dados e guardar num array no JS)
  meu_pedido->buscar_pedidos( ).
* Chama o método que mostra os dados na tela (igual a dar um console.log nos dados em JS)
  meu_pedido->mostrar_pedidos( ).