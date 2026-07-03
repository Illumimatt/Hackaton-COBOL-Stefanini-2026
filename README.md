# Hackaton COBOL Stefanini 2026

Repositório com as resoluções dos desafios propostos durante o Hackaton COBOL Stefanini 2026. Contém desde a lógica básica até processamento batch e integração com JSON.

## Estrutura do Repositório

* `/Enunciados`: PDFs oficiais com as especificações dos exercícios.
* `/Codigos-Iniciais`: Primeiros rascunhos e lógica inicial.
* `/Codigos-Revisados`: Programas finais validados e padronizados.
* `/Arquivos-txt`: Arquivos de entrada, saída e massas de teste.

## Resumo dos Programas (Codigos-Revisados)

* **BHCP0001**: Meu primeiro programa COBOL.
* **BHCP0002**: Cálculo simples com DISPLAY.
* **BHCP0003**: Movimentação simples.
* **BHCP0004**: Cadastro simples de cliente.
* **BHCP0005**: Cálculo de movimentação bancária.
* **BHCP0006**: Classificação de saldo com IF.
* **BHCP0007**: Tipo de conta com EVALUATE.
* **BHCP0008**: Simulação de processamento com PERFORM.
* **BHCP0009**: Cadastro de Funcionários.
* **BHCP0010**: Cadastro de Produtos.
* **BHCP0011**: Sistema Bancário.
* **BHCP0012**: Gerar arquivo batch de participantes (Base).
* **BHCP0013**: Ler participantes, validar e gerar log de rejeições.
* **BHCP0014 / BHCS0014**: Programa principal com chamada de subprograma para processamento completo.
* **BHCP0015**: Processamento de Lançamentos de Conta.
* **BHCP0016**: Geração de Arquivo JSON a partir do BHCF012S.

## Como Executar no OpenCOBOLIDE

Para o correto funcionamento dos programas que realizam leitura e escrita de arquivos (exercícios 12 ao 16):

1. Abra o código desejado da pasta `/Codigos-Revisados` no **OpenCOBOLIDE**.
2. **Importante:** Copie os arquivos da pasta `/Arquivos-txt` (como `BHCF012S.txt`) e cole-os diretamente dentro da pasta **`bin`** interna do seu compilador/ambiente do OpenCOBOLIDE (onde os executáveis são gerados).
3. Compile e rode diretamente pela interface do IDE.
