      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0010
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATHEUS KOLLMANN DETERS
      * DATA......: 30/06/2026
      * OBJETIVO..: CADASTRO DE PRODUTOS COM ARRAY DINAMICO
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 30.06.26 MATHEUS KOLLMANN  CADASTRO DE PRODUTOS
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0010.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************

      *----------------------------------------
       CONFIGURATION                   SECTION.
      *----------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************

      *----------------------------------------
       WORKING-STORAGE                 SECTION.
      *----------------------------------------

       77  CTE-PROG                    PIC  X(008) VALUE 'BHCP0010'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.
       01  WS-CONSTANTES-GR.
           03 WS-BARATO-TX             PIC X(006) VALUE 'BARATO'.
           03 WS-NORMAL-TX             PIC X(006) VALUE 'NORMAL'.
           03 WS-CARO-TX               PIC X(004) VALUE 'CARO'.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01  LS-CONTROLES-GR.
           03 LS-PRODUTOS-TOTAL-NR     PIC 9(003).
           03 LS-INDICE-IC             PIC 9(003).
           03 LS-CLASSIFICACAO-TX      PIC X(006).
           03 LS-LINHA-DETALHE-TX      PIC X(050).

       01  LS-ESTOQUE-GR.
           03 LS-PRODUTOS-OCCURS      OCCURS 0 TO 100 TIMES
                                      DEPENDING ON LS-PRODUTOS-TOTAL-NR.
               05 LS-PRODUTO-CD        PIC 9(003).
               05 LS-PRODUTO-TX        PIC X(010).
               05 LS-PRECO-VL          PIC 9(004).
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.

           PERFORM 100000-PROCEDIMENTOS-INICIAIS.
           PERFORM 200000-CADASTRO-PRODUTOS.
           PERFORM 300000-GERA-RELATORIO.
           PERFORM 900000-PROCEDIMENTOS-FINAIS.

       000099-FIM.
           GOBACK.

      *----------------------------------------------------------------*
       100000-PROCEDIMENTOS-INICIAIS   SECTION.
      *----------------------------------------------------------------*
           MOVE ZEROS                 TO LS-CONTROLES-GR
                                          LS-ESTOQUE-GR.
           MOVE SPACES                TO LS-CLASSIFICACAO-TX
                                          LS-LINHA-DETALHE-TX.

       100099-FIM.
           EXIT.

      *----------------------------------------------------------------*
       200000-CADASTRO-PRODUTOS        SECTION.
      *----------------------------------------------------------------*
           MOVE 10                    TO LS-PRODUTOS-TOTAL-NR.
           MOVE 101 TO LS-PRODUTO-CD(1).
           MOVE 'MOUSE' TO LS-PRODUTO-TX(1).
           MOVE 80 TO LS-PRECO-VL(1).

           MOVE 102 TO LS-PRODUTO-CD(2).
           MOVE 'MONITOR' TO LS-PRODUTO-TX(2).
           MOVE 900 TO LS-PRECO-VL(2).

           MOVE 103 TO LS-PRODUTO-CD(3).
           MOVE 'TECLADO' TO LS-PRODUTO-TX(3).
           MOVE 150 TO LS-PRECO-VL(3).

           MOVE 104 TO LS-PRODUTO-CD(4).
           MOVE 'NOTEBOOK' TO LS-PRODUTO-TX(4).
           MOVE 4200 TO LS-PRECO-VL(4).

           MOVE 105 TO LS-PRODUTO-CD(5).
           MOVE 'HEADSET' TO LS-PRODUTO-TX(5).
           MOVE 280 TO LS-PRECO-VL(5).

           MOVE 106 TO LS-PRODUTO-CD(6).
           MOVE 'WEBCAM' TO LS-PRODUTO-TX(6).
           MOVE 320 TO LS-PRECO-VL(6).

           MOVE 107 TO LS-PRODUTO-CD(7).
           MOVE 'SSD' TO LS-PRODUTO-TX(7).
           MOVE 550 TO LS-PRECO-VL(7).

           MOVE 108 TO LS-PRODUTO-CD(8).
           MOVE 'PENDRIVE' TO LS-PRODUTO-TX(8).
           MOVE 60 TO LS-PRECO-VL(8).

           MOVE 109 TO LS-PRODUTO-CD(9).
           MOVE 'HD EXTERNO' TO LS-PRODUTO-TX(9).
           MOVE 480 TO LS-PRECO-VL(9).

           MOVE 110 TO LS-PRODUTO-CD(10).
           MOVE 'IMPRESSORA' TO LS-PRODUTO-TX(10).
           MOVE 1100 TO LS-PRECO-VL(10).
       200099-FIM.
           EXIT.

      *----------------------------------------------------------------*
       300000-GERA-RELATORIO           SECTION.
      *----------------------------------------------------------------*
           PERFORM VARYING LS-INDICE-IC FROM 1 BY 1
                   UNTIL LS-INDICE-IC GREATER THAN LS-PRODUTOS-TOTAL-NR


               EVALUATE TRUE
                   WHEN LS-PRECO-VL(LS-INDICE-IC) NOT GREATER THAN 99
                       MOVE WS-BARATO-TX TO LS-CLASSIFICACAO-TX

                   WHEN LS-PRECO-VL(LS-INDICE-IC) NOT LESS THAN 100 AND
                        LS-PRECO-VL(LS-INDICE-IC) NOT GREATER THAN 500
                       MOVE WS-NORMAL-TX TO LS-CLASSIFICACAO-TX

                   WHEN OTHER
                       MOVE WS-CARO-TX   TO LS-CLASSIFICACAO-TX
               END-EVALUATE

                IF LS-CLASSIFICACAO-TX NOT EQUAL TO SPACES
                    MOVE SPACES TO LS-LINHA-DETALHE-TX

                  STRING LS-PRODUTO-CD(LS-INDICE-IC) DELIMITED BY SIZE
                         ' '                          DELIMITED BY SIZE
                         LS-PRODUTO-TX(LS-INDICE-IC)  DELIMITED BY SPACE
                         ' '                          DELIMITED BY SIZE
                         LS-CLASSIFICACAO-TX          DELIMITED BY SIZE
                     INTO LS-LINHA-DETALHE-TX
                   END-STRING

                   DISPLAY LS-LINHA-DETALHE-TX
               END-IF

           END-PERFORM.
       300099-FIM.
           EXIT.

      *----------------------------------------------------------------*
       900000-PROCEDIMENTOS-FINAIS     SECTION.
      *----------------------------------------------------------------*
           DISPLAY ' '.
           DISPLAY 'PROGRAMA EXEC-10 ENCERRADO COM SUCESSO: ' CTE-PROG.
       900099-FIM.
           EXIT.

       END PROGRAM BHCP0010.
