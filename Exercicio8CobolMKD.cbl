      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0008
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATEHUS KOLLMANN DETERS
      * DATA......: 28/06/2026
      * OBJETIVO..: SIMULACAO DE PROCESSAMENTO COM PERFORM
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 28.06.26 MATHEUS KOLLMANN  EXERCICIO 08
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0008.

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

       77  CTE-PROG                    PIC  X(008) VALUE 'BHCP0008'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------------------------------*

       01 GDA-VENDAS-INDIVIDUAIS.
            03 GDA-VENDA1-VL           PIC 9(003) VALUE ZEROS.
            03 GDA-VENDA2-VL           PIC 9(003) VALUE ZEROS.
            03 GDA-VENDA3-VL           PIC 9(003) VALUE ZEROS.
            03 GDA-VENDA4-VL           PIC 9(003) VALUE ZEROS.
            03 GDA-VENDA5-VL           PIC 9(003) VALUE ZEROS.

       01 GDA-ACUMULADORES.
            03 GDA-TOTAL-VL            PIC 9(006) VALUE ZEROS.
            03 GDA-MEDIA-VL            PIC 9(004) VALUE ZEROS.
            03 GDA-VENDAS-QT           PIC 9(002) VALUE ZEROS.
            03 GDA-SITUACAO-IN         PIC X(020) VALUE SPACES.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.

            PERFORM 100000-PROCEDIMENTOS-INICIAIS.
            PERFORM 200000-PROCEDIMENTOS-PRINCIPAL.
            PERFORM 900000-PROCEDIMENTOS-FINAIS.

       000099-FIM.
            GOBACK.

      *----------------------------------------------------------------*
       100000-PROCEDIMENTOS-INICIAIS   SECTION.
      *----------------------------------------------------------------*
            MOVE 100                   TO GDA-VENDA1-VL.
            MOVE 200                   TO GDA-VENDA2-VL.
            MOVE 150                   TO GDA-VENDA3-VL.
            MOVE 300                   TO GDA-VENDA4-VL.
            MOVE 400                  TO GDA-VENDA5-VL.

       100099-FIM.
            EXIT.

      *----------------------------------------------------------------*
       200000-PROCEDIMENTOS-PRINCIPAL  SECTION.
      *----------------------------------------------------------------*
            ADD GDA-VENDA1-VL          TO GDA-TOTAL-VL.
            ADD 1                      TO GDA-VENDAS-QT.

            ADD GDA-VENDA2-VL          TO GDA-TOTAL-VL.
            ADD 1                      TO GDA-VENDAS-QT.

            ADD GDA-VENDA3-VL          TO GDA-TOTAL-VL.
            ADD 1                      TO GDA-VENDAS-QT.

            ADD GDA-VENDA4-VL          TO GDA-TOTAL-VL.
            ADD 1                      TO GDA-VENDAS-QT.

            ADD GDA-VENDA5-VL          TO GDA-TOTAL-VL.
            ADD 1                      TO GDA-VENDAS-QT.

            COMPUTE GDA-MEDIA-VL = GDA-TOTAL-VL / GDA-VENDAS-QT.

            IF GDA-TOTAL-VL GREATER THAN 1000 OR
               GDA-TOTAL-VL EQUAL TO 1000
                MOVE 'META ATINGIDA'     TO GDA-SITUACAO-IN
            ELSE
                MOVE 'META NAO ATINGIDA' TO GDA-SITUACAO-IN
            END-IF.

            DISPLAY 'RESUMO DE VENDAS'.
            DISPLAY 'QUANTIDADE: ' GDA-VENDAS-QT.
            DISPLAY 'TOTAL:      ' GDA-TOTAL-VL.
            DISPLAY 'MEDIA:      ' GDA-MEDIA-VL.
            DISPLAY 'SITUACAO:   ' GDA-SITUACAO-IN.

       200099-FIM.
            EXIT.

      *----------------------------------------------------------------*
       900000-PROCEDIMENTOS-FINAIS     SECTION.
      *----------------------------------------------------------------*
            DISPLAY 'FIM DO PROGRAMA: ' CTE-PROG
            .
       900099-FIM.
            EXIT.

       END PROGRAM BHCP0008.
