      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0007
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATEHUS KOLLMANN DETERS
      * DATA......: 28/06/2026
      * OBJETIVO..: TIPO DE CONTA COM EVALUATE
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 28.06.26 MATHEUS KOLLMANN  EXERCICIO 7
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0007.

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

       77  CTE-PROG                    PIC  X(008) VALUE 'BHCP0007'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------

       01 GDA-CONTA.
            03 GDA-TIPO-CD      PIC 9(001) VALUE ZEROS.
            03 GDA-DESCRICAO-TX PIC X(020) VALUE SPACES.

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
            MOVE 2             TO GDA-TIPO-CD.

       100099-FIM.
            EXIT.

      *----------------------------------------------------------------*
       200000-PROCEDIMENTOS-PRINCIPAL  SECTION.
      *----------------------------------------------------------------*
            EVALUATE GDA-TIPO-CD
                WHEN 1
                    MOVE 'CONTA CORRENTE'   TO GDA-DESCRICAO-TX
                WHEN 2
                    MOVE 'CONTA POUPANCA'   TO GDA-DESCRICAO-TX
                WHEN 3
                    MOVE 'CONTA SALARIO'    TO GDA-DESCRICAO-TX
                WHEN 4
                    MOVE 'CONTA EMPRESARIAL' TO GDA-DESCRICAO-TX
                WHEN OTHER
                    MOVE 'TIPO INVALIDO'    TO GDA-DESCRICAO-TX
            END-EVALUATE.

            DISPLAY 'CODIGO DO TIPO: ' GDA-TIPO-CD.
            DISPLAY 'DESCRICAO: '      GDA-DESCRICAO-TX.

       200099-FIM.
            EXIT.

      *----------------------------------------------------------------*
       900000-PROCEDIMENTOS-FINAIS     SECTION.
      *----------------------------------------------------------------*
            DISPLAY 'FIM DO PROGRAMA: ' CTE-PROG.

       900099-FIM.
            EXIT.

       END PROGRAM BHCP0007.
