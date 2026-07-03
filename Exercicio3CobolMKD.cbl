      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0003
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATEHUS KOLLMANN DETERS
      * DATA......: 28/06/2026
      * OBJETIVO..: MOVIMENTACAO SIMPLES
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 28.06.26 MATHEUS KOLLMANN  EXERCICIO 03
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0003.

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

       77  CTE-PROG                    PIC  X(008) VALUE 'BHCP0003'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.

       77 WS-ALUNO-NM                  PIC  X(020) VALUE SPACES.
       77 WS-CURSO-NM                  PIC  X(030) VALUE SPACES.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------

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

            MOVE 'MATHEUS K. DETERS'   TO WS-ALUNO-NM.
            MOVE 'BOOTCAMP COBOL'      TO WS-CURSO-NM.

       100099-FIM.
            EXIT.

      *----------------------------------------------------------------*
       200000-PROCEDIMENTOS-PRINCIPAL  SECTION.
      *----------------------------------------------------------------*
            DISPLAY 'ALUNO: ' WS-ALUNO-NM.
            DISPLAY 'CURSO: ' WS-CURSO-NM.
            DISPLAY 'MEU PROGRAMA COBOL'.

       200099-FIM.
            EXIT.

      *----------------------------------------------------------------*
       900000-PROCEDIMENTOS-FINAIS     SECTION.
      *----------------------------------------------------------------*
            DISPLAY 'FIM DO PROGRAMA: ' CTE-PROG.

       900099-FIM.
            EXIT.

       END PROGRAM BHCP0003.
