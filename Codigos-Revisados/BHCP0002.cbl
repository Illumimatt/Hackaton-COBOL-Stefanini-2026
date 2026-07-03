      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP002
      * ANALISTA..: NOME DO ANALISTA
      * AUTOR.....: MATHEUS KOLLMANN DETERS
      * DATA......: 26/06/2026
      * OBJETIVO..: SOMA SIMPLES ENTRE DUAS VARIAVEIS
      * EXECUCAO..: COBOL - BATCH
      ******************************************************************
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 26.06.26 MATHEUS KOLLMANN  IMPLANTACAO INICIAL
      * 002 03.07.26 MATHEUS KOLLMANN  SEGUNDO UPDATE POS AULA FINAL
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. BHCP002.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  CTE-PROG                    PIC X(008) VALUE 'BHCP0002'.
       77  CTE-VERS                    PIC X(006) VALUE 'VRS002'.
       77  WS-NUM1                     PIC 9(02) VALUE 10.
       77  WS-NUM2                     PIC 9(02) VALUE 25.
       77  WS-RESULT                   PIC 9(03) VALUE ZEROS.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
           PERFORM 100000-INICIALIZAR.
           PERFORM 500000-PROCESSAR-SOMA.
           PERFORM 999999-FINALIZAR.

       000099-FIM.
            GOBACK.

      *----------------------------------------------------------------*
       100000-INICIALIZAR SECTION.
      *----------------------------------------------------------------*
           DISPLAY CTE-PROG' - INICIO DO PROCESSAMENTO'.
       100099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       500000-PROCESSAR-SOMA SECTION.
      *----------------------------------------------------------------*
           COMPUTE WS-RESULT = WS-NUM1 + WS-NUM2.
           DISPLAY 'RESULTADO DA SOMA: ' WS-RESULT.
       500099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       999999-FINALIZAR SECTION.
      *----------------------------------------------------------------*
           DISPLAY CTE-PROG' - FIM DO PROCESSAMENTO'.
       900099-FIM.
            EXIT.

       END PROGRAM BHCP002.
