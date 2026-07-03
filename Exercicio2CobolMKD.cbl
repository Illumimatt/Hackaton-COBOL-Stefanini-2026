      ******************************************************************
      * SIGLA.....: BHC - BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP002
      * ANALISTA..: NOME DO ANALISTA
      * AUTOR.....: MATHEUS KOLLMANN DETERS
      * DATA......: 26/06/2026
      * OBJETIVO..: SOMA SIMPLES ENTRE DUAS VARIAVEIS
      * EXECUCAO..: COBOL - BATCH
      ******************************************************************
      * VRS DATA       RESPONSAVEL      DESCRICAO DA VERSAO
      * --- ---------- ---------------- --------------------------------
      * 001 25.06.26   MATHEUS          SOMA SIMPLES
      * --- ---------- ---------------- --------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP002.

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

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       77 WS-NUM1      PIC 9(02) VALUE ZEROS.
       77 WS-NUM2      PIC 9(02) VALUE ZEROS.
       77 WS-RESULT    PIC 9(02) VALUE ZEROS.
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           MOVE 10 TO WS-NUM1.
           MOVE 25 TO WS-NUM2.
           COMPUTE WS-RESULT = WS-NUM1 + WS-NUM2.
           DISPLAY "RESULTADO DA SOMA: " WS-RESULT.
           GOBACK.
       END PROGRAM BHCP002.
