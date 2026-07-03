      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0005
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATEHUS KOLLMANN DETERS
      * DATA......: 28/06/2026
      * OBJETIVO..: CALCULO DE MOVIMENTACAO BANCARIA
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 28.06.26 MATHEUS KOLLMANN  IMPLANTACAO INICIAL
      * 002 03.07.26 MATHEUS KOLLMANN  SEGUNDO UPDATE POS AULA FINAL
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. BHCP0005.

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
       77  CTE-PROG                    PIC X(008) VALUE 'BHCP0005'.
       77  CTE-VERS                    PIC X(006) VALUE 'VRS001'.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01  GDA-CONTA.
           05  GDA-SALDO-INICIAL-VL    PIC 9(007)V99 VALUE ZEROS.
           05  GDA-DEPOSITO-VL         PIC 9(007)V99 VALUE ZEROS.
           05  GDA-SAQUE-VL            PIC 9(007)V99 VALUE ZEROS.
           05  GDA-SALDO-FINAL-VL      PIC 9(007)V99 VALUE ZEROS.

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
           MOVE 1000,00                TO GDA-SALDO-INICIAL-VL.
           MOVE 500,00                 TO GDA-DEPOSITO-VL.
           MOVE 200,00                 TO GDA-SAQUE-VL.
           MOVE GDA-SALDO-INICIAL-VL   TO GDA-SALDO-FINAL-VL.

       100099-FIM.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCEDIMENTOS-PRINCIPAL  SECTION.
      *----------------------------------------------------------------*
           ADD GDA-DEPOSITO-VL         TO GDA-SALDO-FINAL-VL.
           SUBTRACT GDA-SAQUE-VL       FROM GDA-SALDO-FINAL-VL.

           DISPLAY 'SALDO INICIAL: '  GDA-SALDO-INICIAL-VL.
           DISPLAY 'DEPOSITO: '       GDA-DEPOSITO-VL.
           DISPLAY 'SAQUE: '          GDA-SAQUE-VL.
           DISPLAY 'SALDO FINAL: '    GDA-SALDO-FINAL-VL.

       200099-FIM.
           EXIT.

      *----------------------------------------------------------------*
       900000-PROCEDIMENTOS-FINAIS     SECTION.
      *----------------------------------------------------------------*
           DISPLAY 'FIM DO PROGRAMA: ' CTE-PROG.

       900099-FIM.
           EXIT.

       END PROGRAM BHCP0005.
