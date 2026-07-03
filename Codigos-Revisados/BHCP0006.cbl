      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0006
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATEHUS KOLLMANN DETERS
      * DATA......: 28/06/2026
      * OBJETIVO..: CLASSIFICACAO DE SALDO COM IF
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
       PROGRAM-ID. BHCP0006.

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
       77  CTE-PROG                    PIC X(008) VALUE 'BHCP0006'.
       77  CTE-VERS                    PIC X(006) VALUE 'VRS001'.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01  GDA-CLIENTE.
           05  GDA-CLIENTE-NM          PIC X(020) VALUE SPACES.
           05  GDA-SALDO-VL            PIC S9(007)V99 VALUE ZEROS.
           05  GDA-SITUACAO-NM         PIC X(010) VALUE SPACES.

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
           MOVE 'ANA PAULA'            TO GDA-CLIENTE-NM.
           MOVE 500,00                 TO GDA-SALDO-VL.

       100099-FIM.
           EXIT.

      *----------------------------------------------------------------*
       200000-PROCEDIMENTOS-PRINCIPAL  SECTION.
      *----------------------------------------------------------------*
           IF GDA-SALDO-VL > 0
               MOVE 'POSITIVO'         TO GDA-SITUACAO-NM
           ELSE
               IF GDA-SALDO-VL < 0
                   MOVE 'NEGATIVO'     TO GDA-SITUACAO-NM
               ELSE
                   MOVE 'ZERADO'       TO GDA-SITUACAO-NM
               END-IF
           END-IF.

           DISPLAY 'CLIENTE: '  GDA-CLIENTE-NM.
           DISPLAY 'SALDO: '    GDA-SALDO-VL.
           DISPLAY 'SITUACAO: ' GDA-SITUACAO-NM.

       200099-FIM.
           EXIT.

      *----------------------------------------------------------------*
       900000-PROCEDIMENTOS-FINAIS     SECTION.
      *----------------------------------------------------------------*
           DISPLAY 'FIM DO PROGRAMA: ' CTE-PROG.

       900099-FIM.
           EXIT.

       END PROGRAM BHCP0006.
