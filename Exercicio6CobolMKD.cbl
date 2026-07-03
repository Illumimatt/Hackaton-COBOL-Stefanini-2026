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
      * 001 28.06.26 MATHEUS KOLLMANN  EXERCICIO 06
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

       77  CTE-PROG                    PIC  X(008) VALUE 'BHCP0006'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01 GDA-CLIENTE.
            03 GDA-CLIENTE-NM          PIC X(020) VALUE SPACES.
            03 GDA-SALDO-VL            PIC S9(003) VALUE ZEROS.
            03 GDA-SITUACAO-IN         PIC X(010) VALUE SPACES.
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
            MOVE 'ANA PAULA'           TO GDA-CLIENTE-NM.
            MOVE 500                   TO GDA-SALDO-VL.

       100099-FIM.
            EXIT.

      *----------------------------------------------------------------*
       200000-PROCEDIMENTOS-PRINCIPAL  SECTION.
      *----------------------------------------------------------------*
            IF GDA-SALDO-VL GREATER THAN ZERO
                MOVE 'POSITIVO' TO GDA-SITUACAO-IN
            ELSE
                IF GDA-SALDO-VL LESS THAN ZERO
                    MOVE 'NEGATIVO' TO GDA-SITUACAO-IN
                ELSE
                    MOVE 'ZERADO'   TO GDA-SITUACAO-IN
                END-IF
            END-IF.

            DISPLAY 'CLIENTE: '  GDA-CLIENTE-NM.
            DISPLAY 'SALDO: '    GDA-SALDO-VL.
            DISPLAY 'SITUACAO: ' GDA-SITUACAO-IN.

       200099-FIM.
            EXIT.

      *----------------------------------------------------------------*
       900000-PROCEDIMENTOS-FINAIS     SECTION.
      *----------------------------------------------------------------*
            DISPLAY 'FIM DO PROGRAMA: ' CTE-PROG.

       900099-FIM.
            EXIT.

       END PROGRAM BHCP0006.
