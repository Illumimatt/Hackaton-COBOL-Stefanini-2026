      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0011
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATHEUS KOLLMANN DETERS
      * DATA......: 30/06/2026
      * OBJETIVO..: PROGRAMA PRINCIPAL - GESTAO BANCARIA COM CALL
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 30.06.26 MATHEUS KOLLMANN  ARRAY DINAMICO, EVALUATE E CALL
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0011.

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

       77  CTE-PROG                    PIC  X(008) VALUE 'BHCP0011'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.
       01  WS-CONSTANTES-GR.
           03 WS-VIP-TX                PIC X(008) VALUE 'VIP     '.
           03 WS-ESPECIAL-TX           PIC X(008) VALUE 'ESPECIAL'.
           03 WS-PADRAO-TX             PIC X(008) VALUE 'PADRAO  '.
           03 WS-CORRENTE-TX           PIC X(010) VALUE 'CORRENTE  '.
           03 WS-POUPANCA-TX           PIC X(010) VALUE 'POUPANCA  '.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01  LS-CONTROLES-GR.
           03 LS-CONTAS-TOTAL-NR       PIC 9(002).
           03 LS-INDICE-IC             PIC 9(002).

       01  LS-PARAMETROS-CALL-GR.
           03 LS-CALL-CLIENTE-NM       PIC X(010).
           03 LS-CALL-TIPO-TX          PIC X(010).
           03 LS-CALL-SALDO-VL         PIC 9(005).
           03 LS-CALL-STATUS-TX        PIC X(008).

       01  LS-BANCO-GR.
           03 LS-CONTAS-OCCURS         OCCURS 0 TO 50 TIMES
                                       DEPENDING ON LS-CONTAS-TOTAL-NR.
               05 LS-CONTA-CD          PIC 9(005).
               05 LS-CLIENTE-NM        PIC X(010).
               05 LS-TIPO-IN           PIC X(001).
               05 LS-SALDO-VL          PIC 9(005).
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.

           PERFORM 100000-PROCEDIMENTOS-INICIAIS.
           PERFORM 200000-CADASTRO-CONTAS.
           PERFORM 300000-PROCESSA-DADOS.
           PERFORM 900000-PROCEDIMENTOS-FINAIS.

       000099-FIM.
           GOBACK.

      *----------------------------------------------------------------*
       100000-PROCEDIMENTOS-INICIAIS   SECTION.
      *----------------------------------------------------------------*
           MOVE ZERO                  TO LS-INDICE-IC.
           MOVE SPACES                TO LS-PARAMETROS-CALL-GR
           MOVE ZEROS                 TO LS-BANCO-GR.

           MOVE 10                    TO LS-CONTAS-TOTAL-NR.
       100099-FIM.
           EXIT.

      *----------------------------------------------------------------*
       200000-CADASTRO-CONTAS          SECTION.
      *----------------------------------------------------------------*
           MOVE 10001 TO LS-CONTA-CD(1).
           MOVE 'JOAO' TO LS-CLIENTE-NM(1).
           MOVE 'C' TO LS-TIPO-IN(1).
           MOVE 15000 TO LS-SALDO-VL(1).

           MOVE 10002 TO LS-CONTA-CD(2).
           MOVE 'MARIA' TO LS-CLIENTE-NM(2).
           MOVE 'P' TO LS-TIPO-IN(2).
           MOVE 10000 TO LS-SALDO-VL(2).

           MOVE 10003 TO LS-CONTA-CD(3).
           MOVE 'PEDRO' TO LS-CLIENTE-NM(3).
           MOVE 'C' TO LS-TIPO-IN(3).
           MOVE 8200 TO LS-SALDO-VL(3).

           MOVE 10004 TO LS-CONTA-CD(4).
           MOVE 'ANA' TO LS-CLIENTE-NM(4).
           MOVE 'P' TO LS-TIPO-IN(4).
           MOVE 30000 TO LS-SALDO-VL(4).

           MOVE 10005 TO LS-CONTA-CD(5).
           MOVE 'LUCAS' TO LS-CLIENTE-NM(5).
           MOVE 'C' TO LS-TIPO-IN(5).
           MOVE 9900 TO LS-SALDO-VL(5).

           MOVE 10006 TO LS-CONTA-CD(6).
           MOVE 'FERNANDA' TO LS-CLIENTE-NM(6).
           MOVE 'P' TO LS-TIPO-IN(6).
           MOVE 4500 TO LS-SALDO-VL(6).

           MOVE 10007 TO LS-CONTA-CD(7).
           MOVE 'PAULO' TO LS-CLIENTE-NM(7).
           MOVE 'C' TO LS-TIPO-IN(7).
           MOVE 12000 TO LS-SALDO-VL(7).

           MOVE 10008 TO LS-CONTA-CD(8).
           MOVE 'MARTA' TO LS-CLIENTE-NM(8).
           MOVE 'C' TO LS-TIPO-IN(8).
           MOVE 10000 TO LS-SALDO-VL(8).

           MOVE 10009 TO LS-CONTA-CD(9).
           MOVE 'JULIANA' TO LS-CLIENTE-NM(9).
           MOVE 'P' TO LS-TIPO-IN(9).
           MOVE 7800 TO LS-SALDO-VL(9).

           MOVE 10010 TO LS-CONTA-CD(10).
           MOVE 'ROBERTO' TO LS-CLIENTE-NM(10).
           MOVE 'C' TO LS-TIPO-IN(10).
           MOVE 25000 TO LS-SALDO-VL(10).
       200099-FIM.
           EXIT.

      *----------------------------------------------------------------*
       300000-PROCESSA-DADOS           SECTION.
      *----------------------------------------------------------------*
           DISPLAY '************************************'.
           DISPLAY 'RELATORIO DE CLIENTE'.

           PERFORM VARYING LS-INDICE-IC FROM 1 BY 1
                     UNTIL LS-INDICE-IC GREATER THAN LS-CONTAS-TOTAL-NR

               MOVE LS-CLIENTE-NM(LS-INDICE-IC) TO LS-CALL-CLIENTE-NM
               MOVE LS-SALDO-VL(LS-INDICE-IC)   TO LS-CALL-SALDO-VL

               EVALUATE TRUE
                   WHEN LS-SALDO-VL(LS-INDICE-IC) GREATER THAN 10000
                       MOVE WS-VIP-TX      TO LS-CALL-STATUS-TX
                   WHEN LS-SALDO-VL(LS-INDICE-IC) EQUAL TO 10000
                       MOVE WS-ESPECIAL-TX TO LS-CALL-STATUS-TX
                   WHEN OTHER
                       MOVE WS-PADRAO-TX   TO LS-CALL-STATUS-TX
               END-EVALUATE

               EVALUATE LS-TIPO-IN(LS-INDICE-IC)
                   WHEN 'C'
                       MOVE WS-CORRENTE-TX TO LS-CALL-TIPO-TX
                   WHEN 'P'
                       MOVE WS-POUPANCA-TX TO LS-CALL-TIPO-TX
               END-EVALUATE

               CALL 'BHCS0001' USING LS-PARAMETROS-CALL-GR

           END-PERFORM.
       300099-FIM.
           EXIT.

      *----------------------------------------------------------------*
       900000-PROCEDIMENTOS-FINAIS     SECTION.
      *----------------------------------------------------------------*
           DISPLAY ' '.
           DISPLAY 'PROGRAMA EXEC-11 ENCERRADO COM SUCESSO: ' CTE-PROG.
       900099-FIM.
           EXIT.

       END PROGRAM BHCP0011.
