      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0009
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATEHUS KOLLMANN DETERS
      * DATA......: 30/06/2026
      * OBJETIVO..: CADASTRO E CLASSIFICACAO DE FUNCIONARIOS
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 30.06.26 MATHEUS KOLLMANN  CADASTRO, OCCURS, REDEFINES, STR
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0009.

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

       77  CTE-PROG                    PIC  X(008) VALUE 'BHCP0009'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.
       01  WS-CONSTANTES-GR.
           03 WS-TITULO-TX             PIC X(025) VALUE
              'RELATORIO DE FUNCIONARIOS'.
           03 WS-ALTO-TX               PIC X(006) VALUE 'ALTO  '.
           03 WS-LIMITE-TX             PIC X(006) VALUE 'LIMITE'.
           03 WS-NORMAL-TX             PIC X(006) VALUE 'NORMAL'.

       01  WS-MASCARA-SALARIO-TX       PIC X(008) VALUE '00000,00'.
       01  WS-EDICAO-SALARIO-GR        REDEFINES WS-MASCARA-SALARIO-TX.
           03 WS-INTEIRO-ED            PIC 9(005).
           03 FILLER                   PIC X(001).
           03 WS-DECIMAL-ED            PIC 9(002).
      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01  LS-CADASTRO-FUNCIONARIOS-GR.
           03 LS-FUNCIONARIOS-OCCURS   OCCURS 10 TIMES.
               05 LS-FUNCIONARIO-CD    PIC 9(004).
               05 LS-FUNCIONARIO-NM    PIC X(010).
               05 LS-SALARIO-VL        PIC 9(005).

       01  LS-CONTROLES-GR.
           03 LS-INDICE-IC             PIC 9(002).
           03 LS-CLASSIFICACAO-TX      PIC X(006).
           03 LS-LINHA-HEADER-TX       PIC X(050).
           03 LS-LINHA-DETALHE-TX      PIC X(050).
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.

           PERFORM 100000-PROCEDIMENTOS-INICIAIS.
           PERFORM 200000-CADASTRO-DADOS.
           PERFORM 300000-GERA-RELATORIO.
           PERFORM 900000-PROCEDIMENTOS-FINAIS.

       000099-FIM.
           GOBACK.
      ******************************************************************

      *----------------------------------------------------------------*
       100000-PROCEDIMENTOS-INICIAIS   SECTION.
      *----------------------------------------------------------------*
           MOVE ZEROS                 TO LS-CADASTRO-FUNCIONARIOS-GR
                                         LS-CONTROLES-GR.
           MOVE SPACES                TO LS-CLASSIFICACAO-TX
                                         LS-LINHA-HEADER-TX
                                         LS-LINHA-DETALHE-TX.
       100099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       200000-CADASTRO-DADOS           SECTION.
      *----------------------------------------------------------------*
           MOVE 1001 TO LS-FUNCIONARIO-CD(1).
           MOVE 'JOAO' TO LS-FUNCIONARIO-NM(1).
           MOVE 03200 TO LS-SALARIO-VL(1).

           MOVE 1002 TO LS-FUNCIONARIO-CD(2).
           MOVE 'MARIA' TO LS-FUNCIONARIO-NM(2).
           MOVE 05000 TO LS-SALARIO-VL(2).

           MOVE 1003 TO LS-FUNCIONARIO-CD(3).
           MOVE 'CARLOS' TO LS-FUNCIONARIO-NM(3).
           MOVE 08200 TO LS-SALARIO-VL(3).

           MOVE 1004 TO LS-FUNCIONARIO-CD(4).
           MOVE 'ANA' TO LS-FUNCIONARIO-NM(4).
           MOVE 04100 TO LS-SALARIO-VL(4).

           MOVE 1005 TO LS-FUNCIONARIO-CD(5).
           MOVE 'PEDRO' TO LS-FUNCIONARIO-NM(5).
           MOVE 05100 TO LS-SALARIO-VL(5).

           MOVE 1006 TO LS-FUNCIONARIO-CD(6).
           MOVE 'LUCAS' TO LS-FUNCIONARIO-NM(6).
           MOVE 02800 TO LS-SALARIO-VL(6).

           MOVE 1007 TO LS-FUNCIONARIO-CD(7).
           MOVE 'FERNANDA' TO LS-FUNCIONARIO-NM(7).
           MOVE 05000 TO LS-SALARIO-VL(7).

           MOVE 1008 TO LS-FUNCIONARIO-CD(8).
           MOVE 'PAULO' TO LS-FUNCIONARIO-NM(8).
           MOVE 09100 TO LS-SALARIO-VL(8).

           MOVE 1009 TO LS-FUNCIONARIO-CD(9).
           MOVE 'MARTA' TO LS-FUNCIONARIO-NM(9).
           MOVE 03900 TO LS-SALARIO-VL(9).

           MOVE 1010 TO LS-FUNCIONARIO-CD(10).
           MOVE 'JULIANA' TO LS-FUNCIONARIO-NM(10).
           MOVE 06000 TO LS-SALARIO-VL(10).
       200099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       300000-GERA-RELATORIO           SECTION.
      *----------------------------------------------------------------*
           STRING WS-TITULO-TX        DELIMITED BY SIZE
             INTO LS-LINHA-HEADER-TX
           END-STRING.

           DISPLAY LS-LINHA-HEADER-TX.
           DISPLAY ' '.

           PERFORM VARYING LS-INDICE-IC FROM 1 BY 1
                     UNTIL LS-INDICE-IC GREATER THAN 10

                IF LS-SALARIO-VL(LS-INDICE-IC) GREATER THAN 5000
                    MOVE WS-ALTO-TX   TO LS-CLASSIFICACAO-TX
                ELSE
                    IF LS-SALARIO-VL(LS-INDICE-IC) EQUAL TO 5000
                        MOVE WS-LIMITE-TX TO LS-CLASSIFICACAO-TX
                    ELSE
                        MOVE WS-NORMAL-TX TO LS-CLASSIFICACAO-TX
                    END-IF
                END-IF

               MOVE LS-SALARIO-VL(LS-INDICE-IC) TO WS-INTEIRO-ED
               MOVE ZERO                        TO WS-DECIMAL-ED

               MOVE SPACES TO LS-LINHA-DETALHE-TX
               STRING LS-FUNCIONARIO-CD(LS-INDICE-IC) DELIMITED BY SIZE
                      ' '                             DELIMITED BY SIZE
                      LS-FUNCIONARIO-NM(LS-INDICE-IC) DELIMITED BY SPACE
                      ' '                             DELIMITED BY SIZE
                      WS-MASCARA-SALARIO-TX           DELIMITED BY SIZE
                      ' '                             DELIMITED BY SIZE
                      LS-CLASSIFICACAO-TX             DELIMITED BY SIZE
                 INTO LS-LINHA-DETALHE-TX
               END-STRING

               DISPLAY LS-LINHA-DETALHE-TX
           END-PERFORM.
       300099-FIM.
            EXIT.

      *----------------------------------------------------------------*
       900000-PROCEDIMENTOS-FINAIS     SECTION.
      *----------------------------------------------------------------*
           DISPLAY ' '.
           DISPLAY 'PROGRAMA ENCERRADO COM SUCESSO: ' CTE-PROG.
       900099-FIM.
           EXIT.

       END PROGRAM BHCP0009.
