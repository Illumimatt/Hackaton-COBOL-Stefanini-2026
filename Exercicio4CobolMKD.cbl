      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0004
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATEHUS KOLLMANN DETERS
      * DATA......: 28/06/2026
      * OBJETIVO..: CADASTRO SIMPLES DE CLIENTE
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 28.06.26 MATHEUS KOLLMANN  EXERCICIO 04
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCP0004.

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

       77  CTE-PROG                    PIC  X(008) VALUE 'BHCP0004'.
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------

       01 GDA-CLIENTE.
            03 GDA-CLIENTE-CD          PIC X(004) VALUE SPACES.
            03 GDA-CLIENTE-NM          PIC X(020) VALUE SPACES.
            03 GDA-AGENCIA-NR          PIC 9(004) VALUE ZEROS.
            03 GDA-CONTA-NR            PIC 9(006) VALUE ZEROS.
            03 GDA-SALDO-VL            PIC 9(004) VALUE ZEROS.

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
            MOVE '1001'                TO GDA-CLIENTE-CD.
            MOVE 'JOAO SILVA'          TO GDA-CLIENTE-NM.
            MOVE 1234                  TO GDA-AGENCIA-NR.
            MOVE 987654                TO GDA-CONTA-NR.
            MOVE 1500                  TO GDA-SALDO-VL.

       100099-FIM.
            EXIT.

      *----------------------------------------------------------------*
       200000-PROCEDIMENTOS-PRINCIPAL  SECTION.
      *----------------------------------------------------------------*
            DISPLAY 'CADASTRO DE CLIENTE'
            DISPLAY 'CODIGO: '        GDA-CLIENTE-CD.
            DISPLAY 'NOME: '          GDA-CLIENTE-NM.
            DISPLAY 'AGENCIA: '       GDA-AGENCIA-NR.
            DISPLAY 'CONTA: '         GDA-CONTA-NR.
            DISPLAY 'SALDO INICIAL: ' GDA-SALDO-VL.

       200099-FIM.
            EXIT.

      *----------------------------------------------------------------*
       900000-PROCEDIMENTOS-FINAIS     SECTION.
      *----------------------------------------------------------------*
            DISPLAY 'FIM DO PROGRAMA: ' CTE-PROG.

       900099-FIM.
            EXIT.

       END PROGRAM BHCP0004.
