      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCS0001
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATHEUS KOLLMANN DETERS
      * DATA......: 30/06/2026
      * OBJETIVO..: SUBPROGRAMA DE IMPRESSAO DE CLIENTES
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 30.06.26 MATHEUS KOLLMANN  ESTRUTURA LINKAGE E STRING
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************

       PROGRAM-ID. BHCS0001.

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
       01  WS-CONSTANTES-GR.
           03 WS-SEPARADOR-TX          PIC X(007) VALUE '*******'.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01  LS-MONTAGEM-GR.
           03 LS-LINHA-TXT             PIC X(050).
           03 LS-SALDO-ALFA-TX         PIC X(005).

      *----------------------------------------
       LINKAGE                         SECTION.
      *----------------------------------------

       01  LK-PARAMETROS-GR.
           03 LK-CLIENTE-NM            PIC X(010).
           03 LK-TIPO-CONTA-TX         PIC X(010).
           03 LK-SALDO-VL              PIC 9(005).
           03 LK-STATUS-TX             PIC X(008).

      ******************************************************************
       PROCEDURE DIVISION USING LK-PARAMETROS-GR.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.

            PERFORM 100000-MONTA-RELATORIO.

       000099-FIM.
            GOBACK.

      *----------------------------------------------------------------*
       100000-MONTA-RELATORIO          SECTION.
      *----------------------------------------------------------------*
           MOVE LK-SALDO-VL           TO LS-SALDO-ALFA-TX.

           MOVE SPACES TO LS-LINHA-TXT.
           STRING 'CLIENTE: '         DELIMITED BY SIZE
                  LK-CLIENTE-NM       DELIMITED BY SPACE
             INTO LS-LINHA-TXT
           END-STRING.
           DISPLAY LS-LINHA-TXT.

           MOVE SPACES TO LS-LINHA-TXT.
           STRING 'TIPO   : '         DELIMITED BY SIZE
                  LK-TIPO-CONTA-TX    DELIMITED BY SPACE
             INTO LS-LINHA-TXT
           END-STRING.
           DISPLAY LS-LINHA-TXT.

           MOVE SPACES TO LS-LINHA-TXT.
           STRING 'SALDO  : '         DELIMITED BY SIZE
                  LS-SALDO-ALFA-TX    DELIMITED BY SIZE
             INTO LS-LINHA-TXT
           END-STRING.
           DISPLAY LS-LINHA-TXT.

           MOVE SPACES TO LS-LINHA-TXT.
           STRING 'STATUS : '         DELIMITED BY SIZE
                  LK-STATUS-TX        DELIMITED BY SPACE
             INTO LS-LINHA-TXT
           END-STRING.
           DISPLAY LS-LINHA-TXT.

           DISPLAY WS-SEPARADOR-TX.

       100099-FIM.
           EXIT.

       END PROGRAM BHCS0001.
