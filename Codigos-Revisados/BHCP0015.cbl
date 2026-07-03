      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0015
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATHEUS KOLLMANN DETERS
      * DATA......: 02/07/2026
      * OBJETIVO..: PROCESSAMENTO DE LANCAMENTOS DE CONTA FICTICIA
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 02.07.26 MATHEUS KOLLMANN  IMPLANTACAO
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. BHCP0015.
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
      *----------------------------------------
       CONFIGURATION                   SECTION.
      *----------------------------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------
       FILE-CONTROL.
           SELECT BHCF015E ASSIGN TO "BHCF015E.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS WS-FS-ENTRADA.

           SELECT BHCF015S ASSIGN TO "BHCF015S.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS WS-FS-SAIDA.

           SELECT BHCF015L ASSIGN TO "BHCF015L.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS WS-FS-LOG.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
      *----------------------------------------
       FILE                            SECTION.
      *----------------------------------------
       FD  BHCF015E.
       01  REG-BHCF015E                PIC X(053).

       FD  BHCF015S.
       01  REG-BHCF015S                PIC X(062).

       FD  BHCF015L.
       01  REG-BHCF015L                PIC X(067).

      *----------------------------------------
       WORKING-STORAGE                 SECTION.
      *----------------------------------------
       77  CTE-PROG                    PIC X(008) VALUE 'BHCP0015'.
       77  CTE-VERS                    PIC X(006) VALUE 'VRS001'.

       01  WS-FILE-STATUS.
           05  WS-FS-ENTRADA           PIC X(002) VALUE SPACES.
           05  WS-FS-SAIDA             PIC X(002) VALUE SPACES.
           05  WS-FS-LOG               PIC X(002) VALUE SPACES.

       01  WS-CONSTANTES.
           05  WS-SUCESSO              PIC X(002) VALUE '00'.
           05  WS-FIM-ARQ              PIC X(002) VALUE '10'.

       01  WS-AREA-LEITURA.
           05  IN-COD-CONTA            PIC 9(005).
           05  IN-DATA-LANC            PIC 9(008).
           05  IN-TIPO-LANC            PIC X(001).
           05  IN-VALOR-LANC           PIC 9(007)V99.
           05  IN-HISTORICO            PIC X(030).

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01  GDA-CONTROLES-SISTE.
           05  GDA-IN-FIM-ARQ          PIC X(001) VALUE 'N'.
               88  GDA-FIM-SIM                    VALUE 'S'.
               88  GDA-FIM-NAO                    VALUE 'N'.
           05  GDA-IN-REG-VALIDO       PIC X(001) VALUE 'S'.
               88  GDA-REG-VALIDO                 VALUE 'S'.
               88  GDA-REG-INVALIDO               VALUE 'N'.

       01  GDA-TOTALIZADORES.
           05  GDA-TOT-LIDOS           PIC 9(005) VALUE ZEROS.
           05  GDA-TOT-VALIDOS         PIC 9(005) VALUE ZEROS.
           05  GDA-TOT-REJEITADOS      PIC 9(005) VALUE ZEROS.
           05  GDA-TOT-DEPOSITOS       PIC 9(005) VALUE ZEROS.
           05  GDA-TOT-SAQUES          PIC 9(005) VALUE ZEROS.
           05  GDA-TOT-TRANSFERENCIAS  PIC 9(005) VALUE ZEROS.
           05  GDA-TOT-ERROS-ARQ       PIC 9(005) VALUE ZEROS.

       01  GDA-VALORES.
           05  GDA-VL-SALDO-INICIAL    PIC 9(007)V99 VALUE 1000,00.
           05  GDA-VL-SALDO-ATUAL      PIC 9(007)V99 VALUE ZEROS.
           05  GDA-TOT-VL-DEPOSITOS    PIC 9(007)V99 VALUE ZEROS.
           05  GDA-TOT-VL-SAQUES       PIC 9(007)V99 VALUE ZEROS.
           05  GDA-TOT-VL-TRANSFERENCIA PIC 9(007)V99 VALUE ZEROS.

       01  GDA-DADOS-ERRO.
           05  GDA-CD-ERRO             PIC X(004) VALUE SPACES.
           05  GDA-TX-MENSAGEM         PIC X(040) VALUE SPACES.

       01  GDA-SAIDAS-AUXILIARES.
           05  GDA-REG-SAIDA.
               10  OUT-COD-CONTA       PIC 9(005).
               10  OUT-DATA-LANC       PIC 9(008).
               10  OUT-TIPO-LANC       PIC X(001).
               10  OUT-VALOR-LANC      PIC 9(007)V99.
               10  OUT-HISTORICO       PIC X(030).
               10  OUT-SALDO-APOS      PIC 9(007)V99.
           05  GDA-REG-LOG.
               10  LOG-COD-CONTA       PIC 9(005).
               10  LOG-DATA-LANC       PIC 9(008).
               10  LOG-TIPO-LANC       PIC X(001).
               10  LOG-VALOR-LANC      PIC 9(007)V99.
               10  LOG-CD-ERRO         PIC X(004).
               10  LOG-MENSAGEM        PIC X(040).

       01  GDA-MASCARAS-EDICAO.
           05  GDA-ED-TOT-REGISTROS    PIC ZZ.ZZ9.
           05  GDA-ED-VALOR-FINANCEIRO PIC Z.ZZZ.ZZ9,99.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
           PERFORM 100000-INICIALIZAR.
           PERFORM 200000-ABRIR-ARQUIVOS.
           PERFORM 300000-PROCESSAR-ARQUIVO.
           PERFORM 900000-FECHAR-ARQUIVOS.
           PERFORM 910000-EXIBIR-TOTALIZADORES.
           PERFORM 920000-PROCEDIMENTOS-FINAIS.
       000099-FIM.
           GOBACK.

      *----------------------------------------------------------------*
       100000-INICIALIZAR              SECTION.
      *----------------------------------------------------------------*
           MOVE GDA-VL-SALDO-INICIAL   TO GDA-VL-SALDO-ATUAL
           SET GDA-FIM-NAO             TO TRUE.
       100099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       200000-ABRIR-ARQUIVOS           SECTION.
      *----------------------------------------------------------------*
           OPEN INPUT  BHCF015E
           IF WS-FS-ENTRADA NOT = WS-SUCESSO
              DISPLAY 'ERRO OPEN INPUT BHCF015E FS=' WS-FS-ENTRADA
              ADD 1                    TO GDA-TOT-ERROS-ARQ
              SET GDA-FIM-SIM          TO TRUE
           END-IF

           OPEN OUTPUT BHCF015S
           IF WS-FS-SAIDA NOT = WS-SUCESSO
              DISPLAY 'ERRO OPEN OUTPUT BHCF015S FS=' WS-FS-SAIDA
              ADD 1                    TO GDA-TOT-ERROS-ARQ
              SET GDA-FIM-SIM          TO TRUE
           END-IF

           OPEN OUTPUT BHCF015L
           IF WS-FS-LOG NOT = WS-SUCESSO
              DISPLAY 'ERRO OPEN OUTPUT BHCF015L FS=' WS-FS-LOG
              ADD 1                    TO GDA-TOT-ERROS-ARQ
              SET GDA-FIM-SIM          TO TRUE
           END-IF.
       200099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       300000-PROCESSAR-ARQUIVO SECTION.
      *----------------------------------------------------------------*
           PERFORM UNTIL GDA-FIM-SIM
               PERFORM 310000-LER-ENTRADA

               IF GDA-FIM-NAO
                   PERFORM 320000-VALIDAR-LANCAMENTO

                   IF GDA-REG-VALIDO
                       PERFORM 330000-PROCESSAR-LANCAMENTO
                   ELSE
                       PERFORM 500000-GRAVAR-LOG
                   END-IF
               END-IF
           END-PERFORM.
       300099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       310000-LER-ENTRADA              SECTION.
      *----------------------------------------------------------------*
           READ BHCF015E INTO WS-AREA-LEITURA
                AT END
                   SET GDA-FIM-SIM     TO TRUE
                NOT AT END
                   ADD 1               TO GDA-TOT-LIDOS
           END-READ

           IF WS-FS-ENTRADA NOT = WS-SUCESSO
           AND WS-FS-ENTRADA NOT = WS-FIM-ARQ
              DISPLAY 'ERRO READ BHCF015E FS=' WS-FS-ENTRADA
              ADD 1                    TO GDA-TOT-ERROS-ARQ
              SET GDA-FIM-SIM          TO TRUE
           END-IF.
       310099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       320000-VALIDAR-LANCAMENTO       SECTION.
      *----------------------------------------------------------------*
           SET GDA-REG-VALIDO          TO TRUE
           MOVE SPACES                 TO GDA-CD-ERRO
                                          GDA-TX-MENSAGEM

           EVALUATE TRUE
              WHEN IN-COD-CONTA = ZEROS
                 SET GDA-REG-INVALIDO  TO TRUE
                 MOVE 'E001'           TO GDA-CD-ERRO
                 MOVE 'CODIGO DA CONTA ZERADO' TO GDA-TX-MENSAGEM

              WHEN IN-DATA-LANC = ZEROS
                 SET GDA-REG-INVALIDO  TO TRUE
                 MOVE 'E002'           TO GDA-CD-ERRO
                 MOVE 'DATA DO LANCAMENTO ZERADA' TO GDA-TX-MENSAGEM

              WHEN IN-TIPO-LANC NOT = 'D' AND
                   IN-TIPO-LANC NOT = 'S' AND
                   IN-TIPO-LANC NOT = 'T'
                 SET GDA-REG-INVALIDO  TO TRUE
                 MOVE 'E003'           TO GDA-CD-ERRO
                 MOVE 'TIPO DE OPERACAO INVALIDO' TO GDA-TX-MENSAGEM

              WHEN IN-VALOR-LANC <= ZEROS
                 SET GDA-REG-INVALIDO  TO TRUE
                 MOVE 'E004'           TO GDA-CD-ERRO
                 MOVE 'VALOR DO LANCAMENTO INVALIDO' TO GDA-TX-MENSAGEM

              WHEN IN-HISTORICO = SPACES
                 SET GDA-REG-INVALIDO  TO TRUE
                 MOVE 'E005'           TO GDA-CD-ERRO
                 MOVE 'HISTORICO NAO PREENCHIDO' TO GDA-TX-MENSAGEM

              WHEN (IN-TIPO-LANC = 'S' OR IN-TIPO-LANC = 'T') AND
                   (IN-VALOR-LANC > GDA-VL-SALDO-ATUAL)
                 SET GDA-REG-INVALIDO  TO TRUE
                 MOVE 'E006'           TO GDA-CD-ERRO
                 MOVE 'SALDO INSUFICIENTE' TO GDA-TX-MENSAGEM
           END-EVALUATE.
       320099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       330000-PROCESSAR-LANCAMENTO     SECTION.
      *----------------------------------------------------------------*
           EVALUATE IN-TIPO-LANC
              WHEN 'D'
                 ADD IN-VALOR-LANC     TO GDA-VL-SALDO-ATUAL
                 ADD IN-VALOR-LANC     TO GDA-TOT-VL-DEPOSITOS
                 ADD 1                 TO GDA-TOT-DEPOSITOS

              WHEN 'S'
                 SUBTRACT IN-VALOR-LANC FROM GDA-VL-SALDO-ATUAL
                 ADD IN-VALOR-LANC     TO GDA-TOT-VL-SAQUES
                 ADD 1                 TO GDA-TOT-SAQUES

              WHEN 'T'
                 SUBTRACT IN-VALOR-LANC FROM GDA-VL-SALDO-ATUAL
                 ADD IN-VALOR-LANC     TO GDA-TOT-VL-TRANSFERENCIA
                 ADD 1                 TO GDA-TOT-TRANSFERENCIAS
           END-EVALUATE

           ADD 1                       TO GDA-TOT-VALIDOS
           PERFORM 400000-GRAVAR-SAIDA.
       330099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       400000-GRAVAR-SAIDA             SECTION.
      *----------------------------------------------------------------*
           MOVE IN-COD-CONTA           TO OUT-COD-CONTA
           MOVE IN-DATA-LANC           TO OUT-DATA-LANC
           MOVE IN-TIPO-LANC           TO OUT-TIPO-LANC
           MOVE IN-VALOR-LANC          TO OUT-VALOR-LANC
           MOVE IN-HISTORICO           TO OUT-HISTORICO
           MOVE GDA-VL-SALDO-ATUAL     TO OUT-SALDO-APOS

           WRITE REG-BHCF015S FROM GDA-REG-SAIDA

           IF WS-FS-SAIDA = WS-SUCESSO
              CONTINUE
           ELSE
              DISPLAY 'ERRO WRITE BHCF015S FS=' WS-FS-SAIDA
              ADD 1                    TO GDA-TOT-ERROS-ARQ
           END-IF.
       400099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       500000-GRAVAR-LOG               SECTION.
      *----------------------------------------------------------------*
           MOVE IN-COD-CONTA           TO LOG-COD-CONTA
           MOVE IN-DATA-LANC           TO LOG-DATA-LANC
           MOVE IN-TIPO-LANC           TO LOG-TIPO-LANC
           MOVE IN-VALOR-LANC          TO LOG-VALOR-LANC
           MOVE GDA-CD-ERRO            TO LOG-CD-ERRO
           MOVE GDA-TX-MENSAGEM        TO LOG-MENSAGEM

           WRITE REG-BHCF015L FROM GDA-REG-LOG

           IF WS-FS-LOG = WS-SUCESSO
              ADD 1                    TO GDA-TOT-REJEITADOS
           ELSE
              DISPLAY 'ERRO WRITE BHCF015L FS=' WS-FS-LOG
              ADD 1                    TO GDA-TOT-ERROS-ARQ
           END-IF.
       500099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       900000-FECHAR-ARQUIVOS SECTION.
      *----------------------------------------------------------------*
           CLOSE BHCF015E
                 BHCF015S
                 BHCF015L.
       900099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       910000-EXIBIR-TOTALIZADORES SECTION.
      *----------------------------------------------------------------*
           DISPLAY CTE-PROG ' - PROCESSAMENTO DE LANCAMENTOS'
           DISPLAY '---------------------------------------'
           DISPLAY 'TOTAL DE REGISTROS LIDOS..........: '
                   GDA-TOT-LIDOS

           DISPLAY 'TOTAL DE REGISTROS VALIDOS........: '
                   GDA-TOT-VALIDOS

           DISPLAY 'TOTAL DE REGISTROS REJEITADOS.....: '
                   GDA-TOT-REJEITADOS

           DISPLAY 'TOTAL DE DEPOSITOS................: '
                   GDA-TOT-DEPOSITOS

           DISPLAY 'TOTAL DE SAQUES...................: '
                   GDA-TOT-SAQUES

           DISPLAY 'TOTAL DE TRANSFERENCIAS...........: '
                   GDA-TOT-TRANSFERENCIAS

           DISPLAY 'VALOR TOTAL DEPOSITOS.............: '
                   GDA-TOT-VL-DEPOSITOS

           DISPLAY 'VALOR TOTAL SAQUES................: '
                   GDA-TOT-VL-SAQUES

           DISPLAY 'VALOR TOTAL TRANSFERENCIAS........: '
                   GDA-TOT-VL-TRANSFERENCIA

           DISPLAY 'SALDO INICIAL.....................: '
                   GDA-VL-SALDO-INICIAL

           DISPLAY 'SALDO FINAL.......................: '
                   GDA-VL-SALDO-ATUAL

           DISPLAY 'TOTAL DE ERROS DE ARQUIVO.........: '
                   GDA-TOT-ERROS-ARQ
           DISPLAY '---------------------------------------'.
       910099-FIM.
           EXIT.
      *----------------------------------------------------------------*
       920000-PROCEDIMENTOS-FINAIS SECTION.
      *----------------------------------------------------------------*
           DISPLAY CTE-PROG ' - FIM DO PROCESSAMENTO'.
       920099-FIM.
           EXIT.

       END PROGRAM BHCP0015.
