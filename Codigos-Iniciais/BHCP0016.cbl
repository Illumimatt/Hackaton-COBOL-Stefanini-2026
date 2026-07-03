      ******************************************************************
      * SIGLA.....: BHC – BOOTCAMP HACKATHON COBOL
      * PROGRAMA..: BHCP0016
      * ANALISTA..: JOSE HILARIO
      * AUTOR.....: MATHEUS KOLLMANN DETERS
      * DATA......: 03/07/2026
      * OBJETIVO..: GERACAO DE ARQUIVO JSON A PARTIR DE SEQUENCIAL
      * EXECUCAO..: COBOL - BATCH
      * ----------------------------------------------------------------
      * VRS DATA     RESPONSAVEL       DESCRICAO DA VERSAO
      * --- -------- ----------------- ---------------------------------
      * 001 03.07.26 MATHEUS KOLLMANN  IMPLANTACAO JSON MANUAL
      * ----------------------------------------------------------------
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. BHCP0016.
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
           SELECT BHCF012S ASSIGN TO "BHCF012S.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS WS-FS-ENTRADA.

           SELECT BHCF016J ASSIGN TO "BHCF016J.json"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS WS-FS-SAIDA-JSON.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
      *----------------------------------------
       FILE                            SECTION.
      *----------------------------------------
       FD  BHCF012S.
       01  REG-BHCF012S                PIC X(065).

       FD  BHCF016J.
       01  REG-BHCF016J                PIC X(200).

      *----------------------------------------
       WORKING-STORAGE                 SECTION.
      *----------------------------------------
       77  CTE-PROG                    PIC X(008) VALUE 'BHCP0016'.
       77  CTE-VERS                    PIC X(006) VALUE 'VRS001'.

       01  WS-FILE-STATUS.
           05  WS-FS-ENTRADA           PIC X(002) VALUE SPACES.
           05  WS-FS-SAIDA-JSON        PIC X(002) VALUE SPACES.

       01  WS-CONSTANTES.
           05  WS-SUCESSO              PIC X(002) VALUE '00'.
           05  WS-FIM-ARQ              PIC X(002) VALUE '10'.

       01  WS-AREA-LEITURA.
           05  IN-CODIGO               PIC X(005).
           05  IN-NOME                 PIC X(030).
           05  IN-UF                   PIC X(002).
           05  IN-TRILHA               PIC X(010).
           05  IN-SITUACAO             PIC X(010).
           05  IN-DATA                 PIC X(008).

       01 WS-INDENTACOES.
           05 WS-IND1      PIC X(4) VALUE '    '.
           05 WS-IND2      PIC X(8) VALUE '        '.
           05 WS-IND3      PIC X(12) VALUE '            '.

      *----------------------------------------
       LOCAL-STORAGE                   SECTION.
      *----------------------------------------
       01  GDA-CONTROLES-SISTE.
           05  GDA-IN-FIM-ARQ          PIC X(001) VALUE 'N'.
               88  GDA-FIM-SIM                    VALUE 'S'.
               88  GDA-FIM-NAO                    VALUE 'N'.

       01  GDA-TOTALIZADORES.
           05  GDA-TOT-LIDOS           PIC 9(005) VALUE ZEROS.
           05  GDA-TOT-JSON            PIC 9(005) VALUE ZEROS.
           05  GDA-TOT-ERROS-ARQ       PIC 9(005) VALUE ZEROS.

       01  GDA-DATA-HORA.
           05  GDA-DT-CURRENT-DATE     PIC X(021) VALUE SPACES.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
           PERFORM 100000-INICIALIZAR
           PERFORM 200000-ABRIR-ARQUIVOS
           PERFORM 300000-GRAVAR-CABECALHO-JSON
           PERFORM 500000-PROCESSAR-ARQUIVO
           PERFORM 800000-GRAVAR-RODAPE-JSON
           PERFORM 900000-FECHAR-ARQUIVOS
           PERFORM 910000-EXIBIR-TOTALIZADORES
           PERFORM 999999-FINALIZAR

           GOBACK.

      *----------------------------------------------------------------*
       100000-INICIALIZAR.
      *----------------------------------------------------------------*
           SET GDA-FIM-NAO             TO TRUE
           MOVE FUNCTION CURRENT-DATE  TO GDA-DT-CURRENT-DATE
           DISPLAY CTE-PROG' - INICIO DO PROCESSAMENTO'
           DISPLAY 'DATA/HORA: ' GDA-DT-CURRENT-DATE.

      *----------------------------------------------------------------*
       200000-ABRIR-ARQUIVOS.
      *----------------------------------------------------------------*
           OPEN INPUT  BHCF012S
           IF WS-FS-ENTRADA = WS-SUCESSO
              DISPLAY 'ARQUIVO BHCF012S ABERTO COM SUCESSO'
           ELSE
              DISPLAY 'ERRO OPEN INPUT BHCF012S FS=' WS-FS-ENTRADA
              ADD 1                    TO GDA-TOT-ERROS-ARQ
              SET GDA-FIM-SIM          TO TRUE
           END-IF

           OPEN OUTPUT BHCF016J
           IF WS-FS-SAIDA-JSON = WS-SUCESSO
              DISPLAY 'ARQUIVO BHCF016J ABERTO COM SUCESSO'
           ELSE
              DISPLAY 'ERRO OPEN OUTPUT BHCF016J FS=' WS-FS-SAIDA-JSON
              ADD 1                    TO GDA-TOT-ERROS-ARQ
              SET GDA-FIM-SIM          TO TRUE
           END-IF.

      *----------------------------------------------------------------*
       300000-GRAVAR-CABECALHO-JSON.
      *----------------------------------------------------------------*
           IF GDA-FIM-NAO
              MOVE '{' TO REG-BHCF016J
              WRITE REG-BHCF016J

              MOVE SPACES TO REG-BHCF016J
              STRING WS-IND1
                     '"participantes": ['
                DELIMITED BY SIZE
                INTO REG-BHCF016J
              END-STRING
              WRITE REG-BHCF016J

              IF WS-FS-SAIDA-JSON NOT = WS-SUCESSO
                 DISPLAY 'ERRO WRITE CABECALHO FS='
                         WS-FS-SAIDA-JSON
                 ADD 1 TO GDA-TOT-ERROS-ARQ
              END-IF
           END-IF.

      *----------------------------------------------------------------*
       400000-LER-ENTRADA.
      *----------------------------------------------------------------*
           READ BHCF012S INTO WS-AREA-LEITURA
                AT END
                   SET GDA-FIM-SIM     TO TRUE
                NOT AT END
                   ADD 1               TO GDA-TOT-LIDOS
           END-READ

           IF WS-FS-ENTRADA NOT = WS-SUCESSO
           AND WS-FS-ENTRADA NOT = WS-FIM-ARQ
              DISPLAY 'ERRO READ BHCF012S FS=' WS-FS-ENTRADA
              ADD 1                    TO GDA-TOT-ERROS-ARQ
              SET GDA-FIM-SIM          TO TRUE
           END-IF.

      *----------------------------------------------------------------*
       500000-PROCESSAR-ARQUIVO.
      *----------------------------------------------------------------*
           IF GDA-FIM-NAO
              PERFORM 400000-LER-ENTRADA

              PERFORM UNTIL GDA-FIM-SIM
                  PERFORM 510000-GRAVAR-OBJETO-JSON
                  PERFORM 400000-LER-ENTRADA
              END-PERFORM
           END-IF.

      *----------------------------------------------------------------*
       510000-GRAVAR-OBJETO-JSON.
      *----------------------------------------------------------------*
           IF GDA-TOT-LIDOS > 1
              MOVE SPACES TO REG-BHCF016J
              STRING WS-IND2
                     '},'
                DELIMITED BY SIZE
                INTO REG-BHCF016J
              END-STRING
              WRITE REG-BHCF016J
           END-IF

           MOVE SPACES TO REG-BHCF016J
           STRING WS-IND2
                  '{'
             DELIMITED BY SIZE
             INTO REG-BHCF016J
           END-STRING
           WRITE REG-BHCF016J

           MOVE SPACES TO REG-BHCF016J
           STRING WS-IND3
                  '"codigo": "'
                  IN-CODIGO
                  '",'
             DELIMITED BY SIZE
             INTO REG-BHCF016J
           END-STRING
           WRITE REG-BHCF016J

           MOVE SPACES TO REG-BHCF016J
           STRING WS-IND3
                  '"nome": "'
                  FUNCTION TRIM(IN-NOME)
                  '",'
             DELIMITED BY SIZE
             INTO REG-BHCF016J
           END-STRING
           WRITE REG-BHCF016J

           MOVE SPACES TO REG-BHCF016J
           STRING WS-IND3
                  '"uf": "'
                  FUNCTION TRIM(IN-UF)
                  '",'
             DELIMITED BY SIZE
             INTO REG-BHCF016J
           END-STRING
           WRITE REG-BHCF016J

           MOVE SPACES TO REG-BHCF016J
           STRING WS-IND3
                  '"trilha": "'
                  FUNCTION TRIM(IN-TRILHA)
                  '",'
             DELIMITED BY SIZE
             INTO REG-BHCF016J
           END-STRING
           WRITE REG-BHCF016J

           MOVE SPACES TO REG-BHCF016J
           STRING WS-IND3
                  '"situacao": "'
                  FUNCTION TRIM(IN-SITUACAO)
                  '",'
             DELIMITED BY SIZE
             INTO REG-BHCF016J
           END-STRING
           WRITE REG-BHCF016J

           MOVE SPACES TO REG-BHCF016J
           STRING WS-IND3
                  '"data": "'
                  IN-DATA
                  '"'
             DELIMITED BY SIZE
             INTO REG-BHCF016J
           END-STRING
           WRITE REG-BHCF016J

           ADD 1 TO GDA-TOT-JSON.
      *----------------------------------------------------------------*
       800000-GRAVAR-RODAPE-JSON.
      *----------------------------------------------------------------*
           MOVE SPACES TO REG-BHCF016J
           STRING WS-IND2
                  '}'
             DELIMITED BY SIZE
             INTO REG-BHCF016J
           END-STRING
           WRITE REG-BHCF016J

           MOVE SPACES TO REG-BHCF016J
           STRING WS-IND1
                  ']'
             DELIMITED BY SIZE
             INTO REG-BHCF016J
           END-STRING
           WRITE REG-BHCF016J

           MOVE '}' TO REG-BHCF016J
           WRITE REG-BHCF016J.
      *----------------------------------------------------------------*
       900000-FECHAR-ARQUIVOS.
      *----------------------------------------------------------------*
           CLOSE BHCF012S
                 BHCF016J.

      *----------------------------------------------------------------*
       910000-EXIBIR-TOTALIZADORES.
      *----------------------------------------------------------------*
           DISPLAY 'TOTAL DE REGISTROS LIDOS......: '
                   GDA-TOT-LIDOS
           DISPLAY 'TOTAL DE OBJETOS JSON GERADOS.: '
                   GDA-TOT-JSON
           DISPLAY 'TOTAL DE ERROS DE ARQUIVO.....: '
                   GDA-TOT-ERROS-ARQ.

      *----------------------------------------------------------------*
       999999-FINALIZAR.
      *----------------------------------------------------------------*
           MOVE FUNCTION CURRENT-DATE  TO GDA-DT-CURRENT-DATE
           DISPLAY CTE-PROG' - FIM DO PROCESSAMENTO'
           DISPLAY 'DATA/HORA: ' GDA-DT-CURRENT-DATE
           EXIT.
