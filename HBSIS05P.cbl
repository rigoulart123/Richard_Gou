       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     HBSIS05P.
      *----------------------------------------------------------------*
      * ANALISTA.....: RICHARD GOULART                                 *
      * DATA.........: 17/02/2019                                      *
      * OBJETIVO.....: REALIZAR RELATORIO DE CLIENTES                  *
      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.
      *----------------------------------------------------------------*
      *
           SELECT ARQ-CLIENTE        ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD  KEY IS COD-CLIENTE-HBSIS02C
                       ALTERNATE RECORD KEY IS CNPJ-HBSIS02C
                       ALTERNATE RECORD KEY IS RAZAO-SOCIAL-HBSIS02C
                                  LOCK MODE IS MANUAL
                                FILE STATUS IS WS-FL-STATUS-CLI.

           SELECT ARQ-SORT           ASSIGN TO "SORTCLIENTE.TMP"
                                FILE STATUS IS WS-FL-STATUS-SORT.

           SELECT REL-CLIENTE        ASSIGN TO "RELCLIENTE.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS WS-FL-STATUS-REL.

           SELECT CSV-CLIENTE        ASSIGN TO "RELCLIENTE.CSV"
                               ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS WS-FL-STATUS-CSV.

      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-CLIENTE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'ArqCliente'.
       COPY "HBSIS02C.CPY".

       SD  ARQ-SORT.
       01  REG-SORT.
           05  SORT-COD-CLI            PIC  9(007).
           05  SORT-CNPJ-CLI           PIC  9(014).
           05  SORT-RAZ-SOC            PIC  X(040).
           05  SORT-LATITUDE           PIC S9(003)V9(008).
           05  SORT-LONGITUDE          PIC S9(003)V9(008).
           05  SORT-COD-VEND           PIC  9(003).

       FD  REL-CLIENTE.
       01  REG-REL-CLIENTE             PIC X(120).

       FD  CSV-CLIENTE.
       01  REG-CSV-CLIENTE             PIC X(105).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  WS-FL-STATUS-CLI            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-SORT           PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-REL            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-CSV            PIC  X(002)         VALUE "00".
      *
       01  WS-ARQ-CLIENTE.
           05  WS-ARQ-CODIGO-CLI       PIC  9(007)         VALUE ZEROS.
           05  WS-ARQ-CNPJ-CLI         PIC  9(014)         VALUE ZEROS.
           05  WS-RAZAO-SOCIAL         PIC  X(040)         VALUE SPACES.
           05  WS-ARQ-LATITUDE-CLI     PIC S9(003)V9(008)  VALUE ZEROS.
           05  WS-ARQ-LONGITUDE-CLI    PIC S9(003)V9(008)  VALUE ZEROS.
           05  WS-ARQ-CODIGO-VEND      PIC  9(003)         VALUE ZEROS.

      *----------------------------------------------------------------*
      * AREA DE DECLARACAO DO LAY-OUT DO RELATORIO                     *
      *----------------------------------------------------------------*
       01  CAB1.
           03 FILLER                   PIC  X(120)         VALUE
              "  RELATORIO DE CLIENTES".

       01  CAB2.
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 FILLER                   PIC  X(011)         VALUE
              "COD CLIENTE".
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 FILLER                   PIC  X(004)         VALUE
              "CNPJ".
           03 FILLER                   PIC  X(012)         VALUE SPACES.
           03 FILLER                   PIC  X(012)         VALUE
              "RAZAO SOCIAL".
           03 FILLER                   PIC  X(030)         VALUE SPACES.
           03 FILLER                   PIC  X(008)         VALUE
              "LATITUDE".
           03 FILLER                   PIC  X(007)         VALUE SPACES.
           03 FILLER                   PIC  X(009)         VALUE
              "LONGITUDE".
           03 FILLER                   PIC  X(006)         VALUE SPACES.
           03 FILLER                   PIC  X(012)         VALUE
              "COD VENDEDOR".
           03 FILLER                   PIC  X(003)         VALUE SPACES.

       01  DET.
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 DET-COD-CLI              PIC  9(007)         VALUE ZEROS.
           03 FILLER                   PIC  X(006)         VALUE SPACES.
           03 DET-CNPJ-CLI             PIC  9(014)         VALUE ZEROS.
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 DET-RAZ-SOC              PIC  X(040)         VALUE SPACES.
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 DET-LATITUDE             PIC +ZZ9,99999999   VALUE ZEROS.
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 DET-LONGITUDE            PIC +ZZ9,99999999   VALUE ZEROS.
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 DET-COD-VEND             PIC  9(003)         VALUE ZEROS.
           03 FILLER                   PIC  X(012)         VALUE SPACES.

       01  LINHA-TRACO.
           03 FILLER                   PIC  X(120)         VALUE
              ALL "-".

       01  LINHA-BRANCO.
           03 FILLER                   PIC  X(120)         VALUE
              ALL SPACES.

      *----------------------------------------------------------------*
      * AREA DE DECLARACAO DO LAY-OUT DO RELATORIO CSV                 *
      *----------------------------------------------------------------*
       01  CAB-CSV                     PIC  X(105)         VALUE
           "COD CLIENTE;CNPJ;RAZAO SOCIAL;LATITUDE;LONGITUDE;COD VEND".

       01  DET-CSV.
           03 CSV-COD-CLI              PIC  9(007)         VALUE ZEROS.
           03 FILLER                   PIC  X(001)         VALUE ";".
           03 CSV-CNPJ-CLI             PIC  9(014)         VALUE ZEROS.
           03 FILLER                   PIC  X(001)         VALUE ";".
           03 CSV-RAZ-SOC              PIC  X(040)         VALUE SPACES.
           03 FILLER                   PIC  X(001)         VALUE ";".
           03 CSV-LATITUDE             PIC +ZZ9,99999999   VALUE ZEROS.
           03 FILLER                   PIC  X(001)         VALUE ";".
           03 CSV-LONGITUDE            PIC +ZZ9,99999999   VALUE ZEROS.
           03 FILLER                   PIC  X(001)         VALUE ";".
           03 CSV-COD-VEND             PIC  9(003)         VALUE ZEROS.

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       COPY HBSIS05L.
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION USING HBSIS05L.
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.

           PERFORM 1000-INICIALIZA
           PERFORM 2000-PROCESSA
           PERFORM 3000-FINALIZA

           .
       0000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZA��O                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZA                 SECTION.

           MOVE ZEROS                  TO COD-RETORNO-HBSIS05L
           MOVE "RELATORIO GERADO COM SUCESSO"
                                       TO MSG-RETORNO-HBSIS05L

           PERFORM 1100-VALIDA-PREENCHIMENTO

           IF COD-CLI-HBSIS05L        NOT EQUAL ZEROS  AND
              RAZ-SOC-HBSIS05L        NOT EQUAL SPACES
              PERFORM 1200-BUSCAR-CLIENTE
           END-IF

           PERFORM 1300-ABRE-ARQUIVOS

           .
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE VALIDACAO DO PRRENCHIMENTO DOS CAMPOS DA TELA        *
      *----------------------------------------------------------------*
       1100-VALIDA-PREENCHIMENTO       SECTION.

           IF TIPO-ORD-HBSIS05L        NOT EQUAL "A" AND
              TIPO-ORD-HBSIS05L        NOT EQUAL "D"
              MOVE 1                   TO COD-RETORNO-HBSIS05L
              MOVE "TIPO DE ORDENACAO COM VALOR NAO PERMITIDO"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           IF TIPO-CLA-HBSIS05L        NOT EQUAL "C" AND
              TIPO-CLA-HBSIS05L        NOT EQUAL "R"
              MOVE 2                   TO COD-RETORNO-HBSIS05L
              MOVE "TIPO DE CLASSIFICACAO COM VALOR NAO PERMITIDO"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           IF COD-CLI-HBSIS05L         NOT NUMERIC
              MOVE 3                   TO COD-RETORNO-HBSIS05L
              MOVE "COD CLIENTE COM VALOR NAO PERMITIDO"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           IF COD-VEND-HBSIS05L        NOT NUMERIC
              MOVE 4                   TO COD-RETORNO-HBSIS05L
              MOVE "COD VENDEDOR COM VALOR NAO PERMITIDO"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           .
       1100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE BUSCA DE CLIENTE                                     *
      *----------------------------------------------------------------*
       1200-BUSCAR-CLIENTE             SECTION.

           PERFORM 9100-OPEN-ARQ-CLIENTE

           MOVE COD-CLI-HBSIS05L      TO COD-CLIENTE-HBSIS02C

           READ ARQ-CLIENTE     RECORD INTO WS-ARQ-CLIENTE
                                   KEY IS COD-CLIENTE-HBSIS02C

           IF WS-FL-STATUS-CLI         NOT EQUAL ZEROS
              IF RAZ-SOC-HBSIS05L     NOT EQUAL SPACES
                 MOVE RAZ-SOC-HBSIS05L
                                       TO RAZAO-SOCIAL-HBSIS02C
                 READ ARQ-CLIENTE
                                RECORD INTO WS-ARQ-CLIENTE
                                   KEY IS RAZAO-SOCIAL-HBSIS02C

                   IF WS-FL-STATUS-CLI NOT EQUAL ZEROS
                       MOVE 5          TO COD-RETORNO-HBSIS05L
                       MOVE "CLIENTE NAO ENCONTRADO"
                                       TO MSG-RETORNO-HBSIS05L
                   END-IF
               ELSE
                   MOVE 6              TO COD-RETORNO-HBSIS05L
                   MOVE "CLIENTE NAO ENCONTRADO"
                                       TO MSG-RETORNO-HBSIS05L
               END-IF
           END-IF

           PERFORM 9200-CLOSE-ARQ-CLIENTE

           .
       1200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DOS ARQUIVOS                                *
      *----------------------------------------------------------------*
       1300-ABRE-ARQUIVOS              SECTION.

           PERFORM 9100-OPEN-ARQ-CLIENTE

           OPEN OUTPUT REL-CLIENTE

           IF WS-FL-STATUS-REL         NOT EQUAL ZEROS
              MOVE 9                   TO COD-RETORNO-HBSIS05L
              MOVE "ERRO NA ABERTURA DO ARQUIVO REL-CLIENTE"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           OPEN OUTPUT CSV-CLIENTE

           IF WS-FL-STATUS-CSV         NOT EQUAL ZEROS
              MOVE 9                   TO COD-RETORNO-HBSIS05L
              MOVE "ERRO NA ABERTURA DO ARQUIVO CSV-CLIENTE"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           .
       1300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO                                        *
      *----------------------------------------------------------------*
       2000-PROCESSA                   SECTION.

           IF  TIPO-ORD-HBSIS05L       EQUAL "A"
               IF  TIPO-CLA-HBSIS05L   EQUAL "C"
                   SORT ARQ-SORT
                   ON ASCENDING    KEY SORT-COD-CLI
                   INPUT PROCEDURE  IS 2100-INPUT-SORT
                   OUTPUT PROCEDURE IS 2200-OUTPUT-SORT
               ELSE
                   SORT ARQ-SORT
                   ON ASCENDING    KEY SORT-RAZ-SOC
                   INPUT PROCEDURE  IS 2100-INPUT-SORT
                   OUTPUT PROCEDURE IS 2200-OUTPUT-SORT
               END-IF
           ELSE
               IF  TIPO-CLA-HBSIS05L   EQUAL "C"
                   SORT ARQ-SORT
                   ON DESCENDING   KEY SORT-COD-CLI
                   INPUT PROCEDURE  IS 2100-INPUT-SORT
                   OUTPUT PROCEDURE IS 2200-OUTPUT-SORT
               ELSE
                   SORT ARQ-SORT
                   ON DESCENDING   KEY SORT-RAZ-SOC
                   INPUT PROCEDURE  IS 2100-INPUT-SORT
                   OUTPUT PROCEDURE IS 2200-OUTPUT-SORT
               END-IF
           END-IF

           .
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INPUT DO SORT                                        *
      *----------------------------------------------------------------*
       2100-INPUT-SORT                 SECTION.

           READ ARQ-CLIENTE NEXT

           IF  WS-FL-STATUS-CLI        NOT EQUAL ZEROS
           AND WS-FL-STATUS-CLI        NOT EQUAL "10"
              MOVE 10                  TO COD-RETORNO-HBSIS05L
              MOVE "ERRO NA LEITURA DO ARQUIVO ARQ-CLIENTE"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           PERFORM UNTIL WS-FL-STATUS-CLI
                                       NOT EQUAL "00"
                   PERFORM 2110-ALIMENTA-SORT
           END-PERFORM

           PERFORM 9200-CLOSE-ARQ-CLIENTE

           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE ALIMENTACAO DO SORT                                  *
      *----------------------------------------------------------------*
       2110-ALIMENTA-SORT              SECTION.

           IF COD-CLI-HBSIS05L         EQUAL ZEROS
              IF RAZ-SOC-HBSIS05L      EQUAL SPACES
                 RELEASE REG-SORT      FROM ARQ-HBSIS02C
              ELSE
                 IF RAZAO-SOCIAL-HBSIS02C
                                       EQUAL RAZ-SOC-HBSIS05L
                    RELEASE REG-SORT   FROM ARQ-HBSIS02C
                 END-IF
              END-IF
           ELSE
              IF COD-CLIENTE-HBSIS02C EQUAL COD-CLI-HBSIS05L
                 RELEASE REG-SORT      FROM ARQ-HBSIS02C
              END-IF
           END-IF

           READ ARQ-CLIENTE NEXT

           IF  WS-FL-STATUS-CLI        NOT EQUAL ZEROS
           AND WS-FL-STATUS-CLI        NOT EQUAL "10"
              MOVE 11                  TO COD-RETORNO-HBSIS05L
              MOVE "ERRO NA LEITURA DO ARQUIVO ARQ-CLIENTE"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2110-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA OUTPUT DO SORT                                          *
      *----------------------------------------------------------------*
       2200-OUTPUT-SORT                SECTION.

           RETURN ARQ-SORT AT END

           WRITE REG-REL-CLIENTE       FROM LINHA-TRACO
           WRITE REG-REL-CLIENTE       FROM CAB1
           WRITE REG-REL-CLIENTE       FROM LINHA-BRANCO
           WRITE REG-REL-CLIENTE       FROM CAB2
           WRITE REG-REL-CLIENTE       FROM LINHA-TRACO

           WRITE REG-CSV-CLIENTE       FROM CAB-CSV

           PERFORM UNTIL WS-FL-STATUS-SORT
                                       NOT EQUAL "00"
               PERFORM 2210-GERA-RELATORIO
           END-PERFORM

           CLOSE REL-CLIENTE

           IF WS-FL-STATUS-REL         NOT EQUAL ZEROS
              MOVE 14                  TO COD-RETORNO-HBSIS05L
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO DE RELATORIO"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           CLOSE CSV-CLIENTE

           IF WS-FL-STATUS-CSV         NOT EQUAL ZEROS
              MOVE 14                  TO COD-RETORNO-HBSIS05L
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO DE RELATORIO CSV"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE GERACAO DO RELATORIO DE CLIENTES                     *
      *----------------------------------------------------------------*
       2210-GERA-RELATORIO             SECTION.

           MOVE SORT-COD-CLI           TO DET-COD-CLI
                                          CSV-COD-CLI
           MOVE SORT-CNPJ-CLI          TO DET-CNPJ-CLI
                                          CSV-CNPJ-CLI
           MOVE SORT-RAZ-SOC           TO DET-RAZ-SOC
                                          CSV-RAZ-SOC
           MOVE SORT-LATITUDE          TO DET-LATITUDE
                                          CSV-LATITUDE
           MOVE SORT-LONGITUDE         TO DET-LONGITUDE
                                          CSV-LONGITUDE
           MOVE SORT-COD-VEND          TO DET-COD-VEND
                                          CSV-COD-VEND

           WRITE REG-REL-CLIENTE       FROM DET

           IF WS-FL-STATUS-REL         NOT EQUAL ZEROS
              MOVE 13                  TO COD-RETORNO-HBSIS05L
              MOVE "ERRO AO GRAVAR DETALHE NO REL DE CLIENTES"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           WRITE REG-CSV-CLIENTE       FROM DET-CSV

           IF WS-FL-STATUS-CSV         NOT EQUAL ZEROS
              MOVE 13                  TO COD-RETORNO-HBSIS05L
              MOVE "ERRO AO GRAVAR DETALHE NO REL DE CLIENTES CSV"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2210-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZA��O                                          *
      *----------------------------------------------------------------*
       3000-FINALIZA                   SECTION.

           GOBACK

           .
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DO ARQUIVO DE CLIENTES                      *
      *----------------------------------------------------------------*
       9100-OPEN-ARQ-CLIENTE           SECTION.

           OPEN INPUT ARQ-CLIENTE

           IF WS-FL-STATUS-CLI         NOT EQUAL ZEROS
              MOVE 9                   TO COD-RETORNO-HBSIS05L
              MOVE "ERRO NA ABERTURA DO ARQUIVO CLIENTE"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           .
       9100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE FECHAMENTO DO ARQUIVO DE CLIENTES                    *
      *----------------------------------------------------------------*
       9200-CLOSE-ARQ-CLIENTE          SECTION.

           CLOSE ARQ-CLIENTE

           IF WS-FL-STATUS-CLI         NOT EQUAL ZEROS
              MOVE 9                   TO COD-RETORNO-HBSIS05L
              MOVE "ERRO NA ABERTURA DO ARQUIVO ARQ-CLIENTE"
                                       TO MSG-RETORNO-HBSIS05L
              PERFORM 3000-FINALIZA
           END-IF

           .
       9200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FIM DO PROGRAMA HBSIS05P                                      *
      *----------------------------------------------------------------*
       END PROGRAM                     HBSIS05P.
      *----------------------------------------------------------------*
