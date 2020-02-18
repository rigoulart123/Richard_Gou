       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     HBSIS03P.
      *----------------------------------------------------------------*
      * ANALISTA.....: RICHARD GOULART                                 *
      * DATA.........: 17/02/2019                                      *
      * OBJETIVO.....: GERA RELATORIO DE CLIENTES X VENDEDORES         *
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
           SELECT ARQ-DISTRIBUICAO   ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS WS-FL-STATUS-DIS.

           SELECT REL-DISTRIBUICAO   ASSIGN TO "RELDISTRIBUICAO.CSV"
                               ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS WS-FL-STATUS-REL.
      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-DISTRIBUICAO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'ArqDistribuicao'.
       COPY "HBSIS07C.CPY".

       FD  REL-DISTRIBUICAO.
       01  REG-REL-DISTRIBUICAO        PIC X(110).
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  WS-FL-STATUS-CLI            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-VEN            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-DIS            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-REL            PIC  X(002)         VALUE "00".

      *----------------------------------------------------------------*
      * AREA DE DECLARACAO DO LAY-OUT DO RELATORIO CSV                 *
      *----------------------------------------------------------------*
       01  CAB-CSV                     PIC  X(105)         VALUE
           "COD CLIENTE;RAZAO SOCIAL;COD VEND;NOME VEND; DISTANCIA".

       01  DET-CSV.
           05  CSV-CODIGO-CLIENTE      PIC 9(007) VALUE ZEROS.
           05  FILLER                  PIC X(001) VALUE ";".
           05  CSV-RAZAO-SOCIAL        PIC X(040) VALUE SPACES.
           05  FILLER                  PIC X(001) VALUE ";".
           05  CSV-CODIGO-VENDEDOR     PIC 9(003) VALUE ZEROS.
           05  FILLER                  PIC X(001) VALUE ";".
           05  CSV-NOME-VENDEDOR       PIC X(040) VALUE SPACES.
           05  FILLER                  PIC X(001) VALUE ";".
           05  CSV-DISTANCIA           PIC ZZZZZZZZ9,99 VALUE ZEROS.
           05  FILLER                  PIC X(001) VALUE ";".

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       COPY HBSIS03L.
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION USING HBSIS03L.
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

           MOVE ZEROS                  TO COD-RETORNO-HBSIS03L
           PERFORM 1100-GRAVA-CAB-REL

           .
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * GRAVA CABECALHO RELATORIO                                      *
      *----------------------------------------------------------------*
       1100-GRAVA-CAB-REL              SECTION.

           OPEN OUTPUT REL-DISTRIBUICAO

           WRITE REG-REL-DISTRIBUICAO  FROM CAB-CSV AFTER 1 LINE

           .
       1100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO                                        *
      *----------------------------------------------------------------*
       2000-PROCESSA                   SECTION.

           PERFORM 2100-OPEN-ARQ-DISTRIBUICAO

           PERFORM 2200-LER-ARQ-DISTRIBUICAO

           PERFORM 2300-TRATA-ARQUIVO UNTIL
                   WS-FL-STATUS-DIS   NOT EQUAL "00"

           PERFORM 2400-CLOSE-ARQ-DISTRIBUICAO

           .
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ABERTURA DE ARQUIVO DISTRIBUICAO                               *
      *----------------------------------------------------------------*
       2100-OPEN-ARQ-DISTRIBUICAO      SECTION.

           OPEN INPUT ARQ-DISTRIBUICAO

           IF WS-FL-STATUS-DIS         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS03L
              MOVE "ERRO NA ABERTURA DO ARQUIVO DE DISTRIBUICAO"
                                       TO MSG-RETORNO-HBSIS03L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * LEITURA DE ARQUIVO DISTRIBUICAO                                *
      *----------------------------------------------------------------*
       2200-LER-ARQ-DISTRIBUICAO       SECTION.

           READ ARQ-DISTRIBUICAO

           IF WS-FL-STATUS-DIS         EQUAL ZEROS OR '10'
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS03L
              MOVE "ERRO NA LEITURA DO ARQUIVO DO ARQ-DISTRIBUICAO"
                                       TO MSG-RETORNO-HBSIS03L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * GERA RELATORIO                                                 *
      *----------------------------------------------------------------*
       2300-TRATA-ARQUIVO              SECTION.

           PERFORM 2310-GRAVA-DETALHE
           PERFORM 2200-LER-ARQ-DISTRIBUICAO

           .
       2300-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * GERA��O DE RELATORIO                                           *
      *----------------------------------------------------------------*
       2310-GRAVA-DETALHE              SECTION.

           MOVE COD-CLIENTE-HBSIS07C   TO CSV-CODIGO-CLIENTE
           MOVE RAZAO-SOCIAL-HBSIS07C  TO CSV-RAZAO-SOCIAL
           MOVE COD-VENDEDOR-HBSIS07C  TO CSV-CODIGO-VENDEDOR
           MOVE NOME-VEND-HBSIS07C     TO CSV-NOME-VENDEDOR
           MOVE DISTANCIA-HBSIS07C     TO CSV-DISTANCIA

           WRITE REG-REL-DISTRIBUICAO  FROM DET-CSV AFTER 1 LINE

           .
       2310-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FECHAMENTO DE ARQUIVO                                          *
      *----------------------------------------------------------------*
       2400-CLOSE-ARQ-DISTRIBUICAO     SECTION.

           CLOSE ARQ-DISTRIBUICAO

           IF WS-FL-STATUS-DIS         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS03L
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO DISTRIBUICAO"
                                       TO MSG-RETORNO-HBSIS03L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZA��O                                          *
      *----------------------------------------------------------------*
       3000-FINALIZA                   SECTION.

           CLOSE REL-DISTRIBUICAO

           GOBACK

           .
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FIM DO PROGRAMA HBSIS03P                                       *
      *----------------------------------------------------------------*
       END PROGRAM                     HBSIS03P.
      *----------------------------------------------------------------*
