       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     HBSIS04P.
      *----------------------------------------------------------------*
      * ANALISTA.....: RICHARD GOULART                                 *
      * DATA.........: 17/02/2019                                      *
      * OBJETIVO.....: REALIZAR ACESSOS AO ARQUIVO DE VEND AMBEV       *
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
           SELECT ARQ-VENDEDOR        ASSIGN TO DISK
                                ORGANIZATION IS INDEXED
                                 ACCESS MODE IS DYNAMIC
                                 RECORD  KEY IS COD-VENDEDOR-HBSIS04C
                        ALTERNATE RECORD KEY IS CPF-HBSIS04C
                        ALTERNATE RECORD KEY IS NOME-VEND-HBSIS04C
                                   LOCK MODE IS MANUAL
                                 FILE STATUS IS WS-FL-STATUS-VEN.

           SELECT IMP-VENDEDOR        ASSIGN TO W-LABEL-IMP
                               ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FL-STATUS-IMP.
      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'ArqVendedor'.
       COPY "HBSIS04C.CPY".

       FD  IMP-VENDEDOR
           LABEL RECORD IS STANDARD.
       01  IMP-VENDEDOR-REG.
           03 IMP-COD-VENDEDOR         PIC  9(003).
           03 IMP-CPF                  PIC  9(011).
           03 IMP-MOME                 PIC  X(040).
           03 IMP-LATITUDE             PIC S9(003)V9(008).
           03 IMP-LONGITUDE            PIC S9(003)V9(008).
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  WS-FL-STATUS-VEN            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-IMP            PIC  X(002)         VALUE "00".

       01  WS-OPCAO                    PIC  9(001)         VALUE ZEROS.
       01  WS-CAD-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-REL-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-EXE-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-CLI-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-CONFIRMA                 PIC  X(001)         VALUE ZEROS.
       01  WS-LINHA-BRANCO             PIC  X(078)         VALUE SPACES.
       01  WS-LABEL-IMP                PIC  X(020)         VALUE SPACES.
      *
       01  WS-CODIGO-VEND              PIC  9(003)         VALUE ZEROS.
       01  WS-CPF-VEND                 PIC  9(011)         VALUE ZEROS.
       01  WS-NOME-VEND                PIC  X(040)         VALUE SPACES.
       01  WS-LATITUDE-VEND            PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-LONGITUDE-VEND           PIC S9(003)V9(008)  VALUE ZEROS.
      *
       01  WS-ARQ-VENDEDOR.
           05  WS-ARQ-CODIGO-VEN       PIC  9(003)         VALUE ZEROS.
           05  WS-ARQ-CPF-VEN          PIC  9(011)         VALUE ZEROS.
           05  WS-ARQ-NOME-VEN         PIC  X(040)         VALUE SPACES.
           05  WS-ARQ-LATITUDE-VEN     PIC S9(003)V9(008)  VALUE ZEROS.
           05  WS-ARQ-LONGITUDE-VEN    PIC S9(003)V9(008)  VALUE ZEROS.
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       COPY HBSIS04L.
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION USING HBSIS04L.
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.

           PERFORM 1000-INICIALIZA
           PERFORM 2000-PROCESSA
           PERFORM 3000-FINALIZA

           .
       0000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZAÇÃO                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZA                 SECTION.

           .
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO                                        *
      *----------------------------------------------------------------*
       2000-PROCESSA                   SECTION.

           EVALUATE COD-FUNCAO-HBSIS04
               WHEN 0
                    PERFORM 2100-BUSCAR-VENDEDOR
               WHEN 1
                    PERFORM 2200-INCLUIR-VENDEDOR
               WHEN 2
                    PERFORM 2300-ALTERAR-VENDEDOR
               WHEN 3
                    PERFORM 2400-EXCLUIR-VENDEDOR
               WHEN OTHER
                    MOVE 9             TO COD-RETORNO-HBSIS04
                    MOVE 'CODIGO DA FUNCAO INVALIDA'
                                       TO MSG-RETORNO-HBSIS04
                    PERFORM 3000-FINALIZA
           END-EVALUATE

           .
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE BUSCA DE VENDEDOR                                    *
      *----------------------------------------------------------------*
       2100-BUSCAR-VENDEDOR            SECTION.

           PERFORM 2110-OPEN-ARQ-VENDEDOR

           MOVE COD-VENDEDOR-HBSIS04   TO COD-VENDEDOR-HBSIS04C

           READ ARQ-VENDEDOR  RECORD INTO WS-ARQ-VENDEDOR
                              KEY IS      COD-VENDEDOR-HBSIS04C

           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              MOVE WS-ARQ-CODIGO-VEN   TO COD-VENDEDOR-HBSIS04
              MOVE WS-ARQ-CPF-VEN      TO CPF-HBSIS04
              MOVE WS-ARQ-NOME-VEN     TO NOME-VENDEDOR-HBSIS04
              MOVE WS-ARQ-LATITUDE-VEN TO LATITUDE-VEN-HBSIS04
              MOVE WS-ARQ-LATITUDE-VEN TO LATITUDE-VEN-HBSIS04
              MOVE ZEROS               TO COD-RETORNO-HBSIS04
           ELSE
              MOVE CPF-HBSIS04         TO CPF-HBSIS04C

              READ ARQ-VENDEDOR RECORD INTO WS-ARQ-VENDEDOR
                                KEY IS      CPF-HBSIS04C

              IF WS-FL-STATUS-VEN      EQUAL ZEROS
                 MOVE WS-ARQ-CODIGO-VEN
                                       TO COD-VENDEDOR-HBSIS04
                 MOVE WS-ARQ-CPF-VEN   TO CPF-HBSIS04
                 MOVE WS-ARQ-NOME-VEN  TO NOME-VENDEDOR-HBSIS04
                 MOVE WS-ARQ-LATITUDE-VEN
                                       TO LATITUDE-VEN-HBSIS04
                 MOVE WS-ARQ-LONGITUDE-VEN
                                       TO LONGITUDE-VEN-HBSIS04
                 MOVE ZEROS            TO COD-RETORNO-HBSIS04
              ELSE
                 MOVE 1                TO COD-RETORNO-HBSIS04
                 MOVE "VENDEDOR NAO ENCONTRADO"
                                       TO MSG-RETORNO-HBSIS04
              END-IF
           END-IF

           PERFORM 2120-CLOSE-ARQ-VENDEDOR

           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DE ARQUIVO                                  *
      *----------------------------------------------------------------*
       2110-OPEN-ARQ-VENDEDOR           SECTION.

           OPEN I-O ARQ-VENDEDOR

           IF WS-FL-STATUS-VEN         EQUAL ZEROS OR '05'
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS04
              MOVE "ERRO NA ABERTURA DO ARQUIVO"
                                       TO MSG-RETORNO-HBSIS04
              PERFORM 2120-CLOSE-ARQ-VENDEDOR
              PERFORM 3000-FINALIZA
           END-IF

           .
       2110-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FECHAMENTO DE ARQUIVO                                          *
      *----------------------------------------------------------------*
       2120-CLOSE-ARQ-VENDEDOR         SECTION.

           CLOSE ARQ-VENDEDOR

           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS04
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO"
                                       TO MSG-RETORNO-HBSIS04
              PERFORM 2120-CLOSE-ARQ-VENDEDOR
              PERFORM 3000-FINALIZA
           END-IF

           .
       2120-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INCLUSÃO DE VENDEDOR                                 *
      *----------------------------------------------------------------*
       2200-INCLUIR-VENDEDOR           SECTION.

           PERFORM 2110-OPEN-ARQ-VENDEDOR

           MOVE COD-VENDEDOR-HBSIS04   TO COD-VENDEDOR-HBSIS04C

           READ ARQ-VENDEDOR  RECORD INTO WS-ARQ-VENDEDOR
                              KEY IS      COD-VENDEDOR-HBSIS04C

           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              MOVE 1                   TO COD-RETORNO-HBSIS04
              MOVE "VENDEDOR JA CADASTRADO"
                                       TO MSG-RETORNO-HBSIS04
           ELSE
              MOVE CPF-HBSIS04        TO CPF-HBSIS04C

              READ ARQ-VENDEDOR RECORD INTO WS-ARQ-VENDEDOR
                                KEY IS      CPF-HBSIS04C

              IF WS-FL-STATUS-VEN      EQUAL ZEROS
                 MOVE 1                TO COD-RETORNO-HBSIS04
                 MOVE "VENDEDOR JA CADASTRADO"
                                       TO MSG-RETORNO-HBSIS04
              ELSE
                 MOVE COD-VENDEDOR-HBSIS04
                                       TO WS-ARQ-CODIGO-VEN
                 MOVE CPF-HBSIS04      TO WS-ARQ-CPF-VEN
                 MOVE NOME-VENDEDOR-HBSIS04
                                       TO WS-ARQ-NOME-VEN
                 MOVE LATITUDE-VEN-HBSIS04
                                       TO WS-ARQ-LATITUDE-VEN
                 MOVE LONGITUDE-VEN-HBSIS04
                                       TO WS-ARQ-LONGITUDE-VEN
                 PERFORM 2220-GRAVA-ARQ-VENDEDOR
                 MOVE ZEROS            TO COD-RETORNO-HBSIS04
                 MOVE "VENDEDOR CADASTRADO COM SUCESSO"
                                       TO MSG-RETORNO-HBSIS04
              END-IF
           END-IF

           PERFORM 2120-CLOSE-ARQ-VENDEDOR

           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE GRAVAÇÃO DO ARQUIVO DE VENDEDOR                      *
      *----------------------------------------------------------------*
       2220-GRAVA-ARQ-VENDEDOR         SECTION.

           MOVE WS-ARQ-VENDEDOR        TO ARQ-HBSIS04C

           WRITE ARQ-HBSIS04C

           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS04
              MOVE "ERRO AO GRAVAR O VENDEDOR"
                                       TO MSG-RETORNO-HBSIS04
              PERFORM 2120-CLOSE-ARQ-VENDEDOR
              PERFORM 3000-FINALIZA
           END-IF

           .
       2220-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE ALTERACÃO DE VENDEDOR                                *
      *----------------------------------------------------------------*
       2300-ALTERAR-VENDEDOR            SECTION.

           PERFORM 2110-OPEN-ARQ-VENDEDOR

           MOVE COD-VENDEDOR-HBSIS04    TO COD-VENDEDOR-HBSIS04C

           READ ARQ-VENDEDOR   RECORD INTO WS-ARQ-VENDEDOR
                              KEY IS      COD-VENDEDOR-HBSIS04C

           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              MOVE NOME-VENDEDOR-HBSIS04
                                       TO WS-ARQ-NOME-VEN
              MOVE LATITUDE-VEN-HBSIS04
                                       TO WS-ARQ-LATITUDE-VEN
              MOVE LONGITUDE-VEN-HBSIS04
                                       TO WS-ARQ-LONGITUDE-VEN
              PERFORM 2310-ALTERAR-ARQ-VEND
              MOVE 0                   TO COD-RETORNO-HBSIS04
              MOVE "VENDEDOR ALTERADO COM SUCESSO"
                                       TO MSG-RETORNO-HBSIS04
           ELSE
              MOVE CPF-HBSIS04         TO CPF-HBSIS04C

              READ ARQ-VENDEDOR RECORD INTO WS-ARQ-VENDEDOR
                                KEY IS      CPF-HBSIS04C

              IF WS-FL-STATUS-VEN      EQUAL ZEROS
                 MOVE NOME-VENDEDOR-HBSIS04
                                       TO WS-ARQ-NOME-VEN
                 MOVE LATITUDE-VEN-HBSIS04
                                       TO WS-ARQ-LATITUDE-VEN
                 MOVE LONGITUDE-VEN-HBSIS04
                                       TO WS-ARQ-LONGITUDE-VEN
                 PERFORM 2310-ALTERAR-ARQ-VEND
                 MOVE 0                TO COD-RETORNO-HBSIS04
                 MOVE "VENDEDOR ALTERADO COM SUCESSO"
                                       TO MSG-RETORNO-HBSIS04
              ELSE
                 MOVE 1                TO COD-RETORNO-HBSIS04
                 MOVE "ERRO AO ALTERAR VENDEDOR"
                                       TO MSG-RETORNO-HBSIS04
              END-IF
           END-IF

           PERFORM 2120-CLOSE-ARQ-VENDEDOR

           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE REGRAVAÇÃO DO ARQUIVO DE VENDEDOR                    *
      *----------------------------------------------------------------*
       2310-ALTERAR-ARQ-VEND           SECTION.

           REWRITE ARQ-HBSIS04C

           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS04
              MOVE "ERRO AO ALTERAR VENDEDOR"
                                       TO MSG-RETORNO-HBSIS04
              PERFORM 2120-CLOSE-ARQ-VENDEDOR
              PERFORM 3000-FINALIZA
           END-IF

           .
       2310-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE EXCLUSÃO DE VENDEDOR                                 *
      *----------------------------------------------------------------*
       2400-EXCLUIR-VENDEDOR            SECTION.

           PERFORM 2110-OPEN-ARQ-VENDEDOR

           MOVE COD-VENDEDOR-HBSIS04  TO COD-VENDEDOR-HBSIS04C

           READ ARQ-VENDEDOR  RECORD INTO WS-ARQ-VENDEDOR
                              KEY IS      COD-VENDEDOR-HBSIS04C

           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              PERFORM 2410-EXCLUIR-ARQ-VEND
              MOVE "VENDEDOR EXCLUIDO COM SUCESSO"
                                       TO MSG-RETORNO-HBSIS04
           ELSE
              MOVE CPF-HBSIS04        TO CPF-HBSIS04C

              READ ARQ-VENDEDOR RECORD INTO WS-ARQ-VENDEDOR
                                KEY IS      CPF-HBSIS04C

              IF WS-FL-STATUS-VEN      EQUAL ZEROS
                 PERFORM 2410-EXCLUIR-ARQ-VEND
                 MOVE 0                TO COD-RETORNO-HBSIS04
                 MOVE "VENDEDOR EXCLUIDO COM SUCESSO"
                                       TO MSG-RETORNO-HBSIS04
              ELSE
                 MOVE 1                TO COD-RETORNO-HBSIS04
                 MOVE "ERRO AO EXCLUIR VENDEDOR"
                                       TO MSG-RETORNO-HBSIS04
              END-IF
           END-IF

           PERFORM 2120-CLOSE-ARQ-VENDEDOR

           .
       2400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE EXCLUSÃO DO ARQUIVO DE VENDEDOR                     *
      *----------------------------------------------------------------*
       2410-EXCLUIR-ARQ-VEND           SECTION.

           DELETE ARQ-VENDEDOR RECORD

           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS04
              MOVE "ERRO AO EXCLUIR VENDEDOR"
                                       TO MSG-RETORNO-HBSIS04
              PERFORM 2120-CLOSE-ARQ-VENDEDOR
              PERFORM 3000-FINALIZA
           END-IF

           .
       2220-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * ROTINA DE FINALIZAÇÃO                                          *
      *----------------------------------------------------------------*
       3000-FINALIZA                   SECTION.

           GOBACK.

       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FIM DO PROGRAMA HBSIS04P                                       *
      *----------------------------------------------------------------*
       END PROGRAM                     HBSIS04P.
      *----------------------------------------------------------------*
