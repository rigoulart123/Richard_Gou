       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     HBSIS02P.
      *----------------------------------------------------------------*
      * ANALISTA.....: RICHARD GOULART                                 *
      * DATA.........: 17/02/2019                                      *
      * OBJETIVO.....: REALIZAR ACESSOS AO ARQUIVO DE CLIENTES AMBEV   *
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

           SELECT IMP-CLIENTE        ASSIGN TO W-LABEL-IMP
                               ORGANIZATION IS SEQUENTIAL
                                FILE STATUS IS WS-FL-STATUS-IMP.
      *----------------------------------------------------------------*
       DATA                            DIVISION.
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-CLIENTE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'ArqCliente'.
       COPY "HBSIS02C.CPY".

       FD  IMP-CLIENTE
           LABEL RECORD IS STANDARD.
       01  IMP-CLIENTE-REG.
           03 IMP-COD-CLIENTE          PIC  9(007).
           03 IMP-CNPJ                 PIC  9(014).
           03 IMP-RAZAO-SOCIAL         PIC  X(040).
           03 IMP-LATITUDE             PIC S9(003)V9(008).
           03 IMP-LONGITUDE            PIC S9(003)V9(008).
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  WS-FL-STATUS-CLI            PIC  X(002)         VALUE "00".
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
       01  WS-CODIGO-CLI               PIC  9(007)         VALUE ZEROS.
       01  WS-CNPJ-CLI                 PIC  9(014)         VALUE ZEROS.
       01  WS-RAZAO-SOCIAL             PIC  X(040)         VALUE SPACES.
       01  WS-LATITUDE-CLI             PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-LONGITUDE-CLI            PIC S9(003)V9(008)  VALUE ZEROS.
      *
       01  WS-ARQ-CLIENTE.
           05  WS-ARQ-CODIGO-CLI       PIC  9(007)         VALUE ZEROS.
           05  WS-ARQ-CNPJ-CLI         PIC  9(014)         VALUE ZEROS.
           05  WS-ARQ-RAZAO-SOCIAL-CLI PIC  X(040)         VALUE SPACES.
           05  WS-ARQ-LATITUDE-CLI     PIC S9(003)V9(008)  VALUE ZEROS.
           05  WS-ARQ-LONGITUDE-CLI    PIC S9(003)V9(008)  VALUE ZEROS.
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       COPY HBSIS02L.
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION USING HBSIS02L.
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

           EVALUATE COD-FUNCAO-HBSIS02
               WHEN 0
                    PERFORM 2100-BUSCAR-CLIENTE
               WHEN 1
                    PERFORM 2200-INCLUIR-CLIENTE
               WHEN 2
                    PERFORM 2300-ALTERAR-CLIENTE
               WHEN 3
                    PERFORM 2400-EXCLUIR-CLIENTE
               WHEN OTHER
                    MOVE 9             TO COD-RETORNO-HBSIS02
                    MOVE 'CODIGO DA FUNCAO INVALIDA'
                                       TO MSG-RETORNO-HBSIS02
                    PERFORM 3000-FINALIZA
           END-EVALUATE

           .
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE BUSCA DE CLIENTE                                     *
      *----------------------------------------------------------------*
       2100-BUSCAR-CLIENTE             SECTION.

           PERFORM 2110-OPEN-ARQ-CLIENTE

           MOVE COD-CLIENTE-HBSIS02    TO COD-CLIENTE-HBSIS02C

           READ ARQ-CLIENTE   RECORD INTO WS-ARQ-CLIENTE
                              KEY IS      COD-CLIENTE-HBSIS02C

           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              MOVE WS-ARQ-CODIGO-CLI   TO COD-CLIENTE-HBSIS02
              MOVE WS-ARQ-CNPJ-CLI     TO CNPJ-HBSIS02
              MOVE WS-ARQ-RAZAO-SOCIAL-CLI
                                       TO RAZAO-SOCIAL-HBSIS02
              MOVE WS-ARQ-LATITUDE-CLI TO LATITUDE-CLI-HBSIS02
              MOVE WS-ARQ-LATITUDE-CLI TO LATITUDE-CLI-HBSIS02
              MOVE ZEROS               TO COD-RETORNO-HBSIS02
           ELSE
              MOVE CNPJ-HBSIS02        TO CNPJ-HBSIS02C

              READ ARQ-CLIENTE RECORD INTO WS-ARQ-CLIENTE
                               KEY IS      CNPJ-HBSIS02C

              IF WS-FL-STATUS-CLI      EQUAL ZEROS
                 MOVE WS-ARQ-CODIGO-CLI
                                       TO COD-CLIENTE-HBSIS02
                 MOVE WS-ARQ-CNPJ-CLI  TO CNPJ-HBSIS02
                 MOVE WS-ARQ-RAZAO-SOCIAL-CLI
                                       TO RAZAO-SOCIAL-HBSIS02
                 MOVE WS-ARQ-LATITUDE-CLI
                                       TO LATITUDE-CLI-HBSIS02
                 MOVE WS-ARQ-LONGITUDE-CLI
                                       TO LONGITUDE-CLI-HBSIS02
                 MOVE ZEROS            TO COD-RETORNO-HBSIS02
              ELSE
                 MOVE 1                TO COD-RETORNO-HBSIS02
                 MOVE "CLIENTE NAO ENCONTRADO"
                                       TO MSG-RETORNO-HBSIS02
              END-IF
           END-IF

           PERFORM 2120-CLOSE-ARQ-CLIENTE

           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ABERTURA DE ARQUIVO                                            *
      *----------------------------------------------------------------*
       2110-OPEN-ARQ-CLIENTE           SECTION.

           OPEN I-O ARQ-CLIENTE

           IF WS-FL-STATUS-CLI         EQUAL ZEROS OR '05'
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS02
              MOVE "ERRO NA ABERTURA DO ARQUIVO"
                                       TO MSG-RETORNO-HBSIS02
              PERFORM 2120-CLOSE-ARQ-CLIENTE
              PERFORM 3000-FINALIZA
           END-IF

           .
       2110-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FECHAMENTO DE ARQUIVO                                          *
      *----------------------------------------------------------------*
       2120-CLOSE-ARQ-CLIENTE          SECTION.

           CLOSE ARQ-CLIENTE

           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS02
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO"
                                       TO MSG-RETORNO-HBSIS02
              PERFORM 2120-CLOSE-ARQ-CLIENTE
              PERFORM 3000-FINALIZA
           END-IF

           .
       2120-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INCLUSÃO DE CLIENTE                                  *
      *----------------------------------------------------------------*
       2200-INCLUIR-CLIENTE            SECTION.

           PERFORM 2110-OPEN-ARQ-CLIENTE

           MOVE COD-CLIENTE-HBSIS02    TO COD-CLIENTE-HBSIS02C

           READ ARQ-CLIENTE   RECORD INTO WS-ARQ-CLIENTE
                              KEY IS      COD-CLIENTE-HBSIS02C

           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              MOVE 1                   TO COD-RETORNO-HBSIS02
              MOVE "CLIENTE JA CADASTRADO"
                                       TO MSG-RETORNO-HBSIS02
           ELSE
              MOVE CNPJ-HBSIS02        TO CNPJ-HBSIS02C

              READ ARQ-CLIENTE RECORD INTO WS-ARQ-CLIENTE
                               KEY IS      CNPJ-HBSIS02C

              IF WS-FL-STATUS-CLI      EQUAL ZEROS
                 MOVE 1                TO COD-RETORNO-HBSIS02
                 MOVE "CLIENTE JA CADASTRADO"
                                       TO MSG-RETORNO-HBSIS02
              ELSE
                 MOVE COD-CLIENTE-HBSIS02
                                       TO WS-ARQ-CODIGO-CLI
                 MOVE CNPJ-HBSIS02     TO WS-ARQ-CNPJ-CLI
                 MOVE RAZAO-SOCIAL-HBSIS02
                                       TO WS-ARQ-RAZAO-SOCIAL-CLI
                 MOVE LATITUDE-CLI-HBSIS02
                                       TO WS-ARQ-LATITUDE-CLI
                 MOVE LONGITUDE-CLI-HBSIS02
                                       TO WS-ARQ-LONGITUDE-CLI
                 PERFORM 2220-GRAVA-ARQ-CLIENTE
                 MOVE ZEROS            TO COD-RETORNO-HBSIS02
                 MOVE "CLIENTE CADASTRADO COM SUCESSO"
                                       TO MSG-RETORNO-HBSIS02
              END-IF
           END-IF

           PERFORM 2120-CLOSE-ARQ-CLIENTE

           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE GRAVAÇÃO DO ARQUIVO DE CLIENTE                       *
      *----------------------------------------------------------------*
       2220-GRAVA-ARQ-CLIENTE          SECTION.

           MOVE WS-ARQ-CLIENTE         TO ARQ-HBSIS02C

           WRITE ARQ-HBSIS02C

           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS02
              MOVE "ERRO AO GRAVAR O CLIENTE"
                                       TO MSG-RETORNO-HBSIS02
              PERFORM 2120-CLOSE-ARQ-CLIENTE
              PERFORM 3000-FINALIZA
           END-IF

           .
       2220-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE ALTERACÃO DE CLIENTE                                 *
      *----------------------------------------------------------------*
       2300-ALTERAR-CLIENTE            SECTION.

           PERFORM 2110-OPEN-ARQ-CLIENTE

           MOVE COD-CLIENTE-HBSIS02    TO COD-CLIENTE-HBSIS02C

           READ ARQ-CLIENTE   RECORD INTO WS-ARQ-CLIENTE
                              KEY IS      COD-CLIENTE-HBSIS02C

           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              MOVE RAZAO-SOCIAL-HBSIS02
                                       TO WS-ARQ-RAZAO-SOCIAL-CLI
              MOVE LATITUDE-CLI-HBSIS02
                                       TO WS-ARQ-LATITUDE-CLI
              MOVE LONGITUDE-CLI-HBSIS02
                                       TO WS-ARQ-LONGITUDE-CLI
              PERFORM 2310-ALTERAR-ARQ-CLI
              MOVE 0                   TO COD-RETORNO-HBSIS02
              MOVE "CLIENTE ALTERADO COM SUCESSO"
                                       TO MSG-RETORNO-HBSIS02
           ELSE
              MOVE CNPJ-HBSIS02        TO CNPJ-HBSIS02C

              READ ARQ-CLIENTE RECORD INTO WS-ARQ-CLIENTE
                               KEY IS      CNPJ-HBSIS02C

              IF WS-FL-STATUS-CLI      EQUAL ZEROS
                 MOVE RAZAO-SOCIAL-HBSIS02
                                       TO WS-ARQ-RAZAO-SOCIAL-CLI
                 MOVE LATITUDE-CLI-HBSIS02
                                       TO WS-ARQ-LATITUDE-CLI
                 MOVE LONGITUDE-CLI-HBSIS02
                                       TO WS-ARQ-LONGITUDE-CLI
                 PERFORM 2310-ALTERAR-ARQ-CLI
                 MOVE 0                TO COD-RETORNO-HBSIS02
                 MOVE "CLIENTE ALTERADO COM SUCESSO"
                                       TO MSG-RETORNO-HBSIS02
              ELSE
                 MOVE 1                TO COD-RETORNO-HBSIS02
                 MOVE "ERRO AO ALTERAR CLIENTE"
                                       TO MSG-RETORNO-HBSIS02
              END-IF
           END-IF

           PERFORM 2120-CLOSE-ARQ-CLIENTE

           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE REGRAVAÇÃO DO ARQUIVO DE CLIENTE                     *
      *----------------------------------------------------------------*
       2310-ALTERAR-ARQ-CLI          SECTION.

           REWRITE ARQ-HBSIS02C

           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS02
              MOVE "ERRO AO ALTERAR CLIENTE"
                                       TO MSG-RETORNO-HBSIS02
              PERFORM 3000-FINALIZA
           END-IF

           .
       2310-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE EXCLUSÃO DE CLIENTE                                  *
      *----------------------------------------------------------------*
       2400-EXCLUIR-CLIENTE            SECTION.

           PERFORM 2110-OPEN-ARQ-CLIENTE

           MOVE COD-CLIENTE-HBSIS02   TO COD-CLIENTE-HBSIS02C

           READ ARQ-CLIENTE   RECORD INTO WS-ARQ-CLIENTE
                              KEY IS      COD-CLIENTE-HBSIS02C

           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              PERFORM 2410-EXCLUIR-ARQ-CLI
              MOVE 0                   TO COD-RETORNO-HBSIS02
              MOVE "CLIENTE EXCLUIDO COM SUCESSO"
                                       TO MSG-RETORNO-HBSIS02
           ELSE
              MOVE CNPJ-HBSIS02        TO CNPJ-HBSIS02C

              READ ARQ-CLIENTE RECORD INTO WS-ARQ-CLIENTE
                               KEY IS      CNPJ-HBSIS02C

              IF WS-FL-STATUS-CLI      EQUAL ZEROS
                 PERFORM 2410-EXCLUIR-ARQ-CLI
                 MOVE 0                   TO COD-RETORNO-HBSIS02
                 MOVE "CLIENTE EXCLUIDO COM SUCESSO"
                                       TO MSG-RETORNO-HBSIS02
              ELSE
                 MOVE 1                TO COD-RETORNO-HBSIS02
                 MOVE "ERRO AO EXCLUIR CLIENTE"
                                       TO MSG-RETORNO-HBSIS02
              END-IF
           END-IF

           PERFORM 2120-CLOSE-ARQ-CLIENTE

           .
       2400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE EXCLUSÃO DO ARQUIVO DE CLIENTE                       *
      *----------------------------------------------------------------*
       2410-EXCLUIR-ARQ-CLI          SECTION.

           DELETE ARQ-CLIENTE RECORD

           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS02
              MOVE "ERRO AO EXCLUIR CLIENTE"
                                       TO MSG-RETORNO-HBSIS02
              PERFORM 2120-CLOSE-ARQ-CLIENTE
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
      * FIM DO PROGRAMA HBSIS02P                                       *
      *----------------------------------------------------------------*
       END PROGRAM                     HBSIS02P.
      *----------------------------------------------------------------*
