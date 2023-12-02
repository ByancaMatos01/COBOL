       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE005.
      *AUTHOR Principal: Professor Angelo
      *AUTHOR-co. Byanca Matos.
      ******************************
      *     CADASTRO DE PEDIDOS   *
      ******************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQPED ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS PED-COD
                  ALTERNATE RECORD KEY IS PED-COD-PRO WITH DUPLICATES
                 ALTERNATE RECORD KEY IS PED-CLI-CPFCNPJ WITH DUPLICATES
                 ALTERNATE RECORD KEY IS PED-FOR-CPFCNPJ WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
           SELECT ARQPRO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS PRO-CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS PRO-DESCRICAO
                                   WITH DUPLICATES.

           SELECT ARQCLI ASSIGN TO DISK
                     ORGANIZATION IS INDEXED
                     ACCESS MODE  IS DYNAMIC
                     RECORD KEY   IS CLI-CPFCNPJ
                     ALTERNATE RECORD KEY IS CLI-CODIGO WITH DUPLICATES
                     ALTERNATE RECORD KEY IS CLI-NOME WITH DUPLICATES
                     FILE STATUS  IS ST-ERRO.


      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQPED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQPED.DAT".
       01 REGPED.
          03 PED-COD                 PIC 9(06).
          03 PED-TIPOPEDIDO          PIC X(15).
          03 PED-COD-PRO             PIC 9(09).
          03 PED-TIPOVENDA           PIC 9(01).
          03 PED-CLI-CPFCNPJ         PIC 9(14).
          03 PED-FOR-CPFCNPJ         PIC 9(14).
          03 PED-QUANTIDADE          PIC 9(06).
          03 PED-PRECOUNITARIO       PIC 9(08).
          03 PED-VALORTOTAL          PIC 9(09).
      *
      *-----------------------------------------------------------------
       FD ARQPRO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQPRO.DAT".
       01 REGPRO.
          03 PRO-CODIGO            PIC 9(06).
          03 PRO-DESCRICAO           PIC X(30).
          03 PRO-UNIDADE             PIC X(02).
          03 PRO-TIPOPRODUTO         PIC 9(01).
          03 PRO-PRECO               PIC 9(06).
          03 PRO-DATAULTIMA          PIC 9(08).
          03 PRO-QTDESTOQUE          PIC 9(06).
          03 PRO-CPFCNPJ             PIC 9(14).

       FD ARQCLI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCLI.DAT".
       01 REGCLI.
          03 CLI-CODIGO              PIC 9(06).
          03 CLI-CPFCNPJ             PIC 9(14).
          03 CLI-TIPOCLIENTE         PIC X(01).
          03 CLI-NOME                PIC X(30).
          03 CLI-APELIDO             PIC X(30).
          03 CLI-CEP                 PIC 9(08).
          03 CLI-LOGRADOURO          PIC X(24).
          03 CLI-NUMERO              PIC X(11).
          03 CLI-COMPLEMENTO         PIC X(24).
          03 CLI-BAIRRO              PIC X(40).
          03 CLI-CIDADE              PIC X(24).
          03 CLI-ESTADO              PIC X(02).
          03 CLI-TELEFONE            PIC 9(11).
          03 CLI-EMAIL               PIC X(33).
          03 CLI-CONTATO             PIC X(32).
      *-----------------------------------------------------------------

       WORKING-STORAGE SECTION.
       01 MASC1              PIC 9(06)V99.
       01 W-OPCAO            PIC X(01) VALUE SPACES.
       01 W-UPPERCASE        PIC X(01) VALUE SPACES.
       01 W-ACT              PIC 9(02) VALUE ZEROS.
       01 ST-ERRO            PIC X(02) VALUE "00".
       01 DPED-TIPOPEDIDO    PIC X(20) VALUE SPACES.
       01 DPRO-DESCRICAO       PIC X(20) VALUE SPACES.
       01 W-CONT             PIC 9(06) VALUE ZEROS.
       01 MENS               PIC X(50) VALUE SPACES.
       01 W-SEL              PIC 9(01) VALUE ZEROS.
       01 IND                PIC 9(02) VALUE ZEROS.
       01 TABTIPOPRODUTO.
          03 TBTIPOPRODUTO   PIC X(30) OCCURS 10 TIMES.

       01 PED-PRO-DESCRICAO PIC X(30) VALUE SPACES.

       01 PED-CLI-NOME PIC X(30) VALUE SPACES.
       01 PED-CPFCNPJ-FOR PIC X(30) VALUE SPACES.
       01 PED-FOR-NOME PIC X(30) VALUE SPACES.

      *--------------------------------------------
      *
        SCREEN SECTION.

       01  TELAPRINCIPAL.
            05  LINE 01  COLUMN 01
               VALUE  "       Cadastro de Pedidos                 ".
           05  LINE 01  COLUMN 41
               VALUE  "                                       ".
           05  LINE 02  COLUMN 01
               VALUE  " Codigo Pedido    :".
           05  LINE 02  COLUMN 41
               VALUE  "                                       ".
           05  LINE 03  COLUMN 01
               VALUE  " Tipo Pedido      :  -".
           05  LINE 03  COLUMN 41
               VALUE  "                                       ".
           05  LINE 04  COLUMN 01
               VALUE  " Codigo Produto   :".
           05  LINE 04  COLUMN 41
               VALUE  "                                       ".
           05  LINE 05  COLUMN 01
               VALUE  " Descricao Produto:".
           05  LINE 05  COLUMN 41
               VALUE  "                                       ".
           05  LINE 06  COLUMN 01
               VALUE  " Codigo Cliente   :".
           05  LINE 06  COLUMN 41
               VALUE  "                                       ".
           05  LINE 07  COLUMN 01
               VALUE  " Nome Cliente     :".
           05  LINE 07  COLUMN 41
               VALUE  "                                       ".
           05  LINE 08  COLUMN 01
               VALUE  " Codigo Fornecedor:".
           05  LINE 08  COLUMN 41
               VALUE  "                                       ".
           05  LINE 09  COLUMN 01
               VALUE  "|                   ".
           05  LINE 09  COLUMN 41
               VALUE  "                                       ".
           05  LINE 10  COLUMN 01
               VALUE  "                                        ".
           05  LINE 10  COLUMN 41
               VALUE  "                                         ".
           05  LINE 11  COLUMN 01
               VALUE  " Quantidade       :".
           05  LINE 11  COLUMN 41
               VALUE  "                                         ".
           05  LINE 12  COLUMN 01
               VALUE  " Preco Unitario   :".
           05  LINE 12  COLUMN 41
               VALUE  "                                       ".
           05  LINE 13  COLUMN 01
               VALUE  "                                        ".
           05  LINE 13  COLUMN 41
               VALUE  "                                        ".
           05  LINE 14  COLUMN 01
               VALUE  " Valor Total      :".
           05  LINE 14  COLUMN 41
               VALUE  "                                       ".
           05  LINE 15  COLUMN 01
               VALUE  "                                        ".
           05  LINE 15  COLUMN 41
               VALUE  "                                         ".
           05  LINE 16  COLUMN 01
               VALUE  "| Notificacoes:".
           05  LINE 16  COLUMN 41
               VALUE  "                                       ".
           05  LINE 17  COLUMN 01
               VALUE  "                                        ".
           05  LINE 17  COLUMN 41
               VALUE  "                                        ".
           05  LINE 18  COLUMN 01
               VALUE  "|                                  Infor".
           05  LINE 18  COLUMN 41
               VALUE  "macoes                                 ".
           05  LINE 19  COLUMN 01
               VALUE  "                                       ".
           05  LINE 19  COLUMN 41
               VALUE  "                                       ".
           05  LINE 20  COLUMN 01
               VALUE  "                                         ".
           05  LINE 20  COLUMN 41
               VALUE  "                                       ".
           05  LINE 21  COLUMN 01
               VALUE  "                                        ".
           05  LINE 21  COLUMN 41
               VALUE  "                                       ".
           05  LINE 22  COLUMN 01
               VALUE  "                                        ".
           05  LINE 22  COLUMN 41
               VALUE  "                                       ".
           05  LINE 23  COLUMN 01
               VALUE  "                                        ".
           05  LINE 24  COLUMN 01
               VALUE  "                                         ".
           05  LINE 24  COLUMN 41
               VALUE  "                                         ".
           05  TPED-COD
               LINE 02  COLUMN 22  PIC 9(06)
               USING  PED-COD.
           05  TPED-TIPOPEDIDO
               LINE 03  COLUMN 22  PIC X(01)
               USING  PED-TIPOPEDIDO.
           05  TDPED-TIPOPEDIDO
               LINE 03  COLUMN 24  PIC X(10)
               USING  DPED-TIPOPEDIDO.

           05  TPED-COD-PRO
               LINE 04  COLUMN 22  PIC 9(06)
               USING  PED-COD-PRO.

           05  TPED-PRO-DESCRICAO
               LINE 05  COLUMN 22  PIC X(30)
               USING  PRO-DESCRICAO.



           05  TPED-CLI-CPFCNPJ
               LINE 06  COLUMN 22  PIC 9(14)
               USING  PED-CLI-CPFCNPJ.

           05  TPED-CLI-NOME
               LINE 07  COLUMN 22  PIC X(30)
               USING  CLI-NOME.
           05  TPED-CPFCNPJ-FOR
               LINE 08  COLUMN 22  PIC X(14)
               USING  PRO-CPFCNPJ.
           05  TPED-FOR-NOME
               LINE 09  COLUMN 22  PIC X(30)
               USING  PED-FOR-NOME.
           05  TPED-QUANTIDADE
               LINE 11  COLUMN 22  PIC 9(06)
               USING  PED-QUANTIDADE.
           05  TPED-PRECOUNITARIO
               LINE 12  COLUMN 22  PIC 9(06)V99
               USING  PRO-PRECO.
           05  TPED-VALORTOTAL
               LINE 14  COLUMN 22  PIC 9(08)V99
               USING  PED-VALORTOTAL.

       01  TELATIPOPRODUTO.
           05  LINE 18  COLUMN 01
               VALUE  "                                         ".
           05  LINE 19  COLUMN 01
               VALUE  "                                          ".
           05  LINE 19  COLUMN 41
               VALUE  "                                       ".
           05  LINE 20  COLUMN 01
               VALUE  " 1-SAUDE           2-ALIMENTO           ".
           05  LINE 20  COLUMN 41
               VALUE  " 3-AGROPECUARIA                         ".
           05  LINE 21  COLUMN 01
               VALUE  " 4-PET                       5- Cosmeticos     ".
           05  LINE 21  COLUMN 41
               VALUE  " 6-esportivo                                   ".
           05  LINE 22  COLUMN 01
               VALUE  "7-geek                 8-escolar               ".
           05  LINE 22  COLUMN 41
               VALUE  " 9-outros                                      ".
           05  LINE 23  COLUMN 01
               VALUE  "|".
           05  LINE 23  COLUMN 41
               VALUE  "                                       ".
           05  LINE 24  COLUMN 01
               VALUE  "                                        ".
           05  LINE 24  COLUMN 41
               VALUE  "                                         ".

           01  TELAUNIDADE.

           05  LINE 18  COLUMN 01
               VALUE  "                                         ".
           05  LINE 19  COLUMN 01
               VALUE  "                                        ".
           05  LINE 19  COLUMN 41
               VALUE  "                                        ".
           05  LINE 20  COLUMN 01
               VALUE  " PC-PECA                                 ".
           05  LINE 20  COLUMN 41
               VALUE  "                                     ".
           05  LINE 21  COLUMN 01
               VALUE  " LT-LITRO                             ".
           05  LINE 21  COLUMN 41
               VALUE  "                                       ".
           05  LINE 22  COLUMN 01
               VALUE  "KL-KILO                                 ".
           05  LINE 22  COLUMN 41
               VALUE  "                                        ".
           05  LINE 23  COLUMN 01
               VALUE  "TN-TONELADA".
           05  LINE 23  COLUMN 41
               VALUE  "                                        ".
           05  LINE 24  COLUMN 01
               VALUE  "                                        ".
           05  LINE 24  COLUMN 41
               VALUE  "                                        ".




       01  TELALIMPAR.
           05  LINE 19  COLUMN 01
               VALUE  "                                              ".
           05  LINE 19  COLUMN 41
               VALUE  "                                              ".
           05  LINE 20  COLUMN 01
               VALUE  "                                               ".
           05  LINE 20  COLUMN 41
               VALUE  "                                               ".
           05  LINE 21  COLUMN 01
               VALUE  "                                               ".
           05  LINE 21  COLUMN 41
               VALUE  "                                              ".
           05  LINE 22  COLUMN 01
               VALUE  "                                               ".
           05  LINE 22  COLUMN 41
               VALUE  "                                               ".
           05  LINE 23  COLUMN 01
               VALUE  "                                               ".
           05  LINE 23  COLUMN 41
               VALUE  "                                               ".
           05  LINE 24  COLUMN 01
               VALUE  "                                               ".
           05  LINE 24  COLUMN 41
               VALUE  "                                               ".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

      *-------------ABERTURA DO ARQUIVO -------------------
       R0.    OPEN I-O ARQPED
           IF ST-ERRO NOT = "00"
            IF ST-ERRO = "30"
              OPEN OUTPUT ARQPED
              MOVE "CRIANDO ARQUIVO CADASTRO DE FORNECEDOR " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQPED
              GO TO INICIO
            ELSE
              IF ST-ERRO = "95"
                MOVE "ISAM NAO CARREGADO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
              ELSE
                MOVE "ERRO NA ABERTURA DO ARQUIVO CDOPEDIDO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       R0A.
           OPEN INPUT ARQPRO
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE PROUTOS NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE PRODUTOS " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0B.
           OPEN INPUT ARQCLI
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                MOVE "*** ARQUIVO DE CLIENTES NAO ENCONTRADO **" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CLIENTES " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

      *------------- INICIALIZACAO DAS VARIAVEIS------------
       R1.
           MOVE SPACES TO PED-TIPOPEDIDO DPED-TIPOPEDIDO

           MOVE ZEROS TO PED-COD PED-COD-PRO PED-TIPOVENDA
           PED-CLI-CPFCNPJ PED-FOR-CPFCNPJ PED-QUANTIDADE
           PED-PRECOUNITARIO PED-VALORTOTAL

           MOVE SPACES TO PRO-DESCRICAO PRO-UNIDADE

           MOVE ZEROS TO PRO-CODIGO PRO-TIPOPRODUTO PRO-PRECO
           PRO-DATAULTIMA PRO-QTDESTOQUE PRO-CPFCNPJ.

           MOVE SPACES TO
           CLI-TIPOCLIENTE
           CLI-NOME CLI-APELIDO CLI-LOGRADOURO CLI-NUMERO
           CLI-COMPLEMENTO CLI-BAIRRO CLI-CIDADE CLI-ESTADO
           CLI-EMAIL CLI-CONTATO.

           MOVE ZEROS TO CLI-CODIGO CLI-CPFCNPJ CLI-CEP
           CLI-TELEFONE

      *-------------VISUALIZACAO DA TELA -------------------------------

           DISPLAY TELAPRINCIPAL.
      *----------ENTRADA DE DADOS---------------------------------------

       R2.
           ACCEPT TPED-COD.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                  CLOSE ARQPED
                  STOP RUN.
           IF PED-COD = SPACES
              MOVE "*** CODIGO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.
       LER-ARQPED.
           READ ARQPED
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R3A
                PERFORM R5A THRU R5B
                PERFORM R4A THRU R4B
      *          DISPLAY TELAPRINCIPAL
                MOVE "*** PEDIDO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO PRODUTOS" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** PEDIDO NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.
           ACCEPT TPED-TIPOPEDIDO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R2.
           IF PED-TIPOPEDIDO = SPACES
              MOVE "TIPO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.

       R3A.
           IF PED-TIPOPEDIDO = 1
              MOVE "COMPRA" TO DPED-TIPOPEDIDO
            DISPLAY TPED-TIPOPEDIDO
           DISPLAY TELALIMPAR
           DISPLAY TELAPRINCIPAL

           ELSE
             IF PED-TIPOPEDIDO = 2
                MOVE "VENDA" TO DPED-TIPOPEDIDO
             ELSE
              MOVE "1 - COMPRA : 2 - VENDA" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R3.
           DISPLAY TPED-TIPOPEDIDO.
           DISPLAY TELALIMPAR.
           DISPLAY TELAPRINCIPAL.

       R4.
           ACCEPT TPED-COD-PRO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPRINCIPAL
                   GO TO R3.
           IF PED-COD-PRO = 0
               MOVE "*** CODIGO PRODUTO NÃO INFORMADO  ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4.

       R4A.
           MOVE PED-COD-PRO TO PRO-CODIGO.
           READ ARQPRO
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
               MOVE "*** PRODUTO DIGITADO NAO ENCONTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
               ELSE
               MOVE "ERRO NA LEITURA ARQUIVO PRODUTO" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                MOVE 1 TO IND.
                DISPLAY TELAPRINCIPAL.
       R4B.
               MOVE 1 TO IND.


       R5.
           IF PED-TIPOPEDIDO = 1
              MOVE "COMPRA" TO DPED-TIPOPEDIDO
              MOVE 0 TO PED-CLI-CPFCNPJ
              GO TO R6
           ELSE

              DISPLAY TPED-CLI-CPFCNPJ
           ACCEPT TPED-CLI-CPFCNPJ
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPRINCIPAL
                   GO TO R3.
           IF PED-CLI-CPFCNPJ = 0
               MOVE "*** CODIGO PRODUTO NÃO INFORMADO  ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4.

       R5A.
           MOVE PED-CLI-CPFCNPJ TO CLI-CPFCNPJ.
           READ ARQCLI
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
               MOVE "*** CLIENTE DIGITADO NAO ENCONTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4
               ELSE
               MOVE "ERRO NA LEITURA ARQUIVO CLIENTE" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                MOVE 1 TO IND.
                DISPLAY TELAPRINCIPAL.
       R5B.
               MOVE 1 TO IND.

       R6.
           ACCEPT TPED-QUANTIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPRINCIPAL
                   GO TO R4.
           IF PED-QUANTIDADE = 0
                 MOVE "*** QTD ESTOQUE NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5.

                 COMPUTE PED-VALORTOTAL = (PRO-PRECO
           * PED-QUANTIDADE).

       R7.
           MOVE PED-VALORTOTAL TO MASC1
           DISPLAY TPED-VALORTOTAL.


             IF W-SEL = 1
            GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (16, 30) "Deseja Salvar (S/N) : ".
                ACCEPT (16, 55) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R1.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPED
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                MOVE "* PRODUTO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (20, 18)
                     "N=NOVO REGISTRO | A=ALTERAR | E=EXCLUIR | S=SAIR:"
                ACCEPT (20, 67) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A"
                    AND W-OPCAO NOT = "E" AND W-OPCAO NOT = "S"
                    GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (20, 18) MENS
                IF W-OPCAO = "N" OR "n"
                   GO TO R1
                ELSE
                   IF W-OPCAO = "A" OR "a"
                      MOVE 1 TO W-SEL
                      GO TO R3
                ELSE
                   IF W-OPCAO = "S" OR "s"
                      MOVE 1 TO W-SEL
                      GO TO ROT-FIM.
      *
       EXC-OPC.
                DISPLAY (16, 30) "Deseja EXCLUIR   (S/N) : ".
                ACCEPT (16, 55) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQPED RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO PRODUTO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ALT-OPC.
                DISPLAY (16, 30) "Deseja ALTERAR  (S/N) : ".
                ACCEPT (16, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY

                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.

                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPED
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM

                   GO TO R1.
                   DISPLAY TELALIMPAR.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

      *-----------------------------------------------------------------
       ROT-FIM.
           CLOSE ARQPED ARQPRO ARQCLI.
           STOP RUN.
      *--------------------------------------------------------------
      *---------[ ROTINA DE MENSAGEM ]-------------------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (16, 21) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (16, 21) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      *    95 = ISAM NAO CARREGADO
      *    10 = FIM DA LEITURA ARQUIVO SEQUENCIAL
