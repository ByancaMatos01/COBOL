       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE004.
      *AUTHOR. Byanca Matos.
      ******************************
      *     CADASTRO DE PRODUTOS   *
      ******************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      **Esta é uma divisão obrigatória em um programa COBOL e fornece
      *informações sobre o ambiente em que o programa será executado.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQPRO ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS PRO-CODIGO
                  ALTERNATE RECORD KEY IS PRO-DESCRICAO WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
           SELECT ARQFOR ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS FOR-CPFCNPJ
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS FOR-CODIGO
                                   WITH DUPLICATES.
      *Especifica a chave primária para o arquivo.
      * Em ARQFOR, a chave primária é FOR-CPFCNPJ, e em ARQPRO, é PRO-CODIGO.

      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQPRO
      *-FD é usado para descrever os detalhes do arquivo.
      *-No caso, o arquivo é chamado "ARQPRO" e está associado ao identificador "ARQPRO.DAT".
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQPRO.DAT".
       01 REGPRO.
          03 CHAVE1.
            05 PRO-CODIGO            PIC 9(06).
          03 PRO-DESCRICAO           PIC X(30).
          03 PRO-UNIDADE             PIC X(02).
          03 PRO-TIPOPRODUTO         PIC 9(01).
          03 PRO-PRECO               PIC 9(06).
          03 PRO-DATAULTIMA          PIC 9(08).
          03 PRO-QTDESTOQUE          PIC 9(06).
          03 PRO-CPFCNPJ             PIC 9(14).

      *
      *-----------------------------------------------------------------
       FD ARQFOR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFOR.DAT".
       01 REGFOR.
          03 FOR-CODIGO              PIC 9(06).
          03 FOR-TIPOFORNECEDOR      PIC X(01).
          03 FOR-CPFCNPJ             PIC 9(14).
          03 FOR-NOME                PIC X(30).
          03 FOR-APELIDO             PIC X(30).
          03 FOR-CEP                 PIC 9(08).
          03 FOR-LOGRADOURO          PIC X(24).
          03 FOR-NUMERO              PIC X(11).
          03 FOR-COMPLEMENTO         PIC X(24).
          03 FOR-BAIRRO              PIC X(40).
          03 FOR-CIDADE              PIC X(24).
          03 FOR-ESTADO              PIC X(02).
          03 FOR-TELEFONE            PIC 9(11).
          03 FOR-EMAIL               PIC X(33).
          03 FOR-CONTATO             PIC X(32).
      *-----------------------------------------------------------------

       WORKING-STORAGE SECTION.
       01 W-OPCAO            PIC X(01) VALUE SPACES.
      *-Variável usada para armazenar uma opção do usuário.
       01 W-UPPERCASE        PIC X(01) VALUE SPACES.
      *-Variável usada para armazenar a opção de conversão para maiúsculas.
       01 W-ACT              PIC 9(02) VALUE ZEROS.
      *-Variável numérica usada para armazenar uma ação ou um código de ação.
       01 ST-ERRO            PIC X(02) VALUE "00".
      *-Variável usada para armazenar o status de erro de um arquivo.
      *-É um campo alfanumérico com um tamanho de 2 bytes e inicializado com "00".
       01 DPRO-TIPOPRODUTO   PIC X(20) VALUE SPACES.
       01 DPRO-UNIDADE       PIC X(20) VALUE SPACES.
       01 W-CONT             PIC 9(06) VALUE ZEROS.
       01 MENS               PIC X(50) VALUE SPACES.
       01 W-SEL              PIC 9(01) VALUE ZEROS.
       01 IND                PIC 9(02) VALUE ZEROS.
       01 TABTIPOPRODUTO.
          03 TBTIPOPRODUTO   PIC X(30) OCCURS 10 TIMES.

      *----------------tela TELAPRINCIPAL-------------------------------
      *
        SCREEN SECTION.

       01  TELAPRINCIPAL.
           05  LINE 01  COLUMN 01
               VALUE  " Cadastro de PRODUTOS ".
           05  LINE 02  COLUMN 01
               VALUE  "                                       ".
           05  LINE 02  COLUMN 41
               VALUE  "                                       ".
           05  LINE 03  COLUMN 01
               VALUE  "Dados Produtos".
           05  LINE 04  COLUMN 01
               VALUE  "  Codigo             :".
           05  LINE 04  COLUMN 41
               VALUE  "                                       ".
           05  LINE 05  COLUMN 01
               VALUE  "  Descricao          :".
           05  LINE 05  COLUMN 41
               VALUE  "                                       ".
           05  LINE 06  COLUMN 01
               VALUE  "  Unidade           :   -".
           05  LINE 06  COLUMN 41
               VALUE  "                                       ".
           05  LINE 07  COLUMN 01
               VALUE  " Tipo de Produto       :   -".
           05  LINE 07  COLUMN 41
               VALUE  "                                       ".
           05  LINE 08  COLUMN 01
               VALUE  "  Ultimo Preco       :".
           05  LINE 08  COLUMN 41
               VALUE  "                                       ".
           05  LINE 09  COLUMN 01
               VALUE  "  Data Ultima Compra :".
           05  LINE 09  COLUMN 41
               VALUE  "                                       ".
           05  LINE 10  COLUMN 01
               VALUE  "  Quantida Estoque   :".
           05  LINE 10  COLUMN 41
               VALUE  "                                       ".
           05  LINE 11  COLUMN 01
               VALUE  "                                        ".
           05  LINE 11  COLUMN 41
               VALUE  "                                       ".
           05  LINE 12  COLUMN 01
               VALUE  " Dados do Fornecedor ".
           05  LINE 13  COLUMN 01
               VALUE  " CPF / CNPJ        :".
           05  LINE 13  COLUMN 41
               VALUE  "    Codigo Fornecedor:                 ".
           05  LINE 14  COLUMN 01
               VALUE  "  Nome              :".
           05  LINE 14  COLUMN 41
               VALUE  "    Telefone         :                 ".
           05  LINE 15  COLUMN 01
               VALUE  "                                        ".
           05  LINE 15  COLUMN 41
               VALUE  "                                         ".
           05  LINE 16  COLUMN 01
               VALUE  "| Notificacoes :".
           05  LINE 16  COLUMN 41
               VALUE  "                                       ".
           05  LINE 17  COLUMN 01
               VALUE  "                                        ".
           05  LINE 17  COLUMN 41
               VALUE  "                                         ".
           05  LINE 18  COLUMN 01
               VALUE  "                     Informacoes ".

           05  LINE 19  COLUMN 01
               VALUE  "|".
           05  LINE 19  COLUMN 41
               VALUE  "                                       ".
           05  LINE 20  COLUMN 01
               VALUE  " ".
           05  LINE 20  COLUMN 41
               VALUE  "                                       ".
           05  LINE 21  COLUMN 01
               VALUE  " ".
           05  LINE 21  COLUMN 41
               VALUE  "                                       ".
           05  LINE 22  COLUMN 01
               VALUE  " ".
           05  LINE 22  COLUMN 41
               VALUE  "                                       ".
           05  LINE 23  COLUMN 01
               VALUE  " ".
           05  LINE 23  COLUMN 41
               VALUE  "                                       ".
           05  LINE 24  COLUMN 01
               VALUE  "                                          ".
           05  LINE 24  COLUMN 41
               VALUE  "                                           ".
           05  TPRO-CODIGO
               LINE 04  COLUMN 25  PIC 9(06)
               USING  PRO-CODIGO.
           05  TPRO-DESCRICAO
               LINE 05  COLUMN 25  PIC X(30)
               USING  PRO-DESCRICAO.
           05  TPRO-UNIDADE
               LINE 06  COLUMN 25  PIC X(02)
               USING  PRO-UNIDADE.
           05  TDPRO-UNIDADE
               LINE 06  COLUMN 28  PIC X(15)
               USING  DPRO-UNIDADE.
           05  TPRO-TIPOPRODUTO
               LINE 07  COLUMN 25  PIC 9(01)
               USING  PRO-TIPOPRODUTO.
           05  TDPRO-TIPOPRODUTO
               LINE 07  COLUMN 28  PIC X(15)
               USING  DPRO-TIPOPRODUTO.
           05  TPRO-PRECO
               LINE 08  COLUMN 25  PIC 9(6)V99
               USING  PRO-PRECO.
           05  TPRO-DATAULTIMA
               LINE 09  COLUMN 25  PIC XX/XX/XX
               USING  PRO-DATAULTIMA.
           05  TPRO-QTDESTOQUE
               LINE 10  COLUMN 25  PIC 9(06)
               USING  PRO-QTDESTOQUE.
           05  TPRO-CPFCNPJ
               LINE 13  COLUMN 25  PIC 9(14)
               USING  PRO-CPFCNPJ.
           05  TFOR-CODIGO
               LINE 13  COLUMN 66  PIC 9(13)
               USING  FOR-CODIGO.
           05  TFOR-NOME
               LINE 14  COLUMN 25  PIC X(20)
               USING  FOR-NOME.

           05  TFOR-TELEFONE
               LINE 14  COLUMN 66  PIC 99.99999.9999
               USING  FOR-TELEFONE.
      *-------------------Tela de Produto-------------------------------
       01  TELATIPOPRODUTO.
           05  LINE 18  COLUMN 01
               VALUE  "                                         ".
           05  LINE 19  COLUMN 01
               VALUE  "                                          ".
           05  LINE 19  COLUMN 41
               VALUE  "                                       ".
           05  LINE 20  COLUMN 01
               VALUE  "         1-SAUDE                     2-".
           05  LINE 20  COLUMN 41
               VALUE  "ALIMENTO          3-AGROPECUARIA       ".
           05  LINE 21  COLUMN 01
               VALUE  "        4-PET                       5-".
           05  LINE 21  COLUMN 41
               VALUE  "Cosmeticos          6-esportivo       ".
           05  LINE 22  COLUMN 01
               VALUE  "|         7-geek                 8-".
           05  LINE 22  COLUMN 41
               VALUE  "escolar          9-outros       ".
           05  LINE 23  COLUMN 01
               VALUE  "|".
           05  LINE 23  COLUMN 41
               VALUE  "                                       ".
           05  LINE 24  COLUMN 01
               VALUE  "                                        ".
           05  LINE 24  COLUMN 41
               VALUE  "                                         ".
      *----------------------------TELA UNIDADE-------------------------
       01  TELAUNIDADE.

           05  LINE 18  COLUMN 01
               VALUE  "                                        ".
           05  LINE 19  COLUMN 01
               VALUE  "                                         ".
           05  LINE 19  COLUMN 41
               VALUE  "                                        ".
           05  LINE 20  COLUMN 01
               VALUE  "                                  PC-PEC".
           05  LINE 20  COLUMN 41
               VALUE  "A                                       ".
           05  LINE 21  COLUMN 01
               VALUE  "                                  LT-LIT".
           05  LINE 21  COLUMN 41
               VALUE  "RO                                      ".
           05  LINE 22  COLUMN 01
               VALUE  "                                  KL-KIL".
           05  LINE 22  COLUMN 41
               VALUE  "O                                       ".
           05  LINE 23  COLUMN 01
               VALUE  "                                TN-TON".
           05  LINE 23  COLUMN 41
               VALUE  "ELADA                                   ".
           05  LINE 24  COLUMN 01
               VALUE  "                                        ".
           05  LINE 24  COLUMN 41
               VALUE  "                                        ".
      *-----------------------------------------------------------------
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
            MOVE "SAUDE"                         TO TBTIPOPRODUTO(1)
            MOVE "ALIMENTO"                      TO TBTIPOPRODUTO(2)
            MOVE "AGROPECUARIA"                  TO TBTIPOPRODUTO(3)
            MOVE "PET "                          TO TBTIPOPRODUTO(4)
            MOVE "Cosmeticos"                    TO TBTIPOPRODUTO(5)
            MOVE "Esportivo "                    TO TBTIPOPRODUTO(6)
            MOVE "Geeek"                         TO TBTIPOPRODUTO(7)
            MOVE "Escolar"                       TO TBTIPOPRODUTO(8)
            MOVE "Outros"                        TO TBTIPOPRODUTO(9).
      *-------------ABERTURA DO ARQUIVO -------------------
       R0.    OPEN I-O ARQPRO
           IF ST-ERRO NOT = "00"
            IF ST-ERRO = "30"
              OPEN OUTPUT ARQPRO
              MOVE "CRIANDO ARQUIVO CADASTRO DE Produto " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQPRO
              GO TO INICIO
            ELSE
              IF ST-ERRO = "95"
                MOVE "ISAM NAO CARREGADO " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
              ELSE
                MOVE "ERRO NA ABERTURA DO ARQUIVO CDAMIGOS" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       R0A.
           OPEN INPUT ARQFOR
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

      *------------- INICIALIZACAO DAS VARIAVEIS------------
       R1.
           MOVE SPACES TO PRO-DESCRICAO PRO-UNIDADE DPRO-TIPOPRODUTO
           DPRO-UNIDADE

           MOVE ZEROS TO PRO-CODIGO PRO-TIPOPRODUTO PRO-PRECO
           PRO-DATAULTIMA PRO-QTDESTOQUE PRO-CPFCNPJ

           MOVE ZEROS TO FOR-CPFCNPJ FOR-TELEFONE FOR-CEP FOR-CODIGO

           MOVE SPACES TO FOR-TIPOFORNECEDOR FOR-NOME FOR-APELIDO
           FOR-LOGRADOURO
           FOR-NUMERO FOR-COMPLEMENTO FOR-BAIRRO FOR-CIDADE
           FOR-ESTADO FOR-EMAIL FOR-CONTATO.
      *-------------VISUALIZACAO DA TELA -------------------------------

           DISPLAY TELAPRINCIPAL.
      *----------ENTRADA DE DADOS---------------------------------------

       R2.
           ACCEPT TPRO-CODIGO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                  CLOSE ARQFOR
                  STOP RUN.
           IF PRO-CODIGO = SPACES
              MOVE "*** CODIGO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.
       LER-ARQPRO.
           READ ARQPRO
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R5A
                PERFORM R4A
                PERFORM R9A THRU R9B
      *          DISPLAY TELAPRINCIPAL
                MOVE "*** PRODUTO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO PRODUTOS" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** PRODUTO NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.
           ACCEPT TPRO-DESCRICAO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R2.
           IF PRO-DESCRICAO = SPACES
              MOVE "DESCRICAO NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.
       R4.
           DISPLAY TELAUNIDADE
           ACCEPT TPRO-UNIDADE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.
           IF PRO-UNIDADE = SPACES
              MOVE "UNIDADE NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.
       R4A.
           IF PRO-UNIDADE = "kl" OR "KL" OR "Kl" OR "kL"
              MOVE "KILO" TO DPRO-UNIDADE
              MOVE "KL" TO PRO-UNIDADE
           ELSE
             IF PRO-UNIDADE = "PC" OR "pc" OR "Pc" OR "pC"
                MOVE "PECA" TO DPRO-UNIDADE
                MOVE "PC" TO PRO-UNIDADE
             ELSE
           IF PRO-UNIDADE = "LT" OR "lt" OR "Lt" OR "lT"
                MOVE "LITRO" TO DPRO-UNIDADE
                MOVE "LT" TO PRO-UNIDADE
           ELSE
           IF PRO-UNIDADE = "TN" OR "tN" OR "tn" OR "Tn"
                MOVE "TONELADA" TO DPRO-UNIDADE
                MOVE "TN" TO PRO-UNIDADE
             ELSE
              MOVE " Informe uma Opcao Valida" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R4.
           DISPLAY TDPRO-UNIDADE.
           DISPLAY TELALIMPAR.
           DISPLAY TELAPRINCIPAL.

       R5.
           DISPLAY TELATIPOPRODUTO
           ACCEPT TPRO-TIPOPRODUTO.
           IF W-ACT = 01
             GO TO R4
           IF PRO-TIPOPRODUTO = 0 OR > 10
                 MOVE "*** DIGITE APENAS DE 1 ATE 10 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5.
           MOVE TBTIPOPRODUTO(PRO-TIPOPRODUTO) TO DPRO-TIPOPRODUTO
           DISPLAY TDPRO-TIPOPRODUTO.

       R5A.
           MOVE TBTIPOPRODUTO(PRO-TIPOPRODUTO) TO DPRO-TIPOPRODUTO
           DISPLAY TDPRO-TIPOPRODUTO.
           DISPLAY TELALIMPAR.
           DISPLAY TELAPRINCIPAL.

       R6.
           ACCEPT TPRO-PRECO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R5.
           IF PRO-PRECO = SPACES
              MOVE " *** PRECO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
       R7.
           ACCEPT TPRO-DATAULTIMA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                GO TO R6.
           IF PRO-DATAULTIMA = 0
                 MOVE "*** DATA NAO PODE FICAR EM BRANCO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R7.
       R8.
           ACCEPT TPRO-QTDESTOQUE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPRINCIPAL
                   GO TO R7.
           IF PRO-QTDESTOQUE = 0
                 MOVE "*** QTD ESTOQUE NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8.
       R9.
           ACCEPT TPRO-CPFCNPJ
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPRINCIPAL
                   GO TO R7.
           IF PRO-CPFCNPJ = 0
               MOVE "*** CPF/CNPJ FORNECEDOR NÃO INFORMADO  ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8.

       R9A.
           MOVE PRO-CPFCNPJ TO FOR-CPFCNPJ.
           READ ARQFOR
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
               MOVE "*** FORNECEDOR DIGITADO NAO ENCONTRADO ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8
               ELSE
               MOVE "ERRO NA LEITURA ARQUIVO FORNECEDORES" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                MOVE 1 TO IND.
                DISPLAY TELAPRINCIPAL.
       R9B.
               MOVE 1 TO IND.

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
                WRITE REGPRO
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
                DELETE ARQPRO RECORD
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
                REWRITE REGPRO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM

                   GO TO R1.
                   DISPLAY TELALIMPAR.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM.
      *---------------------------------------------------------------
       ROT-FIM.
           CLOSE ARQPRO ARQFOR.
           STOP RUN.
      *--------------------------------------------------------------
      *---------[ ROTINA DE MENSAGEM ]---------------------
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
