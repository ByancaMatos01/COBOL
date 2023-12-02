       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE0021.
      *AUTHOR. Byanca Matos.
      ******************************
      * CADASTRO DE FUNCIONARIOS   *
      ******************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQCLI ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CLI-CODIGO
                      ALTERNATE RECORD KEY IS CLI-NOME WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
           SELECT ARQCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP-NUMCEP
                    ALTERNATE RECORD KEY IS CEP-LOGRADOURO
                    WITH DUPLICATES
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQCLI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCLI.DAT".
       01 REGCLI.
          03 CHAVE1.
            05 CLI-CODIGO            PIC 9(11).
          03 CLI-TIPOCLIENTE         PIC X(01).
          03 CLI-CPF                 PIC 9(11).
          03 CLI-CNPJ                PIC 9(14).
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

      *
      *-----------------------------------------------------------------
       FD ARQCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCEP.DAT".
       01 REGCEP.
                03 CEP-NUMCEP             PIC 9(08).
                03 CEP-LOGRADOURO         PIC X(30).
                03 CEP-BAIRRO             PIC X(20).
                03 CEP-CIDADE             PIC X(20).
                03 CEP-UF                 PIC X(02).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-OPCAO                PIC X(01) VALUE SPACES.
       01 W-UPPERCASE            PIC X(01) VALUE SPACES.
       01 W-ACT                  PIC 9(02) VALUE ZEROS.
       01 ST-ERRO                PIC X(02) VALUE "00".
       01 DCLI-ESTADO            PIC X(15) VALUE SPACES.
       01 DCLI-TIPOCLIENTE       PIC X(20) VALUE SPACES.
       01 W-CONT                 PIC 9(06) VALUE ZEROS.
       01 MENS                   PIC X(50) VALUE SPACES.
       01 W-SEL                  PIC 9(01) VALUE ZEROS.
       01 TXUF.
            03 UF-SIGLA          PIC X(02) VALUE SPACES.
            03 UF-COMP           PIC X(20) VALUE SPACES.
      *
       01 TBUF.
             03 FILLER     PIC X(22) VALUE "ACACRE".
             03 FILLER     PIC X(22) VALUE "ALALAGOAS".
             03 FILLER     PIC X(22) VALUE "AMAMAZONAS".
             03 FILLER     PIC X(22) VALUE "APAMAPA".
             03 FILLER     PIC X(22) VALUE "BABAHIA".
             03 FILLER     PIC X(22) VALUE "CECEARA".
             03 FILLER     PIC X(22) VALUE "DFDISTRITO FEDERAL".
             03 FILLER     PIC X(22) VALUE "ESESPIRITO SANTO".
             03 FILLER     PIC X(22) VALUE "GOGOIAS".
             03 FILLER     PIC X(22) VALUE "MAMARANHÃO".
             03 FILLER     PIC X(22) VALUE "MTMATO GROSSO".
             03 FILLER     PIC X(22) VALUE "MSMATO GROSSO DO SUL".
             03 FILLER     PIC X(22) VALUE "MGMINAS GERAIS".
             03 FILLER     PIC X(22) VALUE "PAPARA".
             03 FILLER     PIC X(22) VALUE "PBPARAIBA".
             03 FILLER     PIC X(22) VALUE "PRPARANÁ".
             03 FILLER     PIC X(22) VALUE "PEPERNAMBUCO".
             03 FILLER     PIC X(22) VALUE "PIPIAUI".
             03 FILLER     PIC X(22) VALUE "RJRIO DE JANEIRO".
             03 FILLER     PIC X(22) VALUE "RNR. GRANDE DO NORTE".
             03 FILLER     PIC X(22) VALUE "RSRIO GRANDE DO SUL".
             03 FILLER     PIC X(22) VALUE "RORONDONIA".
             03 FILLER     PIC X(22) VALUE "RRRORAIMA".
             03 FILLER     PIC X(22) VALUE "SCSANTA CATARINA".
             03 FILLER     PIC X(22) VALUE "SPSAO PAULO".
             03 FILLER     PIC X(22) VALUE "SESERGIPE".
             03 FILLER     PIC X(22) VALUE "TOTOCANTINS".
       01 TBUFR REDEFINES TBUF.
             03 TABUF    PIC X(22) OCCURS 27 TIMES.
       01 IND            PIC 9(02) VALUE ZEROS.
      *--------------------------------------------
      *
       SCREEN SECTION.
       01  TELAPRINCIPAL.
            05  LINE 01  COLUMN 01
               VALUE  "                           CADASTRO DE CLIENTE".
           05  LINE 03  COLUMN 01
               VALUE  "             DADOS EMPRESARIAIS".
           05  LINE 04  COLUMN 01
               VALUE  "   Informe o Tipo de Cliente :  -".
           05  LINE 04  COLUMN 41
               VALUE  "              Codigo:                   ".
           05  LINE 05  COLUMN 01
               VALUE  "   CPF/CNPJ                  :".
           05  LINE 06  COLUMN 01
               VALUE  "   Nome/Razao Social         :".
           05  LINE 07  COLUMN 01
               VALUE  "   Apelido                   :".
           05  LINE 08  COLUMN 01
               VALUE  "   ENDERECO".
           05  LINE 09  COLUMN 01
               VALUE  "   CEP    :                           Lo".
           05  LINE 09  COLUMN 41
               VALUE  "gradouro  :                             ".
           05  LINE 10  COLUMN 01
               VALUE  "   Numero :                           Co".
           05  LINE 10  COLUMN 41
               VALUE  "mplemento :                             ".
           05  LINE 11  COLUMN 01
               VALUE  "   Bairro :".
           05  LINE 12  COLUMN 01
               VALUE  "   Cidade :                           Es".
           05  LINE 12  COLUMN 41
               VALUE  "tado      :   -                         ".
           05  LINE 13  COLUMN 01
               VALUE  "             CONTATO".
           05  LINE 14  COLUMN 01
               VALUE  "   Telefone :                         E-".
           05  LINE 14  COLUMN 41
               VALUE  "mail :                                  ".
           05  LINE 15  COLUMN 01
               VALUE  "   CONTATO  :".
           05  LINE 17  COLUMN 01
               VALUE  "  MENSAGENS :".
           05  TCLI-TIPOCLIENTE
               LINE 04  COLUMN 32  PIC X(01)
               USING  CLI-TIPOCLIENTE.
           05  TDTIPOCLIENTE
               LINE 04  COLUMN 34  PIC X(20)
               USING  DCLI-TIPOCLIENTE.
           05  TCLI-CODIGO
               LINE 04  COLUMN 62  PIC 9(11)
               USING  CLI-CODIGO.
           05  TCLI-CPF
               LINE 05  COLUMN 32  PIC 999.999.999.999.99
               USING  CLI-CPF.
           05  TCLI-NOME
               LINE 06  COLUMN 32  PIC X(30)
               USING  CLI-NOME.
           05  TCLI-APELIDO
               LINE 07  COLUMN 32  PIC X(30)
               USING  CLI-APELIDO.
           05  TCLI-CEP
               LINE 09  COLUMN 13  PIC 99999.999
               USING  CLI-CEP.
           05  TCLI-LOGRADOURO
               LINE 09  COLUMN 52  PIC X(24)
               USING  CEP-LOGRADOURO.
           05  TCLI-NUMERO
               LINE 10  COLUMN 13  PIC X(11)
               USING  CLI-NUMERO.
           05  TCLI-COMPLEMENTO
               LINE 10  COLUMN 52  PIC X(24)
               USING  CLI-COMPLEMENTO.
           05  TCLI-BAIRRO
               LINE 11  COLUMN 13  PIC X(40)
               USING  CEP-BAIRRO.
           05  TCLI-CIDADE
               LINE 12  COLUMN 13  PIC X(24)
               USING  CEP-CIDADE.
           05  TCLI-ESTADO
               LINE 12  COLUMN 53  PIC X(02)
               USING  CEP-UF.
           05  TDCLI-ESTADO
               LINE 12  COLUMN 56  PIC X(15)
               USING  DCLI-ESTADO.
           05  TCLI-TELEFONE
               LINE 14  COLUMN 15  PIC 9(11)
               USING  CLI-TELEFONE.
           05  TCLI-EMAIL
               LINE 14  COLUMN 47  PIC X(33)
               USING  CLI-EMAIL.
           05  TCLI-CONTATO
               LINE 15  COLUMN 15  PIC X(32)
               USING  CLI-CONTATO.



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
               VALUE  " ".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

      *-------------ABERTURA DO ARQUIVO -------------------
       R0.
           OPEN I-O ARQCLI
           IF ST-ERRO NOT = "00"


            IF ST-ERRO = "30"
              OPEN OUTPUT ARQCLI
              MOVE "CRIANDO ARQUIVO CADASTRO DE FUNCIONARIOS " TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              CLOSE ARQCLI
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
           OPEN INPUT ARQCEP
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
           MOVE SPACES TO
           DCLI-TIPOCLIENTE DCLI-ESTADO CLI-TIPOCLIENTE
           CLI-NOME CLI-APELIDO CLI-LOGRADOURO CLI-NUMERO
           CLI-COMPLEMENTO CLI-BAIRRO CLI-CIDADE CLI-ESTADO
           CLI-EMAIL CLI-CONTATO.
           MOVE SPACES TO CEP-LOGRADOURO CEP-BAIRRO CEP-CIDADE CEP-UF

           MOVE ZEROS TO CLI-CODIGO CLI-CPF CLI-CNPJ CLI-CEP
           CLI-TELEFONE
      *-------------VISUALIZACAO DA TELA ------------------

           DISPLAY TELAPRINCIPAL.
      *----------ENTRADA DE DADOS---------------
       R2.
           ACCEPT TCLI-TIPOCLIENTE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              CLOSE ARQCLI
              STOP RUN.
       R2A.
           IF CLI-TIPOCLIENTE = "f" OR "F"
              MOVE "Pessoa Fisica" TO DCLI-TIPOCLIENTE
              MOVE "F" TO CLI-TIPOCLIENTE
           ELSE
            IF CLI-TIPOCLIENTE = "J" OR "j"
              MOVE "Pessoa Juridica" TO DCLI-TIPOCLIENTE
              MOVE "J" TO CLI-TIPOCLIENTE

             ELSE
              MOVE "P - Pessoa Fisica : J - Pessoa Juridica" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R2.
           DISPLAY TDTIPOCLIENTE.
           DISPLAY TELALIMPAR.
           DISPLAY TELAPRINCIPAL.
       R3.
           ACCEPT TCLI-CODIGO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                  CLOSE ARQCLI
                  STOP RUN.
           IF CLI-CODIGO = SPACES
              MOVE "*** CODIGO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R3.
       LER-ARQCLI.
           READ ARQCLI
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R7A THRU R7B
                DISPLAY TELAPRINCIPAL
                MOVE "*** CLIENTE JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO Cliente" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CLIENTE NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R4.
           ACCEPT TCLI-CPF.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.
           IF CLI-CPF = SPACES
              MOVE "CPF NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R4.
       R5.
           ACCEPT TCLI-NOME.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R4.
           IF CLI-NOME = SPACES
              MOVE "NOME NAO PODE FICAR EM BRANCO" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R5.
       R6.
           ACCEPT TCLI-APELIDO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R5.
           IF CLI-APELIDO = SPACES
              MOVE " *** APELIDO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R6.
       R7.
           ACCEPT TCLI-CEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAPRINCIPAL
                   GO TO R6.
           IF CLI-CEP = 0
                 MOVE "*** CEP NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8.
       R7A.
           MOVE CLI-CEP TO CEP-NUMCEP.
           READ ARQCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** CEP DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                MOVE 1 TO IND.
                DISPLAY TELAPRINCIPAL.
       R7B.
               MOVE 1 TO IND.
       R7C.
               MOVE TABUF (IND) TO TXUF
               IF CEP-UF NOT = UF-SIGLA
                      ADD 1 TO IND
                      IF IND > 27
                         MOVE "UF NAO EXISTE" TO MENS
                         PERFORM ROT-MENS THRU ROT-MENS-FIM
                         GO TO R7
                      ELSE
                         GO TO R7C
               ELSE
                     DISPLAY TXUF.
       R8.
           ACCEPT TCLI-NUMERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R7.
           IF CLI-NUMERO = SPACES
              MOVE " *** NUMERO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R8.
       R9.
           ACCEPT TCLI-COMPLEMENTO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R8.
           IF CLI-COMPLEMENTO = SPACES
            MOVE " *** COMPLEMENTO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R9.

       R10.
           ACCEPT TCLI-TELEFONE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R9.
           IF CLI-TELEFONE = SPACES
            MOVE " *** TELEFONE NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R10.
       R11.
           ACCEPT TCLI-EMAIL.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R10.
           IF CLI-EMAIL = SPACES
            MOVE " *** TELEFONE NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R11.
       R12.
           ACCEPT TCLI-CONTATO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R11.
           IF CLI-CONTATO = SPACES
            MOVE " *** CONTATO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R12.

            IF W-SEL = 1
            GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (17, 30) "Deseja Salvar (S/N) : ".
                ACCEPT (17, 55) W-OPCAO WITH UPDATE
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
                WRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* AMIGO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
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
                IF W-OPCAO = "N"
                   GO TO R1
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3
                ELSE
                   IF W-OPCAO = "S"
                      MOVE 1 TO W-SEL
                      GO TO ROT-FIM.
      *
       EXC-OPC.
                DISPLAY (17, 30) "Deseja EXCLUIR   (S/N) : ".
                ACCEPT (17, 55) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQCLI RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO AMIGO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

       ALT-OPC.
                DISPLAY (17, 30) "Deseja ALTERAR  (S/N) : ".
                ACCEPT (17, 55) W-OPCAO
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
                REWRITE REGCLI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM

                   GO TO R1.
                   DISPLAY TELALIMPAR.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.

      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE ARQCLI ARQCEP.
           STOP RUN.
      *--------------------------------------------------------------
      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (17, 21) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (17, 21) MENS.
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
