       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCE001.
      * AUTHOR. Byanca Matos.
      ********************************
      * MANUTENCAO DO CADASTRO CEP   *
      ********************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP-NUMCEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CEP-LOGRADOURO
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
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
       77 W-SEL          PIC 9(01) VALUE ZEROS.
       77 W-CONT         PIC 9(06) VALUE ZEROS.
       77 W-OPCAO        PIC X(01) VALUE SPACES.
       77 ST-ERRO        PIC X(02) VALUE "00".
       77 W-ACT          PIC 9(02) VALUE ZEROS.
       77 MENS           PIC X(50) VALUE SPACES.
       77 LIMPA          PIC X(50) VALUE SPACES.
      *
       01 TXUF.
            03 UF-SIGLA     PIC X(02) VALUE SPACES.
            03 UF-COMP   PIC X(20) VALUE SPACES.
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
      *
       01 TBUFR REDEFINES TBUF.
             03 TABUF    PIC X(22) OCCURS 27 TIMES.
       01 IND            PIC 9(02) VALUE ZEROS.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  SMT005.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 01
               VALUE  "                               CADASTRO".
           05  LINE 03  COLUMN 41
               VALUE  "DE CEP".
           05  LINE 08  COLUMN 01
               VALUE  "        CEP :".
           05  LINE 10  COLUMN 01
               VALUE  " LOGRADOURO :".
           05  LINE 12  COLUMN 01
               VALUE  "     BAIRRO :".
           05  LINE 14  COLUMN 01
               VALUE  "     CIDADE :".
           05  LINE 16  COLUMN 01
               VALUE  "         UF :".
           05  LINE 23  COLUMN 01
               VALUE  " MENSAGEM :".
           05  TCEP-NUMCEP
               LINE 08  COLUMN 15  PIC 99999.999
               USING  CEP-NUMCEP.
           05  TCEP-LOGRA
               LINE 10  COLUMN 15  PIC X(35)
               USING  CEP-LOGRADOURO.
           05  TCEP-BAIRRO
               LINE 12  COLUMN 15  PIC X(20)
               USING  CEP-BAIRRO.
           05  TCEP-CIDADE
               LINE 14  COLUMN 15  PIC X(20)
               USING  CEP-CIDADE.
           05  TCEP-UF
               LINE 16  COLUMN 15  PIC X(02)
               USING  CEP-UF.
           05  TUF-TEXTO
               LINE 16  COLUMN 18  PIC X(20)
               USING  UF-COMP.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O ARQCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT ARQCEP
                      CLOSE ARQCEP
                      MOVE "* ARQUIVO CEP SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE SPACES TO TXUF CEP-UF CEP-LOGRADOURO CEP-BAIRRO.
                MOVE SPACES TO CEP-CIDADE.
                MOVE ZEROS  TO CEP-NUMCEP.
                DISPLAY SMT005.
       INC-002.
                ACCEPT TCEP-NUMCEP
                ACCEPT W-ACT FROM ESCAPE KEY

                IF W-ACT = 01
                   CLOSE ARQCEP
                   STOP RUN.
                IF CEP-NUMCEP = SPACES
                   MOVE "*** CEP INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-CEP01.
                MOVE 0 TO W-SEL
                READ ARQCEP
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM INC-006A THRU INC-006B
                      DISPLAY SMT005
                      MOVE "*** CEP JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ. CEP"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TCEP-LOGRA
                ACCEPT W-ACT FROM ESCAPE KEY

                IF W-ACT = 02 GO TO INC-002.
                IF CEP-LOGRADOURO = SPACES
                   MOVE "LOGRA. DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.

       INC-004.
                ACCEPT TCEP-BAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF CEP-BAIRRO = SPACES
                   MOVE "BAIRRO DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004.
       INC-005.
                ACCEPT TCEP-CIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF CEP-CIDADE = SPACES
                   MOVE "CIDADE DEVER DIFERENTE DE BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-005.

       INC-006.
                ACCEPT TCEP-UF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO INC-005.
       INC-006A.
               MOVE 1 TO IND.
       INC-006B.
               MOVE TABUF (IND) TO TXUF
               IF CEP-UF NOT = UF-SIGLA
                      ADD 1 TO IND
                      IF IND > 27
                         MOVE "UF NAO EXISTE" TO MENS
                         PERFORM ROT-MENS THRU ROT-MENS-FIM
                         GO TO INC-006
                      ELSE
                         GO TO INC-006B
               ELSE
                     DISPLAY TUF-TEXTO.
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** NOTA JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE NOTAS"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                 "F1=NOVO REGISTRO  F2=ALTERAR  F3=EXCLUIR F4 SAIR"
                ACCEPT (23, 58) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                  AND W-ACT NOT = 05
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
                 IF W-ACT = 05

                  STOP RUN.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ARQCEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA ALTERACAO DO REGISTRO NOTAS"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
