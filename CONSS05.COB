       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSS05.
      **************************************************
      * CONSULTA DE PEDIDOS *
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT ARQPED ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS PED-COD
                  ALTERNATE RECORD KEY IS PED-COD-PRO WITH DUPLICATES
                 ALTERNATE RECORD KEY IS PED-CLI-CPFCNPJ WITH DUPLICATES
                 ALTERNATE RECORD KEY IS PED-FOR-CPFCNPJ WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
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
       WORKING-STORAGE SECTION.
       01 W-SEL             PIC 9(01) VALUE ZEROS.
       01 W-CONT            PIC 9(04) VALUE ZEROS.
       01 W-OPCAO           PIC X(01) VALUE SPACES.
       01 ST-ERRO           PIC X(02) VALUE "00".
       01 W-ACT             PIC 9(02) VALUE ZEROS.
       01 MENS              PIC X(50) VALUE SPACES.
       01 LIMPA             PIC X(55) VALUE SPACES.
       01 SOLIC             PIC X(20) VALUE SPACES.
       01 CONLIN            PIC 9(03) VALUE 001.
       01 PED-CODENTR       PIC 9(06) VALUE ZEROS.
       01 PED-COD-PROENTR   PIC 9(09) VALUE ZEROS.
      *-----------------------------------------------------------------
      *
      *************************
      * REPORT SECTION *
      *************************
      *
       01 DET2.
          03 FILLER                  PIC X(02) VALUE SPACES.
          03 DTEPED-COD              PIC 9(06) VALUE ZEROS.
          03 FILLER                  PIC X(11) VALUE SPACES.
          03 DTEPED-COD-PRO          PIC 9(09) VALUE ZEROS.
          03 FILLER                  PIC X(13) VALUE SPACES.
          03 DTEPED-VALORTOTAL       PIC 9(09) VALUE ZEROS.


      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.

       01  TELACEP.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01
               VALUE  " CADASTRO DE PEDIDOS - SCE005  ***".
           05  LINE 02  COLUMN 41
               VALUE  "CONSULTA DE PEDIDOS  ***".
           05  LINE 04  COLUMN 01
               VALUE  "  CODIGO PEDIDO    CODIGO PRODUTO    ".
           05  LINE 04  COLUMN 41
               VALUE  " VAL0R TOTAL ".
           05  LINE 23  COLUMN 01
               VALUE  "  MENSAGEM :".
           05  TPED-CODENTR
               LINE 06  COLUMN 03  PIC 9(06)
               USING  PED-CODENTR.
           05  TPED-DESCRICAOENTR
               LINE 06  COLUMN 20  PIC 9(6)
               USING  PED-COD-PROENTR.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  ARQPED
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE ARQPED NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "ERRO ABERTURA DO ARQUIVO ARQPED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2.

      *
       INC-001.
           MOVE ZEROS TO PED-CODENTR
           MOVE ZEROS TO PED-COD-PROENTR
           DISPLAY  TELACEP.
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA F2=CONSULTA P/COD PRO."
           ACCEPT TPED-CODENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQPED
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE PED-CODENTR TO PED-COD
           START ARQPED KEY IS NOT LESS PED-COD INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA   F2=CONSULTA P/CODIGO PRO. "
           ACCEPT TPED-DESCRICAOENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQPED
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001A.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE PED-COD-PROENTR TO PED-COD-PRO
           START ARQPED KEY IS NOT LESS PED-COD-PRO INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ ARQPED NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQPED"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
           MOVE PED-COD TO DTEPED-COD
           MOVE PED-COD-PRO TO DTEPED-COD-PRO
           MOVE PED-VALORTOTAL   TO DTEPED-VALORTOTAL

           COMPUTE LIN = CONLIN + 5
           DISPLAY (LIN, 01) DET2
           ADD 1 TO CONLIN
           IF CONLIN < 17
                  GO TO INC-RD2.
      *
       ROT-SOL.
           DISPLAY (23, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELACEP
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO INC-001.
      *
       NOV-SOL.
           DISPLAY (23, 13) " *** NOVA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  GO TO INC-001.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE ARQPED.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *

      *---------------------*** FIM DE PROGRAMA ***--------------------*
