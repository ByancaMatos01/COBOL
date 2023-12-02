       IDENTIFICATION DIVISION.
       PROGRAM-ID. AMIG.
      *AUTHOR. ANGELO LOTIERZO FILHO.
      *Co AUTHOR BYANCA MATOS.
      ************************
      * REGISTRO DE AMIGOS   *
      ************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADAMIGO ASSIGN TO DISK
                    ORGANIZATION IS SEQUENTIAL
                    ACCESS MODE  IS SEQUENTIAL
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADAMIGO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADAMIGO.DAT".
       01 REGAMIGO.
          03 APELIDO       PIC X(12).
          03 NOME          PIC X(30).
          03 ENDERECO      PIC X(50).
          03 CEP           PIC X(10).
          03 TELEFONE      PIC X(15).
          03 EMAIL         PIC X(30).
          03 SEXO          PIC X(01).
          03 GENERO        PIC X(01).
          03 TPAMIGO       PIC 9(01).
	  
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-OPCAO       PIC X(01)  VALUE SPACES.
       01 W-ACT         PIC 9(02)  VALUE ZEROS.
       01 ST-ERRO       PIC X(02)  VALUE "00".
       01 DSEXO         PIC X(12)  VALUE SPACES.
       01 DGENERO       PIC X(12)  VALUE SPACES.
       01 DTPAMIGO      PIC X(12)  VALUE SPACES.
       01 W-CONT        PIC 9(06)  VALUE ZEROS.
       01 MENS          PIC X(50)  VALUE SPACES.
       01 RESPOSTA       PIC X(01) VALUE SPACES.

      *
       SCREEN SECTION.
       01  TELAX02.
           05  LINE 02  COLUMN 01
               VALUE  "                        *** CADASTRO DE".
           05  LINE 02  COLUMN 41
               VALUE  "AMIGOS ***".
           05  LINE 05  COLUMN 01
               VALUE  "APELIDO     :                       NOME".
           05  LINE 05  COLUMN 41
               VALUE  " :".
           05  LINE 07  COLUMN 01
               VALUE  "ENDERECO    :".
           05  LINE 09  COLUMN 01 
               VALUE  "CEP         :".
           05  LINE 11  COLUMN 01
               VALUE  "TELEFONE    :".
           05  LINE 13  COLUMN 01
               VALUE  "EMAIL       :".
           05  LINE 15  COLUMN 01
               VALUE  "SEXO        :".
           05  LINE 17  COLUMN 01
               VALUE  "GENERO      :".
           05  LINE 19  COLUMN 01
               VALUE  "TIPO AMIGO  :".
           05  LINE 21  COLUMN 01
               VALUE  "MENSAGEM    :".
           05  LINE 23  COLUMN 01
               VALUE  "Deseja cadastrar? (S/N):".
           05  TAPELIDO
               LINE 05  COLUMN 14  PIC X(12)
               USING  APELIDO.
           05  TNOME
               LINE 05  COLUMN 44  PIC X(30)
               USING  NOME.
           05  TENDERECO
               LINE 07  COLUMN 14  PIC X(50)
               USING  ENDERECO.
           05  TCEP
               LINE 09 COLUMN 14 PIC X(10)
               USING CEP.
           05  TTELEFONE
               LINE 11  COLUMN 14  PIC X(15)
               USING  TELEFONE.
           05  TEMAIL
               LINE 13  COLUMN 14  PIC X(30)
               USING  EMAIL.
           05  TSEXO
               LINE 15  COLUMN 14  PIC X(01)
               USING  SEXO.
           05  TDSEXO
               LINE 15  COLUMN 16  PIC X(13)
               USING  DSEXO.
           05  TGENERO
               LINE 17  COLUMN 14  PIC X(01)
               USING  GENERO.
           05  TDGENERO
               LINE 17  COLUMN 16  PIC X(12)
               USING  DGENERO.
           05  TTPAMIGO
               LINE 19  COLUMN 14  PIC 9(01)
               USING  TPAMIGO.
           05  TDTPAMIGO
               LINE 19  COLUMN 16  PIC X(12)
               USING  DTPAMIGO.
          05   TRESPOSTA
               LINE 23  COLUMN 29  PIC X(01)
               USING  RESPOSTA.

      *
       01  TELAX02A.
           05  LINE 15  COLUMN 41
               VALUE  "        0-FACULDADE".
           05  LINE 16  COLUMN 41
               VALUE  "        1-IGREJA".
           05  LINE 17  COLUMN 41
               VALUE  "        2-BAIRRO".
           05  LINE 18  COLUMN 41
               VALUE  "        3-ACADEMIA".
           05  LINE 19  COLUMN 41
               VALUE  "        4-BALADA".
           05  LINE 20  COLUMN 41
               VALUE  "        5-SERVICO".
           05  LINE 21  COLUMN 41
               VALUE  "        6-CLUBE".
           05  LINE 22  COLUMN 41
               VALUE  "        7-FAMILIA".
           05  LINE 23  COLUMN 41
               VALUE  "        8-EX".
           05  LINE 24  COLUMN 41
               VALUE  "        9-INIMIGO".
       01  TELAX02B.
           05  LINE 15  COLUMN 41
               VALUE  "        H-HOMEM CIS".
           05  LINE 16  COLUMN 41
               VALUE  "        M-MULHER CIS".
           05  LINE 17  COLUMN 41
               VALUE  "        T-TRANSGENERO".
           05  LINE 18  COLUMN 41
               VALUE  "        P-PANGENERO".
           05  LINE 19  COLUMN 41
               VALUE  "        A-AGENERO".
           05  LINE 20  COLUMN 41
               VALUE  "        N-NaO-BINARIO".
           05  LINE 21  COLUMN 41
               VALUE  "        O-OUTROS".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *-------------ABERTURA DO ARQUIVO -------------------
           OPEN EXTEND CADAMIGO
           IF ST-ERRO = "30"
              OPEN OUTPUT CADAMIGO
              CLOSE CADAMIGO
              GO TO INICIO.
      *------------- INICIALIZACAO DAS VARIAVEIS------------
       R1.
           MOVE SPACES TO APELIDO NOME ENDERECO
           TELEFONE EMAIL SEXO GENERO
                DSEXO DGENERO DTPAMIGO
           MOVE ZEROS TO TPAMIGO.
      *-------------VISUALIZACAO DA TELA ------------------
       RTELA.
           DISPLAY TELAX02.
           ACCEPT RESPOSTA.
           IF RESPOSTA NOT EQUAL "S" AND RESPOSTA NOT EQUAL "s"
              GO TO FIM-ROT-TEMPO.

      *----------ENTRADA DE DADOS---------------
      
       R2.
           ACCEPT TAPELIDO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                  CLOSE CADAMIGO
                  STOP RUN.
           IF APELIDO = SPACES
              MOVE "*** APELIDO NAO PODE FICAR EM BRANCO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R2.
       R3.
           ACCEPT TNOME.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R2.
           IF NOME = SPACES
              GO TO R3.
       R4.
           ACCEPT TENDERECO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R3.
           IF ENDERECO = SPACES
              GO TO R4.
       R5.
            ACCEPT TCEP
            ACCEPT W-ACT FROM ESCAPE KEY
            IF W-ACT = 01
               GO TO R4.
            IF CEP = SPACES
              GO TO R5.

       R6.
           ACCEPT TTELEFONE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R5.
           IF TELEFONE = SPACES
              GO TO R6.
       R7.
           ACCEPT TEMAIL.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R6.
           IF EMAIL = SPACES
              GO TO R7.

       R8.
           ACCEPT TSEXO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
           IF SEXO = "M" OR "m"
              MOVE "MASCULINO" TO DSEXO
              MOVE "M" TO SEXO
           ELSE
             IF SEXO = "F" OR "f"
                MOVE "FEMININO" TO DSEXO
                MOVE "F" TO SEXO
             ELSE
                DISPLAY (25, 15) "DIGITE M=MASCULINO   F=FEMENINO"
                GO TO R8.
           DISPLAY TDSEXO.
       R9.
           DISPLAY TELAX02B
           ACCEPT TGENERO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R9.
           IF GENERO = SPACES
              GO TO R10.
           DISPLAY TELAX02.
       R10.
           DISPLAY TELAX02A
           ACCEPT TTPAMIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R8.
           IF TPAMIGO = SPACES
              GO TO R9.
           IF TPAMIGO = 0
              MOVE "FACULDADE" TO DTPAMIGO
           ELSE
              IF TPAMIGO = 1
                MOVE "IGREJA"  TO DTPAMIGO.
           DISPLAY TELAX02.
     
           STOP "   ".
       INC-WR1.
           WRITE REGAMIGO.
           GO TO R1.
       ROT-FIM.
           CLOSE CADAMIGO.
      *--------------------------------------------------------------
      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
