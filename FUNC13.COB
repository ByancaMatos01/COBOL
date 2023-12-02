       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUNC13.
      *> AUTHOR. BYANCA MATOS.

      *>*******************************************************
      *> CADASTRO DE FUNCIONARIOS                            *
      *>*******************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADFUNC ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CODIGO
               ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
               FILE STATUS IS ST-ERRO.

       DATA DIVISION.
       FILE SECTION.
       FD CADFUNC
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "CADFUNC.DAT".
       01 REGFUNC.
           03 CODIGO PIC 9(06).
           03 NOME PIC X(30).
           03 EMAIL PIC X(30).
           03 SEXO PIC X(01).
           03 DATA-ADMISSAO PIC 9(08).
           03 TIPO-SALARIO PIC X(01).
           03 SALARIO-BASE PIC 9(06)V99.
           03 DEPARTAMENTO PIC 9(02).
           03 CARGO PIC 9(02).

       WORKING-STORAGE SECTION.
       77 W-CONT           PIC 9(06) VALUE ZEROS.
       77 W-ACT            PIC 9(02) VALUE ZEROS.
       77 W-OPCAO          PIC X(01) VALUE SPACES.
       77 MENS             PIC X(50) VALUE SPACES.
       77 LIMPA            PIC X(50) VALUE SPACES.
       01 ST-ERRO          PIC X(02) VALUE "00".
       01 W-SEL            PIC 9(01) VALUE ZEROS.
       01 DSEXO            PIC X(12) VALUE SPACES.
       01 DDEPARTAMENTO    PIC X(12) VALUE SPACES.
       01 DCARGO            PIC X(23) VALUE SPACES.
       01 DTIPO-SALARIO    PIC X(20) VALUE SPACES.
       01 TXTTIPO-SALARIO  PIC X(10) VALUE SPACES.
       01 TXTSEXO          PIC X(12) VALUE SPACES.
       01 TXTDEPARTAMENTO  PIC X(30) VALUE SPACES.
       01 TXTCARGO         PIC X(40) VALUE SPACES.

       01 TABDEPARTAMENTOX.
           03 FILLER PIC X(20) VALUE "CONTABILIDADE".
           03 FILLER PIC X(20) VALUE "RECURSOS HUMANOS".
           03 FILLER PIC X(20) VALUE "FINANCEIRO".
           03 FILLER PIC X(20) VALUE "OUTROS".

       01 TABDEPARTAMENTO REDEFINES TABDEPARTAMENTOX.

           03 TBDEPARTAMENTO PIC X(20) OCCURS 10 TIMES.
       01 TABCARGOX.
           03 FILLER PIC X(20) VALUE "ANALISTA DE SISTEMAS".
           03 FILLER PIC X(20) VALUE "CONTADOR".
           03 FILLER PIC X(20) VALUE "GERENTE FINANCEIRO".
           03 FILLER PIC X(20) VALUE "OUTROS".

       01 TABCARGO REDEFINES TABCARGOX.
           03 TBCARGO PIC X(20) OCCURS 10 TIMES.

       SCREEN SECTION.

       01  TELAFUNC.
            05  LINE 02  COLUMN 01
               VALUE  "                           CADASTRO DE F".
           05  LINE 02  COLUMN 41
               VALUE  "UNCIONARIOS".
           05  LINE 05  COLUMN 01
               VALUE  "   CODIGO       :                    NOM".
           05  LINE 05  COLUMN 41
               VALUE  "E:".
           05  LINE 08  COLUMN 01
               VALUE  "   EMAIL        :".
           05  LINE 11  COLUMN 01
               VALUE  "   SEXO         :  -                  DA".
           05  LINE 11  COLUMN 41
               VALUE  "TA ADMISSAO :".
           05  LINE 14  COLUMN 01
               VALUE  "   TIPO SALARIO :  -                  SA".
           05  LINE 14  COLUMN 41
               VALUE  "LARIO BASE  :".
           05  LINE 17  COLUMN 01
               VALUE  "   DEPARTAMENTO :   -".
           05  LINE 20  COLUMN 01
               VALUE  "   CARGO        :   -".
           05  TCODIGO
               LINE 05  COLUMN 19  PIC 9(06)
               USING  CODIGO.
           05  TNOME
               LINE 05  COLUMN 44  PIC X(30)
               USING  NOME.
           05  TEMAIL
               LINE 08  COLUMN 19  PIC X(30)
               USING  EMAIL.
           05  TSEXO
               LINE 11  COLUMN 19  PIC X(01)
               USING  SEXO.
           05  TDSEXO
               LINE 11  COLUMN 21  PIC X(15)
               USING  DSEXO.
           05  TDATA-ADMISSAO
               LINE 11  COLUMN 55  PIC XX/XX/XX
               USING  DATA-ADMISSAO.
           05  TTIPO-SALARIO
               LINE 14  COLUMN 19  PIC X(01)
               USING  TIPO-SALARIO.
           05  TDTIPO-SALARIO
               LINE 14  COLUMN 21  PIC X(15)
               USING  DTIPO-SALARIO.
           05  TTSALARIO-BASE
               LINE 14  COLUMN 55  PIC 9(06)v99
               USING  SALARIO-BASE.
           05  TDEPARTAMENTO
               LINE 17  COLUMN 19  PIC 9(02)
               USING  DEPARTAMENTO.
           05  TDDEPARTAMENTO
               LINE 17  COLUMN 22  PIC X(23)
               USING  DDEPARTAMENTO.
           05  TCARGO
               LINE 20  COLUMN 19  PIC 9(02)
               USING  CARGO.
           05  TDCARGO
               LINE 20  COLUMN 22  PIC X(23)
               USING  DCARGO.
       01 TELATA.
           05 LINE 17 COLUMN 45 VALUE "01-CONTABILIDADE".
           05 LINE 18 COLUMN 45 VALUE "02-RECURSOS HUMANOS".
           05 LINE 19 COLUMN 45 VALUE "03-FINANCEIRO".
           05 LINE 20 COLUMN 45 VALUE "04-OUTROS".

       01 TELACARGO.
           05 LINE 17 COLUMN 43 VALUE "01-ANALISTA DE SISTEMAS".
           05 LINE 18 COLUMN 43 VALUE "02-CONTADOR".
           05 LINE 19 COLUMN 43 VALUE "03-GERENTE FINANCEIRO".
           05 LINE 20 COLUMN 43 VALUE "04- OUTROS".

       PROCEDURE DIVISION.
       R0.
           OPEN I-O CADFUNC
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                   OPEN OUTPUT CADFUNC
                   CLOSE CADFUNC
                   MOVE "*** ARQUIVO CADFUNC FOI CRIADO **" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R0
               ELSE
                   MOVE "ERRO NA ABERTURA DO ARQUIVO CADFUNC" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-MENS-FIM
           ELSE
               NEXT SENTENCE.
       R1.
           MOVE SPACES TO  NOME EMAIL SEXO TIPO-SALARIO
           MOVE SPACES TO TXTTIPO-SALARIO TXTSEXO
               TXTDEPARTAMENTO TXTCARGO
           MOVE ZEROS TO W-SEL CODIGO DATA-ADMISSAO
           SALARIO-BASE DEPARTAMENTO  CARGO.
           DISPLAY TELAFUNC.
       R2.
           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO ROT-MENS-FIM.
       LER-CADFUNC.
           READ CADFUNC
           IF ST-ERRO NOT = "23"
               IF ST-ERRO = "00"
                   PERFORM R5A
                   PERFORM R6A
                   PERFORM R7A
                   DISPLAY TELAFUNC
                   MOVE "*** FUNCIONARIO JA CADASTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ACE-001
               ELSE
                   MOVE "ERRO NA LEITURA ARQUIVO CADFUNC" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-MENS-FIM
           ELSE
               MOVE "*** FUNCIONARIO NAO CADASTRADO ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
               GO TO R2.
       R4.
           ACCEPT TEMAIL.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
               GO TO R3.

       R5.

           ACCEPT TSEXO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R4.
       R5A.
           IF SEXO = "M" OR "m"
              MOVE "MASCULINO" TO DSEXO
              MOVE "M" TO SEXO
           ELSE
             IF SEXO = "F" OR "f"
                MOVE "FEMININO" TO DSEXO
                MOVE "F" TO SEXO
             ELSE
                MOVE " M -Masculino : F - Feminino" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R5.
           DISPLAY TDSEXO.


       R6.
           ACCEPT TDATA-ADMISSAO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
               GO TO R5.
       R6A.
           MOVE DATA-ADMISSAO TO W-CONT
           MOVE W-CONT TO W-ACT
           ACCEPT W-CONT
           IF W-CONT NOT NUMERIC
               MOVE "DATA INVALIDA, REDIGITE" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO R6.

       R7.

           ACCEPT TTIPO-SALARIO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
              GO TO R6.
       R7A.
           IF TIPO-SALARIO = "H" OR "h"
              MOVE "HORISTA" TO DTIPO-SALARIO
              MOVE "H" TO TIPO-SALARIO
           ELSE
             IF TIPO-SALARIO = "M" OR "m"
                MOVE "MENSALISTA" TO DTIPO-SALARIO
                MOVE "M" TO TIPO-SALARIO
             ELSE
            IF TIPO-SALARIO = "D" OR "d"
                MOVE "DIARISTA" TO DTIPO-SALARIO
                MOVE "D" TO TIPO-SALARIO
             ELSE
              MOVE " H -HORISTA : M - MENSALISTA : D - DIARISTA" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R7.
           DISPLAY TDTIPO-SALARIO.
       R8.
           ACCEPT TTSALARIO-BASE.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
               GO TO R7.
       R10.
           DISPLAY TELATA
           ACCEPT TDEPARTAMENTO.
           IF DEPARTAMENTO = 0
                 MOVE "*** DIGITE APENAS DE 1 ATE 10 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R10.
           MOVE TBDEPARTAMENTO(DEPARTAMENTO) TO DDEPARTAMENTO
           DISPLAY TDDEPARTAMENTO.

       R10A.
           MOVE TBDEPARTAMENTO(DEPARTAMENTO) TO DDEPARTAMENTO
           DISPLAY TDDEPARTAMENTO.
           DISPLAY TELAFUNC.

       R11.
           DISPLAY TELACARGO
           ACCEPT TCARGO.
           IF CARGO = 0
                 MOVE "*** DIGITE APENAS DE 1 ATE 10 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R11.
           MOVE TBCARGO(CARGO) TO DCARGO
           DISPLAY TDCARGO.

       R11A.
           MOVE TBCARGO(CARGO) TO DCARGO
           DISPLAY TDCARGO
           DISPLAY TELAFUNC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGFUNC
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

      *>****************************************
      *> ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *>****************************************
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N"  AND W-OPCAO NOT = "A"
                    AND W-OPCAO NOT = "E"  GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R0
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADFUNC RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO AMIGO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R3.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGFUNC
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *>-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADFUNC.
           STOP RUN.

      *>---------[ ROTINA DE MENSAGEM ]---------------------
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

      *>    FILE STATUS
      *>    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *>    22 = REGISTRO JÁ CADASTRADO
      *>    23 = REGISTRO NÃO ENCONTRADO
      *>    30 = ARQUIVO NÃO ENCONTRADO
