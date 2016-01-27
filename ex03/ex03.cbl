       IDENTIFICATION DIVISION.
       PROGRAM-ID.       EX03.
       AUTHOR.           ANDRE LUIZ.
       INSTALLATION.     FATEC-PC.
       DATE-WRITTEN.     02/12/2015.
       DATE-COMPILED.
       SECURITY.         APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.          REALIZA MERGE ENTRE OS ARQUIVOS DE ENTRADA E.
                         GERA UM ARQUIVO DE SAIDA E DOIS RELATORIOS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.   ANDRE-PC.
       OBJECT-COMPUTER.   FATEC-PC.
       SPECIAL-NAMES.     DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT CADMERC ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT MOVMERC ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT ATUMERC ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT RELERRO ASSIGN TO DISK.

       SELECT RELMERC ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.

       FD CADMERC
       LABEL RECORD ARE STANDARD
       VALUE OF FILE-ID IS "CADMERC.DAT".

       01 REG-CADMERC.
              02 CODIGO-CAD      PIC 9(3).
              02 DESCRICAO-CAD   PIC X(30).
              02 ESTOQUE-MIN-CAD PIC 9(3).
              02 QNT-ESTOQUE-CAD PIC 9(3).
              02 PRECO-UNIT-CAD  PIC 9(4)V99.

       FD MOVMERC
       LABEL RECORD ARE STANDARD
       VALUE OF FILE-ID IS "MOVMERC.DAT".

       01 REG-MOVMERC.
              02 CODIGO-MOV       PIC 9(3).
              02 DESCRICAO-MOV    PIC X(30).
              02 ESTOQUE-MIN-MOV  PIC 9(3).
              02 QNT-ESTOQUE-MOV  PIC 9(3).
              02 PRECO-UNIT-MOV   PIC 9(4)V99.
              02 TIPO-ATUALIZACAO PIC X.
                     88 INCLUSAO VALUE '1'.
                     88 ALTERACAO VALUE '2'.
                     88 EXCLUSAO VALUE '3'.

       FD ATUMERC
       LABEL RECORD ARE STANDARD
       VALUE OF FILE-ID IS "ATUMERC.DAT".

       01 REG-ATUMERC.
              02 CODIGO-ATU      PIC 9(3).
              02 DESCRICAO-ATU   PIC X(30).
              02 ESTOQUE-MIN-ATU PIC 9(3).
              02 QNT-ESTOQUE-ATU PIC 9(3).
              02 PRECO-UNIT-ATU  PIC 9(4)V99.

       FD RELMERC
       LABEL RECORD ARE OMITTED.

       01 REG-RELMERC PIC X(80).

       FD RELERRO
       LABEL RECORD ARE OMITTED.

       01 REG-RELERRO PIC X(80).

       WORKING-STORAGE SECTION.
       77 CH-CAD PIC X(3) VALUE SPACES.
       77 CH-MOV PIC X(3) VALUE SPACES.
       77 CT-PAG PIC 999 VALUE ZEROES.
       77 CT-LIN PIC 99  VALUE 41.
       77 CT-PAG-ERR PIC 999 VALUE ZEROES.
       77 CT-LIN-ERR PIC 99  VALUE 41.

       01 CAB-1-ERR.
              02 FILLER PIC X(25) VALUE SPACES.
              02 FILLER PIC X(22) VALUE
              "RELAÇÃO DE MERCADORIAS".
              02 FILLER PIC X(24) VALUE SPACES.
              02 FILLER PIC X(5) VALUE "PÁG. ".
              02 VAR-PAG-ERR PIC ZZ9.
              02 FILLER PIC X VALUE SPACES.

       01 CAB-2-ERR.
              02 FILLER PIC X(26) VALUE SPACES.
              02 FILLER PIC X(20) VALUE
              "ERROS DE ATUALIZAÇÃO".
              02 FILLER PIC X(34) VALUE SPACES.

       01 CAB-3-ERR.
              02 FILLER PIC XXX VALUE SPACES.
              02 FILLER PIC X(6) VALUE "CODIGO".
              02 FILLER PIC X(6) VALUE SPACES.
              02 FILLER PIC X(9) VALUE "DESCRIÇÃO".
              02 FILLER PIC X(26) VALUE SPACES.
              02 FILLER PIC X(8) VALUE "MENSAGEM".
              02 FILLER PIC X(22) VALUE SPACES.

       01 CAB-DETALHE-ERR.
              02 FILLER PIC X(4) VALUE SPACES.
              02 VAR-CODIGO-ERR PIC 999.
              02 FILLER PIC X(6) VALUE SPACES.
              02 VAR-DESCRICAO-ERR PIC X(30).
              02 FILLER PIC X(6) VALUE SPACES.
              02 VAR-MENSAGEM PIC X(30).
              02 FILLER PIC X VALUE SPACES.

       01 CAB-1.
              02 FILLER PIC X(25) VALUE SPACES.
              02 FILLER PIC X(22) VALUE
              "RELAÇÃO DE MERCADORIAS".
              02 FILLER PIC X(24) VALUE SPACES.
              02 FILLER PIC X(5) VALUE "PÁG. ".
              02 VAR-PAG PIC Z99.

       01 CAB-2.
              02 FILLER PIC X(28) VALUE SPACES.
              02 FILLER PIC X(15) VALUE
              "ESTOQUE CRÍTICO".
              02 FILLER PIC X(37) VALUE SPACES.

       01 CAB-3.
              02 FILLER PIC X VALUE SPACES.
              02 FILLER PIC X(6) VALUE "CODIGO".
              02 FILLER PIC XX VALUE SPACES.
              02 FILLER PIC X(9) VALUE "DESCRIÇÃO".
              02 FILLER PIC X(20) VALUE SPACES.
              02 FILLER PIC X(12) VALUE
              "ESTOQUE MIN.".
              02 FILLER PIC XXX VALUE SPACES.
              02 FILLER PIC X(11) VALUE
              "QNT ESTOQUE".
              02 FILLER PIC X(5) VALUE SPACES.
              02 FILLER PIC X(5) VALUE "PREÇO".
              02 FILLER PIC X(6) VALUE SPACES.

       01 CAB-DETALHE.
              02 FILLER PIC XX VALUE SPACES.
              02 VAR-CODIGO PIC 999.
              02 FILLER PIC XX VALUE SPACES.
              02 VAR-DESCRICAO PIC X(30).
              02 FILLER PIC X(5) VALUE SPACES.
              02 VAR-ESTOQUE-MIN PIC 999.
              02 FILLER PIC X(11) VALUE SPACES.
              02 VAR-QNT-ESTOQUE PIC 999.
              02 FILLER PIC X(9) VALUE SPACES.
              02 VAR-PRECO-UNIT PIC ZZ99,99.

       PROCEDURE DIVISION.

       MAIN.
              PERFORM INICIO.
              PERFORM PRINCIPAL
                      UNTIL CH-CAD EQUALS CH-MOV 
                      AND CH-MOV EQUALS HIGH-VALUES.
              PERFORM TERMINO.
              STOP RUN.

       INICIO.
              OPEN INPUT CADMERC MOVMERC
                  OUTPUT ATUMERC RELERRO RELMERC.
              PERFORM LER-CAD.
              PERFORM LER-MOV.

       LER-CAD.
              READ CADMERC AT END MOVE HIGH-VALUES TO CH-CAD.
              IF CH-CAD NOT EQUAL HIGH-VALUES
                     MOVE CODIGO-CAD TO CH-CAD.

       LER-MOV.
              READ MOVMERC AT END MOVE HIGH-VALUES TO CH-MOV.
              IF CH-MOV NOT EQUAL HIGH-VALUES
                     MOVE CODIGO-MOV TO CH-MOV.

       PRINCIPAL.
              IF CH-MOV EQUALS HIGH-VALUES
                     PERFORM ROT-MERGE
              ELSE
                     PERFORM ATUALIZACAO.

       ATUALIZACAO.
              IF INCLUSAO
                     PERFORM INCLUSAO-ROT.
              IF ALTERACAO
                     PERFORM ALTERACAO-ROT.
              IF EXCLUSAO
                     PERFORM EXCLUSAO-ROT.

       INCLUSAO-ROT.
              PERFORM ROT-MERGE 
              UNTIL CH-CAD NOT LESS THAN CH-MOV.
              IF CH-CAD EQUALS CH-MOV
                     PERFORM INCLUSAO-ERR
              ELSE
                     PERFORM ROT-MERGE.

       ALTERACAO-ROT.
              PERFORM ROT-MERGE 
              UNTIL CH-CAD NOT LESS THAN CH-MOV.
              IF CH-CAD EQUALS CH-MOV
                     PERFORM MOV-MENOR
              ELSE
                     PERFORM ALTERACAO-ERR.

       EXCLUSAO-ROT.
              PERFORM ROT-MERGE 
              UNTIL CH-CAD NOT LESS THAN CH-MOV.
              IF CH-CAD EQUALS CH-MOV
                     PERFORM LER-CAD
                     PERFORM LER-MOV
              ELSE
                     PERFORM EXCLUSAO-ERR.

       ROT-MERGE.
              IF CH-CAD = CH-MOV
                     PERFORM IGUAIS
              ELSE
                     IF CH-CAD < CH-MOV
                            PERFORM CAD-MENOR
                     ELSE
                            PERFORM MOV-MENOR.
              IF QNT-ESTOQUE-ATU < ESTOQUE-MIN-ATU
                     PERFORM ESTOQUE-CRITICO.

       IGUAIS.
              PERFORM CAD-MENOR.
              PERFORM MOV-MENOR.

       CAD-MENOR.
              MOVE CODIGO-CAD TO CODIGO-ATU.
              MOVE DESCRICAO-CAD TO DESCRICAO-ATU.
              MOVE ESTOQUE-MIN-CAD TO ESTOQUE-MIN-ATU.
              MOVE QNT-ESTOQUE-CAD TO QNT-ESTOQUE-ATU.
              MOVE PRECO-UNIT-CAD TO PRECO-UNIT-ATU.
              WRITE REG-ATUMERC.
              PERFORM LER-CAD.

       MOV-MENOR.
              MOVE CODIGO-MOV TO CODIGO-ATU.
              MOVE DESCRICAO-MOV TO DESCRICAO-ATU.
              MOVE ESTOQUE-MIN-MOV TO ESTOQUE-MIN-ATU.
              MOVE QNT-ESTOQUE-MOV TO QNT-ESTOQUE-ATU.
              MOVE PRECO-UNIT-MOV TO PRECO-UNIT-ATU.
              WRITE REG-ATUMERC.
              PERFORM LER-MOV.

       ESTOQUE-CRITICO.
              IF CT-LIN > 40
                     PERFORM CABECALHO.
              MOVE CODIGO-ATU TO VAR-CODIGO.
              MOVE DESCRICAO-ATU TO VAR-DESCRICAO.
              MOVE ESTOQUE-MIN-ATU TO VAR-ESTOQUE-MIN.
              MOVE QNT-ESTOQUE-ATU TO VAR-QNT-ESTOQUE.
              MOVE PRECO-UNIT-ATU TO VAR-PRECO-UNIT.
              WRITE REG-RELMERC FROM CAB-DETALHE
                    AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN.

       CABECALHO.
              MOVE SPACES TO REG-RELMERC.
              WRITE REG-RELMERC AFTER ADVANCING PAGE.
              ADD 1 TO CT-PAG.
              MOVE CT-PAG TO VAR-PAG.
              WRITE REG-RELMERC FROM CAB-1
                     AFTER ADVANCING 1 LINE.
              WRITE REG-RELMERC FROM CAB-2
                     AFTER ADVANCING 2 LINES.
              WRITE REG-RELMERC FROM CAB-3
                     AFTER ADVANCING 1 LINE.
              MOVE SPACES TO REG-RELMERC.
              WRITE REG-RELMERC AFTER ADVANCING 1 LINE.
              MOVE ZEROES TO CT-LIN.

       INCLUSAO-ERR.
              MOVE "INCLUSÃO DE REG. JA EXISTENTE " TO VAR-MENSAGEM.
              PERFORM ERR.

       ALTERACAO-ERR.
              MOVE "ALTERAÇÃO DE REG NAO EXISTENTE" TO VAR-MENSAGEM.
              PERFORM ERR.

       EXCLUSAO-ERR.
              MOVE "EXCLUSÃO DE REG NAO EXISTENTE " TO VAR-MENSAGEM.
              PERFORM ERR.

       ERR.
              IF CT-LIN-ERR > 40
                     PERFORM CABECALHO-ERR.
              MOVE CODIGO-MOV TO VAR-CODIGO-ERR.
              MOVE DESCRICAO-MOV TO VAR-DESCRICAO-ERR.
              WRITE REG-RELERRO FROM CAB-DETALHE-ERR
                     AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN-ERR.
              PERFORM LER-MOV.

       CABECALHO-ERR.
              MOVE SPACES TO REG-RELERRO.
              WRITE REG-RELERRO AFTER ADVANCING PAGE.
              ADD 1 TO CT-PAG-ERR.
              MOVE CT-PAG-ERR TO VAR-PAG-ERR.
              WRITE REG-RELERRO FROM CAB-1-ERR
                     AFTER ADVANCING 1 LINE.
              WRITE REG-RELERRO FROM CAB-2-ERR
                     AFTER ADVANCING 2 LINES.
              WRITE REG-RELERRO FROM CAB-3-ERR
                     AFTER ADVANCING 1 LINE.
              MOVE SPACES TO REG-RELERRO.
              WRITE REG-RELERRO AFTER ADVANCING 1 LINE.
              MOVE ZEROES TO CT-LIN-ERR.

       TERMINO.
              CLOSE CADMERC MOVMERC ATUMERC RELERRO RELMERC.
