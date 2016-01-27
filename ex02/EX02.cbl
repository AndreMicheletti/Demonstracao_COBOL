       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EX02.
       AUTHOR.          KELVIN.
       INSTALLATION.    IBM-PC.
       DATE-WRITTEN.    02/12/2015.
       DATE-COMPILED.
       SECURITY.        APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       FILE-CONTROL.
       SELECT CADANT ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT ARQMOV ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT CADATU ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT RELOCORR ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.

       FD CADANT
       LABEL RECORD ARE STANDARD
       VALUE OF FILE-ID IS "CADANT.DAT".

       01 REGANT.
              02 CC-ANT     PIC 9(3).
              02 TIPO-ANT   PIC X.
              02 ENDER-ANT  PIC X(50).

       FD ARQMOV
       LABEL RECORD ARE STANDARD
       VALUE OF FILE-ID IS "ARQMOV.DAT".

       01 REGMOV.
              02 CC-MOV     PIC 9(3).
              02 TIPO-MOV   PIC X.
                     88 INCLUSAO VALUE "I".
                     88 ALTERACAO VALUE "A".
                     88 EXCLUSAO VALUE "E".
              02 ENDER-MOV  PIC X(50).

       FD CADATU
       LABEL RECORD ARE STANDARD
       VALUE OF FILE-ID IS "CADATU.DAT".

       01 REGATU.
              02 CC-ATU     PIC 9(3).
              02 TIPO-ATU   PIC X.
              02 ENDER-ATU  PIC X(50).

       FD RELOCORR
       LABEL RECORD ARE OMITTED.
       01 REGOCORR PIC X(80).

       WORKING-STORAGE SECTION.
       77 CH-ANT PIC X(3) VALUE SPACES.
       77 CH-MOV PIC X(3) VALUE SPACES.
       77 CT-PAG PIC 999 VALUE ZEROES.
       77 CT-LIN PIC 99 VALUE 41.

       01 CAB01.
              02 FILLER   PIC X(23) VALUE SPACES.
              02 FILLER   PIC X(22) VALUE "Relação de atualização".
              02 FILLER   PIC X(24) VALUE SPACES.
              02 FILLER   PIC X(05) VALUE "PAG. ".
              02 VAR-PAG  PIC 9(03).
              02 FILLER   PIC X(03) VALUE SPACES.

       01 CAB02.
            02 FILLER   PIC X(03) VALUE SPACES.
            02 FILLER   PIC X(06) VALUE "codigo".
            02 FILLER   PIC X(05) VALUE SPACES.
            02 FILLER   PIC X(04) VALUE "tipo".
            02 FILLER   PIC X(10) VALUE SPACES.
            02 FILLER   PIC X(08) VALUE "mensagem".
            02 FILLER   PIC X(44) VALUE SPACES.


        01 DETALHE.
            02 FILLER   PIC X(05) VALUE SPACES.
            02 VAR-CC  PIC 9(03). 
            02 FILLER   PIC X(08) VALUE SPACES.
            02 VAR-TIPO PIC X(01).
            02 FILLER   PIC X(10) VALUE SPACES.
            02 VAR-MSG  PIC X(33).
            02 FILLER   PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN.
              PERFORM INICIO.
              PERFORM PRINCIPAL
                 UNTIL CH-ANT EQUALS CH-MOV 
                 AND CH-MOV EQUALS HIGH-VALUES.
              PERFORM TERMINO.
              STOP RUN.

       INICIO.
              OPEN INPUT CADANT ARQMOV
                  OUTPUT CADATU RELOCORR.
              PERFORM LE-ANT.
              PERFORM LE-MOV.

       LE-ANT.
              READ CADANT AT END MOVE HIGH-VALUES TO CH-ANT.
              IF CH-ANT NOT EQUAL HIGH-VALUES
                     MOVE CC-ANT TO CH-ANT.

       LE-MOV.
              READ ARQMOV AT END MOVE HIGH-VALUES TO CH-MOV.
              IF CH-MOV NOT EQUAL HIGH-VALUES
                     MOVE CC-MOV TO CH-MOV.

       PRINCIPAL.
              IF CH-MOV EQUAL HIGH-VALUES
                     PERFORM ROT-MERGE
              ELSE
                     PERFORM ROT-ATUALIZA.

       ROT-ATUALIZA.
              IF INCLUSAO
                     PERFORM ROT-INCLUSAO.
              IF ALTERACAO
                     PERFORM ROT-ALTERACAO.
              IF EXCLUSAO
                     PERFORM ROT-EXCLUSAO.

       ROT-INCLUSAO.
              PERFORM ROT-MERGE UNTIL CH-ANT NOT LESS THAN CH-MOV.
              IF CH-ANT = CH-MOV
                     MOVE "INCLUSÃO P/REG. JÁ EXISTENTE     "
                             TO VAR-MSG
                     MOVE CC-MOV TO VAR-CC
                     MOVE TIPO-MOV TO VAR-TIPO
                     PERFORM ROT-RELAT
                     PERFORM ANT-MENOR
                     PERFORM LE-MOV
              ELSE
                     MOVE "REGISTRO INCLUIDO                "
                             TO VAR-MSG
                     MOVE CC-MOV TO VAR-CC
                     MOVE TIPO-MOV TO VAR-TIPO
                     PERFORM ROT-RELAT
                     PERFORM MOV-MENOR.

       ROT-ALTERACAO.
              PERFORM ROT-MERGE UNTIL CH-ANT NOT LESS THAN CH-MOV.
              IF CH-ANT = CH-MOV
                     MOVE "REGISTRO SUBSTITUIDO             "
                             TO VAR-MSG
                     MOVE CC-ANT TO VAR-CC
                     MOVE TIPO-ANT TO VAR-TIPO
                     PERFORM ROT-RELAT
                     MOVE "REGISTRO SUBSTITUTO              "
                             TO VAR-MSG
                     MOVE CC-MOV TO VAR-CC
                     MOVE TIPO-MOV TO VAR-TIPO
                     PERFORM ROT-RELAT
                     PERFORM MOV-MENOR
                     PERFORM LE-ANT
              ELSE
                     MOVE "ALTERAÇÃO P/REG. INEXISTENTE     "
                             TO VAR-MSG
                     MOVE CC-MOV TO VAR-CC
                     MOVE TIPO-MOV TO VAR-TIPO
                     PERFORM ROT-RELAT
                     PERFORM LE-MOV.

       ROT-EXCLUSAO.
              PERFORM ROT-MERGE UNTIL CH-ANT NOT LESS THAN CH-MOV.
              IF CH-ANT = CH-MOV
                     MOVE "REGISTRO EXCLUIDO                "
                             TO VAR-MSG
                     MOVE CC-MOV TO VAR-CC
                     MOVE TIPO-MOV TO VAR-TIPO
                     PERFORM ROT-RELAT
                     PERFORM LE-MOV
                     PERFORM LE-ANT
              ELSE
                     MOVE "EXCLUSÃO P/REG. INEXISTENTE      "
                             TO VAR-MSG
                     MOVE CC-MOV TO VAR-CC
                     MOVE TIPO-MOV TO VAR-TIPO
                     PERFORM ROT-RELAT
                     PERFORM LE-MOV.

       ROT-MERGE.
              IF CH-ANT = CH-MOV
                     PERFORM MOV-MENOR
              ELSE
                     IF CH-ANT < CH-MOV
                            PERFORM ANT-MENOR
                     ELSE
                            PERFORM MOV-MENOR.

       ROT-RELAT.
              IF CT-LIN > 40
                     PERFORM CAB.
              WRITE REGOCORR FROM DETALHE
                     AFTER ADVANCING 1 LINE.
              ADD 1 TO CT-LIN.

       CAB.
              ADD 1 TO CT-PAG.
              MOVE CT-PAG TO VAR-PAG.
              MOVE SPACES TO REGOCORR.
              WRITE REGOCORR AFTER ADVANCING PAGE.
              WRITE REGOCORR FROM CAB01
                     AFTER ADVANCING 1 LINE.
              WRITE REGOCORR FROM CAB02
                     AFTER ADVANCING 2 LINES.
              MOVE ZEROES TO CT-LIN.

       ANT-MENOR.
              MOVE CC-ANT TO CC-ATU.
              MOVE TIPO-ANT TO TIPO-ATU.
              MOVE ENDER-ANT TO ENDER-ATU.
              WRITE REGATU.
              PERFORM LE-ANT.

       MOV-MENOR.
              MOVE CC-MOV TO CC-ATU.
              MOVE TIPO-MOV TO TIPO-ATU.
              MOVE ENDER-MOV TO ENDER-ATU.
              WRITE REGATU.
              PERFORM LE-MOV.

       TERMINO.
              CLOSE CADANT ARQMOV CADATU RELOCORR.