       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EX01.
       AUTHOR.          ANDRE LUIZ.
       INSTALLATION.    FATEC-PC.
       DATE-WRITTEN.    30/11/2015.
       DATE-COMPILED.   
       SECURITY.        APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.         REALIZA O MERGE DOS ARQUIVOS ARQA
                        E ARQB PARA O ARQC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       OBJECT-COMPUTER.    FATEC-PC.
       SOURCE-COMPUTER.    ANDRE-PC.
       SPECIAL-NAMES.      DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
       SELECT ARQA ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT ARQB ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT ARQC ASSIGN TO DISK
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD ARQA
       LABEL RECORD ARE STANDARD
       VALUE OF FILE-ID IS "ARQA.DAT".

       01 REG-A.
              02 CODIGO-A    PIC 999.
              02 NOME-A      PIC X(30).
              02 TURMA-A     PIC X(20).

       FD ARQB
       LABEL RECORD ARE STANDARD
       VALUE OF FILE-ID IS "ARQB.DAT".

       01 REG-B.
              02 CODIGO-B    PIC 999.
              02 NOME-B      PIC X(30).
              02 TURMA-B     PIC X(20).

       FD ARQC
       LABEL RECORD ARE STANDARD
       VALUE OF FILE-ID IS "ARQC.DAT".

       01 REG-C.
              02 CODIGO-C    PIC 999.
              02 NOME-C      PIC X(30).
              02 TURMA-C     PIC X(20).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ PIC XXX VALUE "NAO".
       77 CH-A    PIC X(4) VALUE SPACES.
       77 CH-B    PIC X(4) VALUE SPACES.

       PROCEDURE DIVISION.

       MAIN.
              PERFORM INICIO.
              PERFORM PRINCIPAL
                     UNTIL CH-A EQUAL CH-B AND CH-B EQUAL HIGH-VALUES.
              PERFORM TERMINO.
              STOP RUN.

       INICIO.
              OPEN INPUT ARQA ARQB
                  OUTPUT ARQC.
              PERFORM LER-A.
              PERFORM LER-B.

       LER-A.
              READ ARQA AT END MOVE HIGH-VALUES TO CH-A.
              IF CH-A NOT EQUAL HIGH-VALUES
                     MOVE CODIGO-A TO CH-A.

       LER-B.
              READ ARQB AT END MOVE HIGH-VALUES TO CH-B.
              IF CH-B NOT EQUAL HIGH-VALUES
                     MOVE CODIGO-B TO CH-B.

       PRINCIPAL.
              IF CH-A = CH-B
                     PERFORM IGUAIS
              ELSE
                     IF CH-A < CH-B
                            PERFORM A-MENOR
                     ELSE
                            PERFORM B-MENOR.

       A-MENOR.
              MOVE CODIGO-A TO CODIGO-C.
              MOVE NOME-A TO NOME-C.
              MOVE TURMA-A TO TURMA-C.
              WRITE REG-C.
              PERFORM LER-A.

       B-MENOR.
              MOVE CODIGO-B TO CODIGO-C.
              MOVE NOME-B TO NOME-C.
              MOVE TURMA-B TO TURMA-C.
              WRITE REG-C.
              PERFORM LER-B.

       IGUAIS.
              MOVE CODIGO-A TO CODIGO-C.
              MOVE NOME-A TO NOME-C.
              MOVE TURMA-A TO TURMA-C.
              WRITE REG-C.
              MOVE CODIGO-B TO CODIGO-C.
              MOVE NOME-B TO NOME-C
              MOVE TURMA-B TO TURMA-C.
              WRITE REG-C.
              PERFORM LER-A.
              PERFORM LER-B.              

       TERMINO.
              CLOSE ARQA ARQB ARQC.