      ******************************************************************
      * Author: Luis Angel Baez Nieto
      * Date: 23/01/24
      * Purpose: Learning proyect number 2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 2EMPCAP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEES ASSIGN TO DISK.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEES.
       01  EMP-REG.
           02 EMP-NOMI PIC 9(06) VALUE ZEROES.
           02 EMP-NOMB PIC X(20) VALUE "NAN".
           02 EMP-DEPT PIC X(20) VALUE "NAN".
           02 EMP-PERC PIC 9(05)V99 VALUE ZEROES.
           02 EMP-DEDU PIC 9(05) VALUE ZEROES.
       WORKING-STORAGE SECTION.
       77  RESP PIC X VALUE "Y".
       77  ERR PIC 9 VALUE 0.
       77  DEPT-VALIDOS PIC X(3).
           88 DEPT-ACC VALUE "ADM", "CON", "MER", "SIS", "RH", "TEC".
       77  EMP-CONT PIC 99 VALUE 0.
       01  EMP-DEPT-PERC-TAB.
           02 EMP-DEPT-PERC OCCURS 6 TIMES.
               03 PERC-SUM PIC 9(06)V99 VALUE 0.
       01  EMP-DEPT-DEDU-TAB.
           02 EMP-DEPT-DEDU OCCURS 6 TIMES.
               03 DEDU-SUM PIC 9(06) VALUE 0.
       PROCEDURE DIVISION.

      *------------------------ Main procedure ------------------------
       MAIN-PROCEDURE.
           DISPLAY "INICIO DE CAPTURA DE EMPLEADOS".
           OPEN OUTPUT EMPLOYEES.
           PERFORM EMP-CAPTURA UNTIL RESP = "N".
           DISPLAY "TOTAL DE EMPLEADOS:", EMP-CONT.
           DISPLAY "PERCEPCIONES EN ADMINISTRACION", PERC-SUM(1).
           DISPLAY "DEDUCCIONES EN ADMINISTRACION", DEDU-SUM(1).
           DISPLAY "PERCEPCIONES EN CONTABILIDAD", PERC-SUM(2).
           DISPLAY "DEDUCCIONES EN CONTABILIDAD", DEDU-SUM(2).
           DISPLAY "PERCEPCIONES EN MERCADOTECNIA", PERC-SUM(3).
           DISPLAY "DEDUCCIONES EN MERCADOTECNIA", DEDU-SUM(3).
           DISPLAY "PERCEPCIONES EN SISTEMAS", PERC-SUM(4).
           DISPLAY "DEDUCCIONES EN SISTEMAS", DEDU-SUM(4).
           DISPLAY "PERCEPCIONES EN R.H.", PERC-SUM(5).
           DISPLAY "DEDUCCIONES EN R.H.", DEDU-SUM(5).
           DISPLAY "PERCEPCIONES EN TECNOLOGIA", PERC-SUM(6).
           DISPLAY "DEDUCCIONES EN TECNOLOGIA", DEDU-SUM(6).
           CLOSE EMPLOYEES.
           STOP RUN.


      *------------------------- EMP CAPTURE -------------------------
       EMP-CAPTURA.
           MOVE 0 TO ERR.
           DISPLAY "INGRESE NOMINA:".
           ACCEPT EMP-NOMI.
           DISPLAY "INGRESE NOMBRE:".
           ACCEPT EMP-NOMB.
           DISPLAY "INGRESE DEPARTAMENTO:".
           ACCEPT EMP-DEPT.
           MOVE EMP-DEPT TO DEPT-VALIDOS.
           IF DEPT-ACC
               DISPLAY "INGRESE PERCEPCION"
               ACCEPT EMP-PERC
               DISPLAY "INGRESE DEDUCCION"
               ACCEPT EMP-DEDU
           ELSE
               DISPLAY "DEPARTAMENTO INVALIDO"
               MOVE 1 TO ERR.
           IF ERR = 0
               WRITE EMP-REG.
               ADD 1 TO EMP-CONT.
               IF EMP-DEPT = "ADM"
                   ADD EMP-PERC TO PERC-SUM(1)
                   ADD EMP-DEDU TO DEDU-SUM(1)
               ELSE IF EMP-DEPT = "CON"
                   ADD EMP-PERC TO PERC-SUM(2)
                   ADD EMP-DEDU TO DEDU-SUM(2)
               ELSE IF EMP-DEPT = "MER"
                   ADD EMP-PERC TO PERC-SUM(3)
                   ADD EMP-DEDU TO DEDU-SUM(3)
               ELSE IF EMP-DEPT = "SIS"
                   ADD EMP-PERC TO PERC-SUM(4)
                   ADD EMP-DEDU TO DEDU-SUM(4)
               ELSE IF EMP-DEPT = "RH"
                   ADD EMP-PERC TO PERC-SUM(5)
                   ADD EMP-DEDU TO DEDU-SUM(5)
               ELSE IF EMP-DEPT = "TEC"
                   ADD EMP-PERC TO PERC-SUM(6)
                   ADD EMP-DEDU TO DEDU-SUM(6).
           DISPLAY "DESEA INGRESAR OTRO EMPLEDO? (S/N):"
           ACCEPT RESP.

       END PROGRAM 2EMPCAP.
