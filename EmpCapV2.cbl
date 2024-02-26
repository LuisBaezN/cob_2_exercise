      ******************************************************************
      * Author: Luis Angel Baez Nieto
      * Date: 23/01/24
      * Purpose: Learning proyect number 1
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
       77  RESP PIC X VALUE "S".
       77  I PIC 9.
       77  ERR PIC 9 VALUE 0.
       77  DEPT-VALIDOS PIC X(3).
           88 DEPT-ACC VALUE "ADM", "CON", "MER", "SIS", "RH", "TEC".
       01  DEPTS-TAB.
           02 DEPTS-NAMES OCCURS 6 TIMES.
               03 DEPTS-NAME PIC X(3) VALUE "NAN".
       77  EMP-CONT PIC 99.
       01  EMP-DEPT-PERC-TAB.
           02 EMP-DEPT-PERC OCCURS 6 TIMES.
               03 PERC-SUM PIC 9(06)V99 VALUE 0.
       01  EMP-DEPT-DEDU-TAB.
           02 EMP-DEPT-DEDU OCCURS 6 TIMES.
               03 DEDU-SUM PIC 9(06) VALUE 0.
       PROCEDURE DIVISION.

      *------------------------ Main procedure ------------------------
       MAIN-PROCEDURE.
           MOVE "ADM" TO DEPTS-NAME(1).
           MOVE "CON" TO DEPTS-NAME(2).
           MOVE "MER" TO DEPTS-NAME(3).
           MOVE "SIS" TO DEPTS-NAME(4).
           MOVE "RH" TO DEPTS-NAME(5).
           MOVE "TEC" TO DEPTS-NAME(6).

           DISPLAY "INICIO DE CAPTURA DE EMPLEADOS".
           OPEN OUTPUT EMPLOYEES.
           PERFORM EMP-CAPTURA UNTIL RESP = "N".
           DISPLAY "TOTAL DE EMPLEADOS:", EMP-CONT.
           PERFORM DISP-DEPT-SUMS VARYING I FROM 1 BY 1 UNTIL I > 6.
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
               WRITE EMP-REG
               ADD 1 TO EMP-CONT
               PERFORM SUM-PER-DEDU VARYING I FROM 1 BY 1 UNTIL I > 6.
           DISPLAY "DESEA INGRESAR OTRO EMPLEADO? (S/N):"
           ACCEPT RESP.

      *------------------------- DISP DEP SUMS -------------------------
       DISP-DEPT-SUMS.
           DISPLAY "PERCEPCIONES EN ", DEPTS-NAME(I), ":" PERC-SUM(I).
           DISPLAY "DEDUCCIONES EN ", DEPTS-NAME(I), ":" DEDU-SUM(I).

      *------------------------- SUM PER DEDU -------------------------
       SUM-PER-DEDU.
           IF EMP-DEPT = DEPTS-NAME(I)
               ADD EMP-PERC TO PERC-SUM(I)
               ADD EMP-DEDU TO DEDU-SUM(I).

       END PROGRAM 2EMPCAP.
