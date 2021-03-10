      *>************************************************************************
      *>  SAMPLE DB2 INSERT PROGRAM
      *>************************************************************************

      *>************************************************************************
      *> Program:      DB2FETCH.sqb
      *>
      *> Purpose:      DB2 FETCH Module
      *>
      *> Author:       AVINASH KUMAR
      *>
      *> Date-Written: 2021.03.10
      *>
      *>
      *>               Implemented features:
      *>               - FETCH FROM DB2
      *>               - WRITE TO OUTPUT FILE
      *>               - TABLE EMPLOYEE
      *>
      *>************************************************************************
      *> Date       Name / Change description
      *> ========== ============================================================
      *> 2021.03.10 Avinash Kumar
      *>            - FIRST VERSION.
      *>************************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2FETCH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT EMP-OUTPUT ASSIGN TO 'output.txt'
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMP-OUTPUT.
           01 OUT-EMPLOYEE-DATA.
               10 OUT-EMPNO            PIC X(6).
               10 OUT-FIRSTNME         PIC X(12).
               10 OUT-MIDINIT          PIC X(1).
               10 OUT-LASTNAME         PIC X(15).
               10 OUT-WORKDEPT         PIC X(3).
               10 OUT-PHONENO          PIC X(4).
               10 OUT-HIREDATE         PIC X(10).
               10 OUT-JOB              PIC X(8).
               10 OUT-EDLEVEL          PIC 9(05).
               10 OUT-SEX              PIC X(1).
               10 OUT-BIRTHDATE        PIC X(10).
               10 OUT-SALARY           PIC 9(09).
               10 OUT-BONUS            PIC 9(09).
               10 OUT-COMM             PIC 9(09).

       WORKING-STORAGE SECTION.

       01  SQLDA-ID pic 9(4) comp-5.
       01  SQLDSIZE pic 9(4) comp-5.
       01  SQL-STMT-ID pic 9(4) comp-5.
       01  SQLVAR-INDEX pic 9(4) comp-5.
       01  SQL-DATA-TYPE pic 9(4) comp-5.
       01  SQL-HOST-VAR-LENGTH pic 9(9) comp-5.
       01  SQL-S-HOST-VAR-LENGTH pic 9(9) comp-5.
       01  SQL-S-LITERAL pic X(258).
       01  SQL-LITERAL1 pic X(130).
       01  SQL-LITERAL2 pic X(130).
       01  SQL-LITERAL3 pic X(130).
       01  SQL-LITERAL4 pic X(130).
       01  SQL-LITERAL5 pic X(130).
       01  SQL-LITERAL6 pic X(130).
       01  SQL-LITERAL7 pic X(130).
       01  SQL-LITERAL8 pic X(130).
       01  SQL-LITERAL9 pic X(130).
       01  SQL-LITERAL10 pic X(130).
       01  SQL-IS-LITERAL pic 9(4) comp-5 value 1.
       01  SQL-IS-INPUT-HVAR pic 9(4) comp-5 value 2.
       01  SQL-CALL-TYPE pic 9(4) comp-5.
       01  SQL-SECTIONUMBER pic 9(4) comp-5.
       01  SQL-INPUT-SQLDA-ID pic 9(4) comp-5.
       01  SQL-OUTPUT-SQLDA-ID pic 9(4) comp-5.
       01  SQL-VERSION-NUMBER pic 9(4) comp-5.
       01  SQL-ARRAY-SIZE pic 9(4) comp-5.
       01  SQL-IS-STRUCT  pic 9(4) comp-5.
       01  SQL-IS-IND-STRUCT pic 9(4) comp-5.
       01  SQL-STRUCT-SIZE pic 9(4) comp-5.
       01  SQLA-PROGRAM-ID.
           05 SQL-PART1 pic 9(4) COMP-5 value 172.
           05 SQL-PART2 pic X(6) value "AEAVAI".
           05 SQL-PART3 pic X(24) value "eBsaHKDl01111 2         ".
           05 SQL-PART4 pic 9(4) COMP-5 value 8.
           05 SQL-PART5 pic X(8) value "DB2INST1".
           05 SQL-PART6 pic X(120) value LOW-VALUES.
           05 SQL-PART7 pic 9(4) COMP-5 value 8.
           05 SQL-PART8 pic X(8) value "DB2FETCH".
           05 SQL-PART9 pic X(120) value LOW-VALUES.
                               

      *> SQL communication area
       COPY "sqlca.cpy".

      *> SQL status
       01 WS-SQL-STATUS                PIC S9(9) COMP-5.
          88 SQL-STATUS-OK             VALUE    0.
          88 SQL-STATUS-NOT-FOUND      VALUE  100.
          88 SQL-STATUS-DUP            VALUE -803.

       01 WS-CSR-STATUS                PIC X(01).
          88 CSR-NOT-END-OF-DATA  VALUE 'N'.
          88 CSR-END-OF-DATA      VALUE 'E'.

      *> SQL declare variables
       
      *EXEC SQL BEGIN DECLARE SECTION END-EXEC.

        
      *EXEC SQL INCLUDE EMPLOYEE END-EXEC

       
      *SQL0062W  Starting INCLUDE of file 
      *"/src/workspace/CobolDB2/EMPLOYEE.sqb".

       01 EMPLOYEE.
         05 EMP-EMPNO PIC X(6).
         05 EMP-FIRSTNME.
           49 EMP-FIRSTNME-LEN PIC S9(4) COMP-5.
           49 EMP-FIRSTNME-DAT PIC X(12).
         05 EMP-MIDINIT PIC X(1).
         05 EMP-LASTNAME.
           49 EMP-LASTNAME-LEN PIC S9(4) COMP-5.
           49 EMP-LASTNAME-DAT PIC X(15).
         05 EMP-WORKDEPT PIC X(3).
         05 EMP-PHONENO PIC X(4).
         05 EMP-HIREDATE PIC X(10).
         05 EMP-JOB PIC X(8).
         05 EMP-EDLEVEL PIC S9(4) COMP-5.
         05 EMP-SEX PIC X(1).
         05 EMP-BIRTHDATE PIC X(10).
         05 EMP-SALARY PIC S9(7)V9(2) COMP-3.
         05 EMP-BONUS PIC S9(7)V9(2) COMP-3.
         05 EMP-COMM PIC S9(7)V9(2) COMP-3.

       01 IND-TAB-EMPLOYEE.
         05 IND-EMPLOYEE PIC S9(4) COMP-5 OCCURS 14 TIMES.

      *SQL0063W  Completed INCLUDE of file "EMPLOYEE.sqb".


       01 HV-AREA.
           05 HV-MAX-EMP            PIC X(6).
           05 HV-MIN-EMP            PIC X(6).


       
      *EXEC SQL END   DECLARE SECTION END-EXEC
                                               

       
      *EXEC SQL 
      *DECLARE CURSOR_ALL_EMPS CURSOR FOR
      *      SELECT  EMPNO
      *             ,FIRSTNME
      *             ,MIDINIT
      *             ,LASTNAME
      *             ,WORKDEPT
      *             ,PHONENO
      *             ,HIREDATE
      *             ,JOB
      *             ,EDLEVEL
      *             ,SEX
      *             ,BIRTHDATE
      *             ,SALARY
      *             ,BONUS
      *             ,COMM
      *      FROM EMPLOYEE
      *      WHERE EMPNO <= :HV-MAX-EMP
      *        AND EMPNO >= :HV-MIN-EMP
      *      ORDER BY  EMPNO             ASC
      * END-EXEC
                


       LINKAGE SECTION.
        COPY "EMPDB2CO.cpy".

       PROCEDURE DIVISION USING CPY-EMPLOYEE.

      *>------------------------------------------------------------------------
       MAIN-DB2FETCH SECTION.
      *>------------------------------------------------------------------------
           DISPLAY "DB2FETCH: INSIDE DB2 FETCH MODULE"

           IF CPY-EMPLOYEE-SELECT
      *>--- OPEN I-O FILE
              OPEN EXTEND EMP-OUTPUT
       *>--- PREPARE HOST VARIABLES
              PERFORM MOVE-COPY-TO-HOST THRU MOVE-COPY-TO-HOST-EXIT

       *>--- OPEN
              PERFORM OPEN-CURSOR-ALL-EMPS
                 THRU OPEN-CURSOR-ALL-EMPS-EXIT

       *>--- FETCH -- LOOP -- WRITE
              PERFORM FETCH-FROM-EMPL
                 THRU FETCH-FROM-EMPL-EXIT

       *>--- CLOSE CURSOR AND FILE
              PERFORM CLOSE-CURSOR-ALL-EMPS
                 THRU CLOSE-CURSOR-ALL-EMPS-EXIT

              CLOSE EMP-OUTPUT

           END-IF

           DISPLAY "DB2FETCH: EXITING DB2 FETCH MODULE"

           GOBACK

          .
       MAIN-DB2FETCH-EX.
          EXIT.

      *>------------------------------------------------------------------------
       MOVE-COPY-TO-HOST SECTION.
      *>------------------------------------------------------------------------

               INITIALIZE EMPLOYEE
                          HV-AREA

               IF CPY-EMPNO > SPACES
                   MOVE CPY-EMPNO              TO EMP-EMPNO
                   MOVE EMP-EMPNO              TO HV-MIN-EMP
                                                  HV-MAX-EMP
               ELSE
                   MOVE LOW-VALUES             TO HV-MIN-EMP
                   MOVE HIGH-VALUES            TO HV-MAX-EMP
               END-IF
          .
       MOVE-COPY-TO-HOST-EXIT.
          EXIT.

      *>------------------------------------------------------------------------
       FETCH-FROM-EMPL SECTION.
      *>------------------------------------------------------------------------
           DISPLAY "DB2FETCH: FETCHING FROM EMPLOYEE TABLE"

           SET CSR-NOT-END-OF-DATA TO TRUE

           PERFORM SELECT-ROW-FROM-TABLE
              THRU SELECT-ROW-FROM-TABLE-EXIT
             UNTIL CSR-END-OF-DATA

          .
       FETCH-FROM-EMPL-EXIT.
          EXIT.

      *>------------------------------------------------------------------------
       SELECT-ROW-FROM-TABLE SECTION.
      *>------------------------------------------------------------------------
           DISPLAY "DB2FETCH: SELECT ROW"
           INITIALIZE EMPLOYEE

          
      *EXEC SQL 
      *FETCH CURSOR_ALL_EMPS
      *          INTO :EMP-EMPNO
      *             , :EMP-FIRSTNME
      *             , :EMP-MIDINIT
      *             , :EMP-LASTNAME
      *             , :EMP-WORKDEPT
      *             , :EMP-PHONENO
      *             , :EMP-HIREDATE
      *             , :EMP-JOB
      *             , :EMP-EDLEVEL
      *             , :EMP-SEX
      *             , :EMP-BIRTHDATE
      *             , :EMP-SALARY
      *             , :EMP-BONUS
      *             , :EMP-COMM
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 1 TO SQL-STMT-ID 
           MOVE 14 TO SQLDSIZE 
           MOVE 3 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 6 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-EMPNO
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 12 TO SQL-HOST-VAR-LENGTH
           MOVE 448 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-FIRSTNME
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 1 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-MIDINIT
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 15 TO SQL-HOST-VAR-LENGTH
           MOVE 448 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-LASTNAME
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-WORKDEPT
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 4 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 5 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-PHONENO
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 10 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 6 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-HIREDATE
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 8 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 7 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-JOB
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 2 TO SQL-HOST-VAR-LENGTH
           MOVE 500 TO SQL-DATA-TYPE
           MOVE 8 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-EDLEVEL
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 1 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 9 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-SEX
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 10 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 10 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-BIRTHDATE
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 521 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 11 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-SALARY
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 521 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 12 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-BONUS
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 521 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 13 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE EMP-COMM
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 1 TO SQL-SECTIONUMBER 
           MOVE 25 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                                                                        

          MOVE SQLCODE TO WS-SQL-STATUS

          DISPLAY "DB2FETCH: SQLCODE : "
           SQLCODE

           EVALUATE WS-SQL-STATUS
             WHEN 0
               DISPLAY "DB2FETCH: WRITE FILE"
      *>--- WRITE RECORD IN FILE
               PERFORM MOVE-HOST-TO-COPY
                  THRU MOVE-HOST-TO-COPY-EXIT
               WRITE OUT-EMPLOYEE-DATA

             WHEN 100
               DISPLAY "DB2FETCH: END OF DB2 CURSOR"
               SET CSR-END-OF-DATA TO TRUE

             WHEN OTHER
               SET CSR-END-OF-DATA TO TRUE
               SET CPY-EMPLOYEE-FAIL TO TRUE
               SET CPY-EMP-ERROR TO TRUE
           END-EVALUATE

          .
       SELECT-ROW-FROM-TABLE-EXIT.
          EXIT.

      *>------------------------------------------------------------------------
       MOVE-HOST-TO-COPY SECTION.
      *>------------------------------------------------------------------------

               INITIALIZE OUT-EMPLOYEE-DATA

              MOVE  EMP-EMPNO          TO  OUT-EMPNO
              MOVE  EMP-FIRSTNME-DAT   TO  OUT-FIRSTNME
              MOVE  EMP-MIDINIT        TO  OUT-MIDINIT
              MOVE  EMP-LASTNAME-DAT   TO  OUT-LASTNAME
              MOVE  EMP-WORKDEPT       TO  OUT-WORKDEPT
              MOVE  EMP-PHONENO        TO  OUT-PHONENO
              MOVE  EMP-HIREDATE       TO  OUT-HIREDATE
              MOVE  EMP-JOB            TO  OUT-JOB
              MOVE  EMP-EDLEVEL        TO  OUT-EDLEVEL
              MOVE  EMP-SEX            TO  OUT-SEX
              MOVE  EMP-BIRTHDATE      TO  OUT-BIRTHDATE
              MOVE  EMP-SALARY         TO  OUT-SALARY
              MOVE  EMP-BONUS          TO  OUT-BONUS
              MOVE  EMP-COMM           TO  OUT-COMM

          .
       MOVE-HOST-TO-COPY-EXIT.
          EXIT.

      *>------------------------------------------------------------------------
       OPEN-CURSOR-ALL-EMPS SECTION.
      *>------------------------------------------------------------------------
              DISPLAY "DB2FETCH: CURSOR OPEN"
           
      *EXEC SQL 
      *OPEN CURSOR_ALL_EMPS
      *     END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 2 TO SQL-STMT-ID 
           MOVE 2 TO SQLDSIZE 
           MOVE 2 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 6 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-MAX-EMP
            OF
            HV-AREA
            BY VALUE 0
                     0

           MOVE 6 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-MIN-EMP
            OF
            HV-AREA
            BY VALUE 0
                     0

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 2 TO SQL-INPUT-SQLDA-ID 
           MOVE 1 TO SQL-SECTIONUMBER 
           MOVE 26 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                                                                        
              DISPLAY "DB2FETCH: CURSOR OPEN SQLCODE >> " SQLCODE

          .
       OPEN-CURSOR-ALL-EMPS-EXIT.
          EXIT.

      *>------------------------------------------------------------------------
       CLOSE-CURSOR-ALL-EMPS SECTION.
      *>------------------------------------------------------------------------
           DISPLAY "DB2FETCH: CURSOR CLOSE "
           
      *EXEC SQL 
      *CLOSE CURSOR_ALL_EMPS
      *     END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 1 TO SQL-SECTIONUMBER 
           MOVE 20 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                                                                        
           DISPLAY "DB2FETCH: CURSOR CLOSE SQLCODE >> " SQLCODE
          .
       CLOSE-CURSOR-ALL-EMPS-EXIT.
          EXIT.

       END PROGRAM DB2FETCH.
