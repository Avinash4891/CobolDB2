      *>************************************************************************
      *>  SAMPLE DB2 UPDATE PROGRAM
      *>************************************************************************

      *>************************************************************************
      *> Program:      DB2UPDT.sqb
      *>
      *> Purpose:      DB2 UPDATE Module
      *>
      *> Author:       AVINASH KUMAR
      *>
      *> Date-Written: 2021.03.01
      *>
      *>
      *>               Implemented features:
      *>               - UPDATE DB2
      *>               - TABLE EMPLOYEE
      *>
      *>************************************************************************
      *> Date       Name / Change description
      *> ========== ============================================================
      *> 2021.03.01 Avinash Kumar
      *>            - FIRST VERSION.
      *>************************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2UPDT.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
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
           05 SQL-PART3 pic X(24) value "QAayMBDl01111 2         ".
           05 SQL-PART4 pic 9(4) COMP-5 value 8.
           05 SQL-PART5 pic X(8) value "DB2INST1".
           05 SQL-PART6 pic X(120) value LOW-VALUES.
           05 SQL-PART7 pic 9(4) COMP-5 value 8.
           05 SQL-PART8 pic X(8) value "DB2UPDT ".
           05 SQL-PART9 pic X(120) value LOW-VALUES.
                               

      *> SQL communication area
       COPY "sqlca.cpy".

      *> SQL status
       01 WS-SQL-STATUS                PIC S9(9) COMP-5.
          88 SQL-STATUS-OK             VALUE    0.
          88 SQL-STATUS-NOT-FOUND      VALUE  100.
          88 SQL-STATUS-DUP            VALUE -803.

      *> SQL declare variables
       
      *EXEC SQL BEGIN DECLARE SECTION END-EXEC.

         
      *EXEC SQL INCLUDE EMPLOYEE END-EXEC

       
      *SQL0062W  Starting INCLUDE of file 
      *"/home/jenkins/agent/workspace/Compile_CobolDB2_Samples/EMP
      *LOYEE.sqb".

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


       
      *EXEC SQL END   DECLARE SECTION END-EXEC
                                               

       LINKAGE SECTION.
        COPY "EMPDB2CO.cpy".

       PROCEDURE DIVISION USING CPY-EMPLOYEE.

      *>------------------------------------------------------------------------
       MAIN-DB2UPDT SECTION.
      *>------------------------------------------------------------------------
           DISPLAY "DB2UPDT: INSIDE DB2 UPDATE MODULE"

           IF CPY-EMPLOYEE-ADD
              PERFORM MOVE-TO-EMP-HOST THRU MOVE-TO-EMP-HOST-EXIT
              PERFORM UPDATE-EMPL THRU UPDATE-EMPL-EXIT
           END-IF
           DISPLAY "DB2UPDT: EXITING DB2 UPDATE MODULE"

           GOBACK

          .
       MAIN-DB2UPDT-EX.
          EXIT.

      *>------------------------------------------------------------------------
       MOVE-TO-EMP-HOST SECTION.
      *>------------------------------------------------------------------------
               INITIALIZE EMPLOYEE

               MOVE CPY-EMPNO              TO EMP-EMPNO
               MOVE LENGTH OF CPY-FIRSTNME TO EMP-FIRSTNME-LEN
               MOVE CPY-FIRSTNME           TO EMP-FIRSTNME-DAT
               MOVE CPY-MIDINIT            TO EMP-MIDINIT
               MOVE LENGTH OF CPY-LASTNAME TO EMP-LASTNAME-LEN
               MOVE CPY-LASTNAME           TO EMP-LASTNAME-DAT
               MOVE CPY-WORKDEPT           TO EMP-WORKDEPT
               MOVE CPY-PHONENO            TO EMP-PHONENO
               MOVE CPY-HIREDATE           TO EMP-HIREDATE
               MOVE CPY-JOB                TO EMP-JOB
               MOVE CPY-EDLEVEL            TO EMP-EDLEVEL
               MOVE CPY-SEX                TO EMP-SEX
               MOVE CPY-BIRTHDATE          TO EMP-BIRTHDATE
               MOVE CPY-SALARY             TO EMP-SALARY
               MOVE CPY-BONUS              TO EMP-BONUS
               MOVE CPY-COMM               TO EMP-COMM
          .
       MOVE-TO-EMP-HOST-EXIT.
          EXIT.

      *>------------------------------------------------------------------------
       UPDATE-EMPL SECTION.
      *>------------------------------------------------------------------------
           DISPLAY "DB2UPDT:UPDATE EMPLOYEE TABLE"

           
      *EXEC SQL 
      *UPDATE EMPLOYEE
      *            SET EMPNO      = :EMP-EMPNO
      *               ,FIRSTNME   = :EMP-FIRSTNME
      *               ,MIDINIT    = :EMP-MIDINIT
      *               ,LASTNAME   = :EMP-LASTNAME
      *               ,WORKDEPT   = :EMP-WORKDEPT
      *               ,PHONENO    = :EMP-PHONENO
      *               ,HIREDATE   = :EMP-HIREDATE
      *               ,JOB        = :EMP-JOB
      *               ,EDLEVEL    = :EMP-EDLEVEL
      *               ,SEX        = :EMP-SEX
      *               ,BIRTHDATE  = :EMP-BIRTHDATE
      *               ,SALARY     = :EMP-SALARY
      *               ,BONUS      = :EMP-BONUS
      *               ,COMM       = :EMP-COMM
      *         WHERE EMPNO =  :EMP-EMPNO
      *     END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 1 TO SQL-STMT-ID 
           MOVE 15 TO SQLDSIZE 
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
            BY REFERENCE EMP-EMPNO
            OF
            EMPLOYEE
            BY VALUE 0
                     0

           MOVE 12 TO SQL-HOST-VAR-LENGTH
           MOVE 448 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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
           MOVE 2 TO SQLDA-ID

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

           MOVE 6 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 14 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

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

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 2 TO SQL-INPUT-SQLDA-ID 
           MOVE 1 TO SQL-SECTIONUMBER 
           MOVE 24 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                                                                        

           MOVE SQLCODE TO WS-SQL-STATUS

           DISPLAY "DB2UPDT: UPDATE SQLCODE : "
           SQLCODE

           EVALUATE WS-SQL-STATUS
             WHEN 0
               SET CPY-EMPLOYEE-SUCCESS TO TRUE
               SET CPY-EMP-ALL-DONE TO TRUE
               PERFORM SQL-COMMIT THRU SQL-COMMIT-EXIT

             WHEN OTHER
               SET CPY-EMPLOYEE-FAIL TO TRUE
               SET CPY-EMP-ERROR TO TRUE
           END-EVALUATE

          .
       UPDATE-EMPL-EXIT.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-COMMIT SECTION.
      *>------------------------------------------------------------------------

           
      *EXEC SQL 
      *COMMIT
      *     END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 0 TO SQL-SECTIONUMBER 
           MOVE 21 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                                                                        

          .
       SQL-COMMIT-EXIT.
          EXIT.

       END PROGRAM DB2UPDT.
