      *>************************************************************************
      *>  SAMPLE DB2 DRIVER PROGRAM
      *>************************************************************************

      *>************************************************************************
      *> Program:      DB2CRUD.cob
      *>
      *> Purpose:      DB2 CRUD Module
      *>
      *> Author:       AVINASH KUMAR
      *>
      *> Date-Written: 2021.03.01
      *>
      *>
      *>               Implemented features:
      *>               - DB2 CRUD OPERATIONS
      *>               - TABLE EMPLOYEE
      *>
      *>************************************************************************
      *> Date       Name / Change description
      *> ========== ============================================================
      *> 2021.03.01 Avinash Kumar
      *>            - FIRST VERSION.
      *>************************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2CRU1.

       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT EMPLOYEE-FILE ASSIGN TO 'input.txt'
        ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
         COPY "EMPDB2CO.cpy".

       WORKING-STORAGE SECTION.

         COPY "LNMOD1.cpy".

        01 WS-THIS-PGM           PIC X(7) VALUE 'DB2CRUD'.
        01 WS-DB2-CONN-PGM       PIC X(7) VALUE 'DB2CONN'.
        01 WS-ADD-PGM            PIC X(8) VALUE 'DB2INSRT'.
        01 WS-UPDATE-PGM         PIC X(7) VALUE 'DB2UPDT'.
        01 WS-DELETE-PGM         PIC X(8) VALUE 'DB2DELET'.

        01 WS-FILE-STATUS        PIC X.
           88 WS-EOF              VALUE 'Y'.
           88 WS-NOT-EOF          VALUE 'N'.

       PROCEDURE DIVISION.

      *>------------------------------------------------------------------------
       MAIN-DB2INSRT SECTION.
      *>------------------------------------------------------------------------
           DISPLAY "DB2CRUD: INSIDE DB2CRUD MODULE"

           OPEN INPUT EMPLOYEE-FILE.
           SET WS-NOT-EOF TO TRUE.

      *>   PERFORM CONNECT-DB2 THRU CONNECT-DB2-EXIT.


            PERFORM PROCESS-INPUT-FILE
               THRU PROCESS-INPUT-FILE-EXIT
               UNTIL WS-EOF

            CLOSE EMPLOYEE-FILE

           DISPLAY "DB2CRUD: EXITING DB2CRUD MODULE"

           STOP RUN.
      *>------------------------------------------------------------------------          .
       MAIN-DB2INSRT-EX.
          EXIT.
      *>------------------------------------------------------------------------

      *>------------------------------------------------------------------------
      *>--- CONNECT-DB2 SECTION.
      *>------------------------------------------------------------------------

      *>---      INITIALIZE LN-MOD
      *>---      MOVE "SAMPLE"   TO LN-DBALIAS OF LN-MOD
      *>---      MOVE "DB2INST1" TO LN-USERID  OF LN-MOD
      *>---      MOVE "db2admin" TO LN-PSWD    OF LN-MOD
      *>---     CALL WS-DB2-CONN-PGM USING LN-MOD END-CALL.

      *>------------------------------------------------------------------------          .
      *>--- CONNECT-DB2-EXIT.
      *>---    EXIT.
      *>------------------------------------------------------------------------

      *>------------------------------------------------------------------------
       PROCESS-INPUT-FILE SECTION.
      *>------------------------------------------------------------------------
           INITIALIZE CPY-EMPLOYEE
           READ EMPLOYEE-FILE INTO CPY-EMPLOYEE
           AT END
               SET WS-EOF TO TRUE
           NOT AT END
               PERFORM PROCESS-RECORD THRU PROCESS-RECORD-EXIT
           END-READ.

      *>------------------------------------------------------------------------          .
       PROCESS-INPUT-FILE-EXIT.
          EXIT.
      *>------------------------------------------------------------------------

      *>------------------------------------------------------------------------
       PROCESS-RECORD SECTION.
      *>------------------------------------------------------------------------
           EVALUATE TRUE
             WHEN CPY-EMPLOYEE-ADD
               CALL WS-ADD-PGM    USING CPY-EMPLOYEE END-CALL
             WHEN CPY-EMPLOYEE-UPDATE
               CALL WS-UPDATE-PGM USING CPY-EMPLOYEE END-CALL
             WHEN CPY-EMPLOYEE-DELETE
               CALL WS-DELETE-PGM USING CPY-EMPLOYEE END-CALL
             WHEN CPY-EMPLOYEE-SELECT
             DISPLAY "OPERATION NOT SUPPORTED CURRENTLY"
             WHEN OTHER
               DISPLAY "NOT A VALID OPERATION"
           END-EVALUATE
           .
      *>------------------------------------------------------------------------          .
       PROCESS-RECORD-EXIT.
          EXIT.
      *>------------------------------------------------------------------------

       END PROGRAM DB2CRU1.
