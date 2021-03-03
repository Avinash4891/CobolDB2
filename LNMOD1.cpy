      *>************************************************************************
      *> Program:      LNMOD1.cpy
      *>
      *> Usage:        Use this copy file in GnuCOBOL DB2 programs.
      *>
      *>************************************************************************
      *> Date       Name / Change description
      *> ========== ============================================================
      *> 
      *>            - first version.
      *>************************************************************************

       01 LN-MOD.
         02 LN-INPUT.
           03 LN-FNC                      PIC X(2).
              88 V-LN-FNC-CONNECT         VALUE "CO".
              88 V-LN-FNC-CONNECT-RESET   VALUE "CR".
           03 LN-CONNECT.
             04 LN-DBALIAS            PIC X(9).
             04 LN-USERID             PIC X(20).
             04 LN-PSWD               PIC X(20).
         02 LN-OUTPUT.
           03 LN-MSG.
             04 LN-SQLCODE                PIC S9(10).
             04 LN-SQLSTATE               PIC X(5).
             04 LN-MSG-1                  PIC X(80).
             04 LN-MSG-2                  PIC X(80).
             04 LN-MSG-3                  PIC X(80).
             04 LN-MSG-4                  PIC X(80).
