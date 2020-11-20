       IDENTIFICATION DIVISION.
       PROGRAM-ID. "MENU".

      *
WOWBGN*
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      ******************************************************************
      * Beginning of editable Special-Names.
      *   You can edit code between here and the next marker.
WOWCOD* WOWSPN
           ALPHABET UPPER-LOWER IS
               1 THRU 65,
               'A' ALSO 'a', 'B' ALSO 'b', 'C' ALSO 'c', 'D' ALSO 'd',
               'E' ALSO 'e', 'F' ALSO 'f', 'G' ALSO 'g', 'H' ALSO 'h',
               'I' ALSO 'i', 'J' ALSO 'j', 'K' ALSO 'k', 'L' ALSO 'l',
               'M' ALSO 'm', 'N' ALSO 'n', 'O' ALSO 'o', 'P' ALSO 'p',
               'Q' ALSO 'q', 'R' ALSO 'r', 'S' ALSO 's', 'T' ALSO 't',
               'U' ALSO 'u', 'V' ALSO 'v', 'W' ALSO 'w', 'X' ALSO 'x',
               'Y' ALSO 'y', 'Z' ALSO 'z',   2 THRU  7, 124 THRU 128 ;
                    DECIMAL-POINT IS COMMA.
WOWCOD*
      * End of editable Special-Names.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      ******************************************************************
      * Beginning of editable File-Control.
      *   You can edit code between here and the next marker.
WOWCOD* WOWFCT
           copy tabgral.sel.
WOWCOD*
      * End of editable File-Control.
      ******************************************************************

       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      * Beginning of editable File Section.
      *   You can edit code between here and the next marker.
WOWCOD* WOWFLS
           copy tabgral.fd.
WOWCOD*
      * End of editable File Section.
      ******************************************************************

       WORKING-STORAGE SECTION.
      ******************************************************************
      * Beginning of editable Working-Storage Section.
      *   You can edit code between here and the next marker.
WOWCOD* WOWPWS
       77  res                     pic 9(04).
       77  tipo                    pic 9(02).
       77  nivel                   pic 9(02).
       77  texto                   pic x(40).
       77  programa                pic x(40).
WOWCOD*
      * End of editable Working-Storage Section.
      ******************************************************************

      *
      * Generated Form Handles
      *
       01 PRINCIPAL-H PIC 9(10) BINARY(8) VALUE 0.

           COPY "principal.wws".

           COPY "WINDOWS.CPY".

       PROCEDURE DIVISION.

       MAIN SECTION.

       MAIN-PROCEDURE.
           SET WOW-QUIT TO FALSE.
           PERFORM PROGRAM-INITIALIZATION.
           IF NOT WOW-QUIT
              PERFORM CREATE-WINDOWS
              PERFORM PROCESS-EVENTS UNTIL WOW-QUIT
              PERFORM DESTROY-WINDOWS
           END-IF.
           PERFORM PROGRAM-SHUTDOWN.
           EXIT PROGRAM.
           STOP RUN.

       PROGRAM-INITIALIZATION SECTION.

       PROGRAM-INIT-PARAGRAPH.
      ******************************************************************
      * Beginning of editable Program-Initialization.
      *   You can edit code between here and the next marker.
WOWCOD* WOWPPI
           open input tabgral.
WOWCOD*
      * End of editable Program-Initialization.
      ******************************************************************

       PROGRAM-SHUTDOWN SECTION.

       PROGRAM-SHUTDOWN-PARAGRAPH.
      ******************************************************************
      * Beginning of editable Program-Shutdown.
      *   You can edit code between here and the next marker.
WOWCOD* WOWPPS
           close tabgral.
WOWCOD*
      * End of editable Program-Shutdown.
      ******************************************************************

       CREATE-WINDOWS SECTION.

       CREATE-WINDOWS-PARAGRAPH.
           PERFORM PRINCIPAL-CREATE-WINDOW.

       DESTROY-WINDOWS SECTION.

       DESTROY-WINDOWS-PARAGRAPH.
           PERFORM PRINCIPAL-DESTROY-WINDOW.

       PROCESS-EVENTS SECTION.

       PROCESS-EVENTS-PARAGRAPH.
           CALL WOWGETMESSAGE USING WIN-RETURN WIN-MSG-WS WM-NOTIFY-WS.
           IF WIN-RETURN IS EQUAL TO 0 SET WOW-QUIT TO TRUE.
           IF NOT WOW-QUIT EVALUATE WIN-MSG-HANDLE
             WHEN PRINCIPAL-H PERFORM PRINCIPAL-EVALUATE-EVENTS
           END-EVALUATE.

       USER-PROCEDURES SECTION.

      ******************************************************************
      * Beginning of editable Procedure Division.
      *   You can edit code between here and the next marker.
WOWCOD* WOWPPR
       INICIO.
           perform InicializarMenu

           perform CargarMenuItem.

       InicializarMenu.
           Call AXDoMethod Using Win-Return
                menu-H "ClearItems" Giving res.

       CargarMenuItem.
           Call AXDoMethod Using Win-Return
                menu-H "AddItem" tipo texto 0 Giving res

           Call AXSetIndexProp Using Win-Return
                menu-H "ItemLevel" nivel res
           Call AXSetIndexProp Using Win-Return
                menu-H "ItemTextPosition" 3 res
           Call AXSetIndexProp Using Win-Return
                menu-H "ItemTips" programa res.
WOWCOD*
      * End of editable Procedure Division.
      ******************************************************************

      *
WOWDNE*
      *
           COPY "principal.wpr".

       END PROGRAM "MENU".
