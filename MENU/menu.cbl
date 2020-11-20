       IDENTIFICATION DIVISION.
       PROGRAM-ID. "MENU".

      *
WOWBGN*
      *
       DATA DIVISION.
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
           EXIT PROGRAM.
           STOP RUN.

       PROGRAM-INITIALIZATION SECTION.

       PROGRAM-INIT-PARAGRAPH.
      ******************************************************************
      * Beginning of editable Program-Initialization.
      *   You can edit code between here and the next marker.
WOWCOD* WOWPPI
           display "inicio programa".
WOWCOD*
      * End of editable Program-Initialization.
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

