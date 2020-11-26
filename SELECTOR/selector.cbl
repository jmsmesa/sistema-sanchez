       IDENTIFICATION DIVISION.
       PROGRAM-ID. "SELECTOR".

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
           copy operadores.sel.
WOWCOD*
      * End of editable File-Control.
      ******************************************************************

       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      * Beginning of editable File Section.
      *   You can edit code between here and the next marker.
WOWCOD* WOWFLS
           copy operadores.fd.
WOWCOD*
      * End of editable File Section.
      ******************************************************************

       WORKING-STORAGE SECTION.
      ******************************************************************
      * Beginning of editable Working-Storage Section.
      *   You can edit code between here and the next marker.
WOWCOD* WOWPWS
       77  pos                     pic x(01).
       77  res                     pic 9(04).
       77  tipo                    pic 9(02).
       77  nivel                   pic 9(02).
       77  eof-operadores          pic x(01).
       77  existe-operadores       pic x(01).
       77  error-operadores        pic x(01).
       77  texto                   pic x(80).
       77  ind                     pic 9(04).
       77  sub                     pic 9(04).
       77  c-fila                  pic 9(04).
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
           open input operadores.
WOWCOD*
      * End of editable Program-Initialization.
      ******************************************************************

       PROGRAM-SHUTDOWN SECTION.

       PROGRAM-SHUTDOWN-PARAGRAPH.
      ******************************************************************
      * Beginning of editable Program-Shutdown.
      *   You can edit code between here and the next marker.
WOWCOD* WOWPPS
           close operadores.
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
       Cargar-Listas.
           Call AXDoMethod Using Win-Return menu-H "ClearItems"

           initialize reg-operadores
           perform start-operadores
           if eof-operadores = "n"
              perform leer-operadores-next

              perform varying ind from 1 by 1
                 until eof-operadores = "s"
                    move "0" to pos
                    if operadores-estado = "h" or = "H"
                       move "1" to pos
                    end-if
                    initialize texto
                    string pos ";" operadores-razon-social
                           delimited by "  " into texto
                    if operadores-codigo < 20
                       Call AXDoMethod Using Win-Return
                            lst1-H "AddItem" texto
                    end-if
                    if operadores-codigo > 20 and < 70
                       Call AXDoMethod Using Win-Return
                            lst2-H "AddItem" texto
                       if pos = "1"
                       Call AXDoMethod Using Win-Return
                            menu-H "AddItem" 0 operadores-razon-social 0
                              Giving sub
                              add 1 to ind
                 Call AXSetIndexProp Using Win-Return
                      menu-H "ItemLevel" 1 sub
                 Call AXSetIndexProp Using Win-Return
                      menu-H "ItemTextPosition" 1 sub
                            end-if
                    end-if
                    if operadores-codigo > 70
                        Call AXDoMethod Using Win-Return
                            lst3-H "AddItem" texto
                    end-if
                 perform leer-operadores-next
              end-perform
           end-if.

       leer-operadores.
           move 's' to existe-operadores.
           read operadores
                        invalid key
                                    move 'n' to existe-operadores.
       start-operadores.
           move 'n' to eof-operadores.
           start operadores
                       key not < operadores-key
                             invalid key
                                        move 's' to eof-operadores.
       leer-operadores-next.
           move 'n' to eof-operadores.
           read operadores next
                           at end
                                 move 's' to eof-operadores.
       regrabar-operadores.
           move 'n' to error-operadores.
           rewrite reg-operadores
                             invalid key
                                        move 's' to error-operadores.
       borrar-operadores.
           move 'n' to error-operadores.
           delete operadores
                             invalid key
                                        move 's' to error-operadores.
       grabar-operadores.
           move 'n' to error-operadores.
           write reg-operadores
                             invalid key
                                        move 's' to error-operadores.
WOWCOD*
      * End of editable Procedure Division.
      ******************************************************************

      *
WOWDNE*
      *
           COPY "principal.wpr".

       END PROGRAM "SELECTOR".

