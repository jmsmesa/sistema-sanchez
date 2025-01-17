       IDENTIFICATION DIVISION.
       PROGRAM-ID. "CLIENTES-ABM".

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
               'Y' ALSO 'y', 'Z' ALSO 'z',   2 THRU  7, 124 THRU 128 .
      *              DECIMAL-POINT IS COMMA.
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
       77  item-seleccionado          pic 9(04).
       77  mi-codigo                  pic 9(04).
       77  mi-opcion                  pic x(01).
       77  eof-operadores             pic x(01).
       77  existe-operadores          pic x(01).
       77  error-operadores           pic x(01).
       77  texto-fecha                pic x(10).
       77  texto                      pic x(80).
       01  fecha                      pic 9(08).
       01  ff redefines fecha.
           02 dia                     pic 9(02).
           02 mes                     pic 9(02).
           02 ano                     pic 9(04).
WOWCOD*
      * End of editable Working-Storage Section.
      ******************************************************************

      *
      * Generated Form Handles
      *
       01 MENU-H PIC 9(10) BINARY(8) VALUE 0.
       01 PRINCIPAL-H PIC 9(10) BINARY(8) VALUE 0.

           COPY "menu.wws".
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
           open i-o operadores.
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
           PERFORM MENU-CREATE-WINDOW.
           PERFORM PRINCIPAL-CREATE-WINDOW.

       DESTROY-WINDOWS SECTION.

       DESTROY-WINDOWS-PARAGRAPH.
           PERFORM MENU-DESTROY-WINDOW.
           PERFORM PRINCIPAL-DESTROY-WINDOW.

       PROCESS-EVENTS SECTION.

       PROCESS-EVENTS-PARAGRAPH.
           CALL WOWGETMESSAGE USING WIN-RETURN WIN-MSG-WS WM-NOTIFY-WS.
           IF WIN-RETURN IS EQUAL TO 0 SET WOW-QUIT TO TRUE.
           IF NOT WOW-QUIT EVALUATE WIN-MSG-HANDLE
             WHEN MENU-H PERFORM MENU-EVALUATE-EVENTS
             WHEN PRINCIPAL-H PERFORM PRINCIPAL-EVALUATE-EVENTS
           END-EVALUATE.

       USER-PROCEDURES SECTION.

      ******************************************************************
      * Beginning of editable Procedure Division.
      *   You can edit code between here and the next marker.
WOWCOD* WOWPPR
       ACTUALIZAR.
           evaluate mi-opcion
              when "a"
                  perform mover-datos-al-registro
                  perform grabar-operadores
              when "b"
                  perform mover-datos-al-registro
                  perform borrar-operadores
              when "m"
                  perform mover-datos-al-registro
                  perform regrabar-operadores
           end-evaluate.

       CARGAR-REGISTRO.
           Call WowGetProp Using Win-Return codigo-H "text" mi-codigo
           initialize reg-operadores
           move mi-codigo to operadores-codigo

           perform leer-operadores
           if existe-operadores = "n" and mi-opcion not = "a"
              Move all 'N' to Message-Box-Flags
              Set Mb-OKCancel Mb-IconHand To True
              Call WowMessageBox Using Win-Return principal-H
                   "No existe el cliente" "Error" Message-Box-Flags
           else if existe-operadores = "s" and mi-opcion = "a"
              Move all 'N' to Message-Box-Flags
              Set Mb-OKCancel Mb-IconHand To True
              Call WowMessageBox Using Win-Return principal-H
                   "Ya existe el cliente" "Error" Message-Box-Flags
           else
              perform mover-datos-al-form
           end-if.

       limpiar-form.

           Call WowSetProp Using Win-Return txtNombre-H "text" " "
           Call WowSetProp Using Win-Return cbServicio-H "text" " "
           Call WowSetProp Using Win-Return dtFecha-H "text" " "
           Call WowSetProp Using Win-Return chEstado-H "Value" 0
           Call WowSetProp Using Win-Return txtImporte-H "text" " ".

       mover-datos-al-form.
           perform limpiar-form.
           Call WowSetProp Using Win-Return txtNombre-H "text"
                operadores-razon-social
           Call WowSetProp Using Win-Return cbServicio-H "text"
                "Telefonia"
           string operadores-fecha-inicio(1:2)  "/"
                  operadores-fecha-inicio(3:2) "/"
                  operadores-fecha-inicio(5:)
                      delimited by size into texto-fecha
           Call WowSetProp Using Win-Return dtFecha-H "text" texto-fecha

           if operadores-estado = "H" or = "h"
              Call WowSetProp Using Win-Return chEstado-H "Value" 1
           else
              Call WowSetProp Using Win-Return chEstado-H "Value" 0
           end-if
           Call WowSetProp Using Win-Return txtImporte-H "text"
           operadores-minuto-normal.

       mover-datos-al-registro.
           Call WowGetProp Using Win-Return txtNombre-H "text"
                operadores-razon-social

           Call WowGetProp Using Win-Return cbServicio-H "text"
                operadores-Servicio giving pos

           Call WowGetProp Using Win-Return cbServicio-H "ListIndex" pos
           Call AXSetIndexProp Using Win-Return cbServicio-H "ListData"
                item-seleccionado pos


           Call WowGetProp Using Win-Return chEstado-H "Value"
                operadores-estado

           if operadores-estado = 1
              move "h" to operadores-estado
           else
              move "i" to operadores-estado.

           Call WowGetProp Using Win-Return dtFecha-H "text" texto-fecha
           string texto-fecha(1:2)
                  texto-fecha(4:2)
                  texto-fecha(7:4)
                     delimited by size into fecha

           move fecha to operadores-fecha-inicio

           Call WowGetProp Using Win-Return txtImporte-H "text"
           operadores-minuto-normal.

      *
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
           COPY "menu.wpr".
           COPY "principal.wpr".

       END PROGRAM "CLIENTES-ABM".

