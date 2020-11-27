       IDENTIFICATION DIVISION.
       PROGRAM-ID. "CLIENTES-ABM".

      *
WOWBGN*
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      ******************************************************************
      * Beginning of editable Configuration Section.
      *   You can edit code between here and the next marker.
WOWCOD* WOWCFS
       SOURCE-COMPUTER. MULTIPLATAFORMA.
       OBJECT-COMPUTER. MULTIPLATAFORMA
           PROGRAM COLLATING SEQUENCE IS MAYUS-MINUS.
WOWCOD*
      * End of editable Configuration Section.
      ******************************************************************
       SPECIAL-NAMES.
      ******************************************************************
      * Beginning of editable Special-Names.
      *   You can edit code between here and the next marker.
WOWCOD* WOWSPN
           ALPHABET MAYUS-MINUS IS
               1 THRU 65,
               'A' ALSO 'a', 'B' ALSO 'b', 'C' ALSO 'c', 'D' ALSO 'd',
               'E' ALSO 'e', 'F' ALSO 'f', 'G' ALSO 'g', 'H' ALSO 'h',
               'I' ALSO 'i', 'J' ALSO 'j', 'K' ALSO 'k', 'L' ALSO 'l',
               'M' ALSO 'm', 'N' ALSO 'n', 'O' ALSO 'o', 'P' ALSO 'p',
               'Q' ALSO 'q', 'R' ALSO 'r', 'S' ALSO 's', 'T' ALSO 't',
               'U' ALSO 'u', 'V' ALSO 'v', 'W' ALSO 'w', 'X' ALSO 'x',
               'Y' ALSO 'y', 'Z' ALSO 'z',  92 THRU 97, 124 THRU 128.
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
           copy clientsd.cpy.
WOWCOD*
      * End of editable File-Control.
      ******************************************************************

       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      * Beginning of editable File Section.
      *   You can edit code between here and the next marker.
WOWCOD* WOWFLS
           copy clientfd.cpy.
WOWCOD*
      * End of editable File Section.
      ******************************************************************

       WORKING-STORAGE SECTION.
      ******************************************************************
      * Beginning of editable Working-Storage Section.
      *   You can edit code between here and the next marker.
WOWCOD* WOWPWS
       77  mi-codigo                  pic 9(06).
       77  mi-opcion                  pic x(01).
       77  eof-cliente                pic x(01).
       77  existe-cliente             pic x(01).
       77  error-cliente              pic x(01).
       77  texto-fecha                pic x(10).
       77  texto                      pic x(80).
       01  fecha                      pic 9(08).
       01  ff redefines fecha.
           02 dia                     pic 9(02).
           02 mes                     pic 9(02).
           02 ano                     pic 9(04).
       77  WY-STATUS-GRL              pic x(02).
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
           open i-o cliente.
WOWCOD*
      * End of editable Program-Initialization.
      ******************************************************************

       PROGRAM-SHUTDOWN SECTION.

       PROGRAM-SHUTDOWN-PARAGRAPH.
      ******************************************************************
      * Beginning of editable Program-Shutdown.
      *   You can edit code between here and the next marker.
WOWCOD* WOWPPS
           close cliente.
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
                  perform grabar-cliente
              when "b"
                  perform mover-datos-al-registro
                  perform borrar-cliente
              when "m"
                  perform mover-datos-al-registro
                  perform regrabar-cliente
           end-evaluate.

       CARGAR-REGISTRO.
           Call WowGetProp Using Win-Return codigo-H "text" mi-codigo
           initialize a101-reg
           move mi-codigo to a101-cliente

           perform leer-cliente
           if existe-cliente = "n" and mi-opcion not = "a"
              Move all 'N' to Message-Box-Flags
              Set Mb-OKCancel Mb-IconHand To True
              Call WowMessageBox Using Win-Return principal-H
                   "No existe el cliente" "Error" Message-Box-Flags
           else if existe-cliente = "s" and mi-opcion = "a"
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
                a101-nombre
      *     Call WowSetProp Using Win-Return cbServicio-H "text"
      *          "Telefonia"
           string a101-fecha-alta(1:2)  "/"
                  a101-fecha-alta(3:2) "/"
                  a101-fecha-alta(5:)
                      delimited by size into texto-fecha
           Call WowSetProp Using Win-Return dtFecha-H "text" texto-fecha

           if a101-lugar-entrega = "y"
              Call WowSetProp Using Win-Return chEstado-H "Value" 1
           else
              Call WowSetProp Using Win-Return chEstado-H "Value" 0
           end-if
           Call WowSetProp Using Win-Return txtImporte-H "text"
           a101-monto-credito.

       mover-datos-al-registro.
           Call WowGetProp Using Win-Return txtNombre-H "text"
                a101-nombre

      *    Call WowGetProp Using Win-Return cbServicio-H "text"
      *         cliente-Servicio

           Call WowGetProp Using Win-Return chEstado-H "Value"
                a101-lugar-entrega

           if a101-lugar-entrega = "1"
              move "Y" to a101-lugar-entrega
           else
              move "N" to a101-lugar-entrega.

           Call WowGetProp Using Win-Return dtFecha-H "text" texto-fecha
           string texto-fecha(1:2)
                  texto-fecha(4:2)
                  texto-fecha(7:4)
                     delimited by size into fecha

           move fecha to a101-fecha-alta

           Call WowGetProp Using Win-Return txtImporte-H "text"
           a101-monto-credito.

      *
       leer-cliente.
           move 's' to existe-cliente.
           read cliente
                        invalid key
                                    move 'n' to existe-cliente.
       start-cliente.
           move 'n' to eof-cliente.
           start cliente
                       key not < a101-clave
                             invalid key
                                        move 's' to eof-cliente.
       leer-cliente-next.
           move 'n' to eof-cliente.
           read cliente next
                           at end
                                 move 's' to eof-cliente.
       regrabar-cliente.
           move 'n' to error-cliente.
           rewrite a101-reg
                             invalid key
                                        move 's' to error-cliente.
       borrar-cliente.
           move 'n' to error-cliente.
           delete cliente
                             invalid key
                                        move 's' to error-cliente.
       grabar-cliente.
           move 'n' to error-cliente.
           write a101-reg
                             invalid key
                                        move 's' to error-cliente.
WOWCOD*
      * End of editable Procedure Division.
      ******************************************************************

      *
WOWDNE*
      *
           COPY "menu.wpr".
           COPY "principal.wpr".

       END PROGRAM "CLIENTES-ABM".

