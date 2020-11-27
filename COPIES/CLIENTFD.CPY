      ******************************************************************
      ******        ARCHIVO DE CLIENTES NUEVO         *********************
      ******************************************************************
       FD  CLIENTE RECORD CONTAINS 1000 CHARACTERS
                   LABEL RECORD STANDARD.
       01  A101-REG.
           03 A101-CLAVE.
              05 A101-SUC              PIC 99.
              05 A101-NRO              PIC 9(4).
           03 A101-CLIENTE  REDEFINES A101-CLAVE PIC 9(6).
           03 A101-DISTRITO            PIC 99.
           03 A101-ZONA-VTA            PIC 99.
           03 A101-SUBZONA-VTA         PIC 99.
           03 A101-VENDEDOR            PIC 999.
           03 A101-NOMBRE              PIC X(35).
           03 A101-DOMICILIO           PIC X(35).
           03 A101-LOCALIDAD           PIC X(20).
           03 A101-CODIGO-POSTAL       PIC X(8).
      * SE SACA 2 DE COD-POSTAL
      *       Valores: 1000-9999
           03 A101-PROVINCIA           PIC 99.
      *       Valores: 01-24 Segun Tabla copy WPCIAS.CPY
           03 A101-TEL-1               PIC X(12).
           03 A101-TEL-2               PIC X(12).
           03 A101-FAX                 PIC X(12).
      * SE AGRANDA 2 A TEL-1
           03 A101-COD-VENTA           PIC XXX.
           03 A101-COD-IVA             PIC 9.
      *       Valores:   '0'  Inscripto
      *                  '1'  No Inscripto
      *                  '2'  No Responsable
      *                  '3'  Exento
      *                  '4'  Consumidor Final
      *                  '5'  exento importacion
      *                  '6'  exento promocion
      *                  '7'  monotributista
      *                  '8'  no categorizado
           03  A101-NRO-CUIT            PIC 9(14).
      * SAQUE INGR BRUTOS 32 CARACT.                                  
           03  A101-FECHA-ALTA          PIC 9(8).
      * FORMATO DD/MM/AAAA
      * LE AGREGO 2 A FECHA ALTA
           03  A101-BLOQUEO             PIC X.
             88  A101-BLOQUEADA-SI  VALUE '1'.
      *        Valores:   '0'  Normal
      *                   '1'  Cuenta Bloqueada
           03  A101-ESPECIAL            PIC 99.
      *        Codigo de Actividad o Especialidad
           03  A101-COPIAS              PIC 9.
      *        Cantidad de Copias de la Factura
           03  A101-MARCA-CREDITO       PIC X.
      *        Valores:   '0'  Sin Credito
      *                   '1'  Credito Otorgado
      *                   '9'  No Otorgar Credito
           03  A101-FECHA-CREDITO       PIC 9(8).
      * LE AGRE 4 A FECHA CREDITO (FORMATO AAAA/MM/DD)
           03  A101-MONTO-CREDITO       PIC 9(11)V99.
           03  A101-CREDITO-UTILIZADO   PIC 9(11)V99.
           03  A101-SALDOS-MAES.
              05 A101-CAMPOS-SALDOS OCCURS 4 TIMES.
      *        Ocurrencia:'1'  Pesos
      *                   '2'  Pesos      confidenciales
      *                   '3'  Dolares
      *                   '4'  Dolares    confidenciales
             07  A101-SALDO               PIC S9(14)V99.
             07  A101-SALDO-RESU          PIC S9(14)V99.
             07  A101-HOJA-RESU           PIC 999.
             07  A101-FECHA-ULT-RESU      PIC 9(6).
      * FORMATO DDMMAAA
             07  A101-FECHA-ULT-FACT      PIC 9(6).
      * FORMATO DDMMAAA
             07  A101-PEND-REMITOS        PIC S9(15)V99.
             07  A101-PEND-PEDIDOS        PIC S9(15)V99.
             07  A101-MONTO-VALORES       PIC S9(15)V99.
             07  A101-MONTO-DOCUMEN       PIC S9(15)V99.
           03    A101-CLAVE-IDENT.
            05   A101-TIPO-DOC            PIC XXX.
      *          Valores:  'CI ' Cedula de Identidad
      *                    'CM ' Cedula Militar
      *                    'DNI' Documento Nacional de Identidad
      *                    'LC ' Libreta Civica
      *                    'LE ' Libreta de Enrolamiento
      *                    'OTR' Otros
      *                    'PP ' Pasaporte
            05   A101-NRO-DOC             PIC 9(8).
           03    A101-CLAVE-ACTUAL.
                 05 A101-FECHA-ACTUALIZ   PIC 9(8).
      * FORMATO AAAA/MM/DD
      * SE AGRANDA 2
      **              indica la fecha de ultimo movimiento que modifico
      **              algun dato, sirve para luego actualizar los datos
      **              del sistema central y/o viceversa
      *    03  A101-RESOL-3337-91       PIC 9.
      * SE SACA 1
      **             0= No recarga   porcentaje segun parametros y %
      **             1= Si recarga   porcentaje segun parametros y %
      *
           03    A101-COD-TRANSPOR           PIC 999.
              88  A101-TRANSPORTE-NO  VALUE ZEROS.
      **                 Codigo de transporte (archivo TRANSPOR)
           03    A101-MARCA-DISTRITO-ABASTO  PIC X.
      **   VALORES 'Y' O 'N'
      **         MARCA PARA IMPUESTO ABASTO PARA MAGGI SEGUN DISTRITO
      **         PARTIDO
           03 A101-LISTA-PRECIOS    PIC 9(2).

      *    ----
      *    ---- esta area es para ser utilizada segun las necesidades
      *    ----                 de cada empresa
      *    ----          no utilizados en la pantalla de alta
      *    ----
           03 A101-AREA-REUTILIZABLE.
              04 A101-DATOS-TARJETA.
                 05 A101-TARJETA          PIC 999.
                 05 A101-TARJETA-NOMBRE   PIC X(20).
                 05 A101-TARJETA-NRO      PIC 9(16).
                 05 A101-TARJETA-VTO.
                    07 A101-TARJETA-MES      PIC 99.
                    07 A101-TARJETA-YEAR     PIC 9999.
                 05 A101-TARJETA-CUOTAS      PIC 99.
                 05 A101-TARJETA-ABREVIATURA PIC X(6).
                 05 A101-TARJETA-RECARGO     PIC 99V99.
              04 FILLER                PIC X(21).
      *    ----
      *    ---- fin del area para ser utilizada segun las necesidades
      *    ----                 de cada empresa
      *    ----          no utilizados en la pantalla de alta
      *    ----
           
           03    A101-COMPENSA          PIC X.
                    88  A101-COMPENSA-SI  VALUE 'Y'.
                    88  A101-COMPENSA-NO  VALUE 'N'.

           03    A101-CONVENIO-MULTI          PIC 9.
                 88  A101-MULTILATERAL-SI  VALUE 1.
      **   VALORES 1= SI   0= NO
      **         INDICA SI ES CONVENIO MULTILATERAL EN INGRESOS BRUTOS

           03 A101-CTA-IMPUTA            PIC 9(4).
      **-        nro de Cta de imputacion si FECH-70-CLIE-IMPUTA = 'Y'
      **-

      *    ----
      *    ---- a partir de aqui son datos comunes usados en el panel
      *    ----
           03  A101-ESTADO-DEUDA       PIC 9.
      *                     0 = normal      1 = Gestion Cobranza
      *                     7 = Concurso    9 = Incobrable

           03  A101-LUGAR-ENTREGA      PIC X.
              88 A101-LUGAR-ENTREGA-SI    VALUE 'Y'.
              88 A101-LUGAR-ENTREGA-NO    VALUE 'N'.

      **   VALORES 'Y' O 'N'
      **         MARCA PARA SABER SI TIENE UN LUGAR DE ENTREGA FIJO
      **         DIFERENTE AL DOMICILIO PRINCIPAL

           03  A101-COBRADOR           PIC 999.

           03  A101-CLISUCUR           PIC X.
             88 A101-CONSUCURSAL  VALUE 'Y', 'y'.
             88 A101-SINSUCURSAL  VALUE  'N', 'n'.
      **   VALORES 'Y' O 'N'
      **         MARCA PARA SABER SI ESTE CLIENTE TIENE SUCURSALES
      **         SE USA EN LA FACTURACION
          03 A101-CONTACTO         PIC X(60).
          03 A101-EMAIL            PIC X(50).
          03 FILLER                PIC X(86).
