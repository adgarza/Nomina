10 'SAVE"NOMINA
20 ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '
30 ' NOMINA                                                            '
40 ' Sistema N¢mina Versi¢n 2 para el c lculo de pago de rayas a los   '
50 ' trabajadores de Brise¤o & Asociados con opci¢n de Impresi¢n  de   '
60 ' N¢minas y captura secuencial                                      '
70 ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '
80 'Inicializa
90  COMMON EMPRESA$, MODI, CAPTURA
100  FOR I=3 TO 10: KEY I,"": NEXT
110  KEY OFF: DEFINT A-Z: DEF SEG : SCREEN 0, 0, 0, 0: WIDTH 80
120 OPTION BASE 1
130 'Lectura Constantes Y Variables
140   DIM C(6, 9), P(2, 6), OP$(6), H$(6)
150   FALSE = FALSE: TRUE = NOT FALSE
160   MAXOP = 0'M ximo n£mero de opciones
170   CE = 0  'Color Elegido
180   OP = 0  'Opci¢n del men£
190   LINEA = 0'Booleano: False no es l¡nea, true=s¡ lo es
200   EMPRESA$ = ""'Cadena, contiene el nombre de la empresa
210   FA$ = CHR$(0) + "H": FI$ = CHR$(0) + "K"
220   FB$ = CHR$(0) + "P": FD$ = CHR$(0) + "M"
230   I = 0: N = 0: J = 0'Contadores
240   FOR I = 1 TO 6
250     FOR J = 1 TO 9
260       READ C(I, J) 'L‚e los colores
270   NEXT J, I
280   OPEN "R", 1, "NOMINA.INI"
290   IF LOF(1) = 0 THEN CLOSE : GOSUB 1860 ELSE CLOSE
300   OPEN "I", 1, "NOMINA.INI"
310     INPUT #1, EMPRESA$, CE
320   CLOSE
330 :
340 'Proceso del Programa Principal
350 FIN = FALSE
360 WHILE NOT FIN
370    LINEA = TRUE
380    OP$(1) = " Archivo ": OP$(2) = " Trabajador ": OP$(3) = " Rayas ": OP$(4) = " Colores ": OP$(5) = " Informaci¢n "
390    P(1, 1) = 1: P(2, 1) = 2: P(1, 2) = 1: P(2, 2) = 11: P(1, 3) = 1: P(2, 3) = 23: P(1, 4) = 1: P(2, 4) = 30: P(1,5)=1: P(2,5)=39
400    H$(1) = "Manejo de Datos y Salida del sistema": H$(2) = "Altas/Bajas/Modificaci¢n/Reportes de Trabajadores": H$(3) = "Elaboraci¢n/Modificaci¢n/Reportes de rayas": H$(4) = "Cambiar colores de presentaci¢n": H$(5)="Informaci¢n del sistema"
410    CADENT$ = "ATRCIatrci": MAXOP = 5
420    GOSUB 470
430    ON OP GOTO 1410, 1300, 1660, 1750,2010
440 WEND
450 END
460 :
470 'SUB MasterMen£
480    IF LINEA THEN GOSUB 900 ELSE GOSUB 1020
490    OP = 1
500    C1 = 5: C2 = 6: M$ = H$(OP): GOSUB 840: GOSUB 1160: T$ = ""
510    WHILE T$ <> CHR$(13)
520      GOSUB 1260 'Pulsa tecla
530      IF T$ = FB$ AND LINEA THEN T$ = CHR$(13): GOTO 590
540      IF T$ = FD$ OR T$ = FB$ THEN GOSUB 620'Avanza y acomoda
550      IF T$ = FI$ OR T$ = FA$ THEN GOSUB 690'Retrocede y acomoda
560      IF INSTR(CADENT$, T$) > 0 THEN GOSUB 760'Por letra, Localiza y acepta
570      IF T$ = CHR$(27) THEN RUN 10
580      IF T$ <> CHR$(13) THEN BEEP
590    WEND
600 RETURN
610 :
620 'SUB Flecha Arriba o derecha
630   C1 = 3: C2 = 4: GOSUB 840
640   OP = OP + 1
650   IF OP > MAXOP THEN OP = 1
660   C1 = 5: C2 = 6: M$ = H$(OP): GOSUB 840: GOSUB 1160
670 RETURN 590
680 :
690 'SUB Flecha Abajo o Izquierda
700   C1 = 3: C2 = 4: GOSUB 840
710   OP = OP - 1
720   IF OP < 1 THEN OP = MAXOP
730   C1 = 5: C2 = 6: M$ = H$(OP): GOSUB 840: GOSUB 1160
740 RETURN 590
750 :
760 'SUB ChecaLaCadena
770   C1 = 3: C2 = 4: GOSUB 840
780   OP = INSTR(CADENT$, T$)
790   IF OP > MAXOP THEN OP = OP - MAXOP
800   C1 = 5: C2 = 6: M$ = H$(OP): GOSUB 840: GOSUB 1160
810   T$ = CHR$(13)
820 RETURN
830 :
840 'SUB coloca/remueve la opci¢n
850   LOCATE P(1, OP), P(2, OP)
860   COLOR C(CE, C1), C(CE, C2)
870   PRINT OP$(OP);
880 RETURN
890 :
900 'SUB LineaSuperior
910   VIEW PRINT 2 TO 24
920   COLOR , C(CE, 2): CLS
930   LS$ = " "
940   FOR I = 1 TO MAXOP: LS$ = LS$ + OP$(I): NEXT
950   LS$ = LS$ + SPACE$(63 - LEN(LS$)) + " GMD N¢minas V2  "
960   VIEW PRINT
970   LOCATE 1, 1
980   COLOR C(CE, 3), C(CE, 4)
990   PRINT LS$;: COLOR C(CE,9),C(CE,6): LOCATE 1,64: PRINT " GMD N¢minas V2 "
1000 RETURN
1010 :
1020 'SUB CuadroAbajo
1030   LM = LEN(OP$(1))
1040   MASK$ = "º\" + SPACE$(LM - 2) + "\º"
1050   LOCATE P(1, 1) - 1, P(2, 1) - 1
1060   COLOR C(CE, 3), C(CE, 4)
1070   PRINT "É" + STRING$(LM, 205) + "»";
1080   FOR I = 1 TO MAXOP
1090     LOCATE P(1, I), P(2, I) - 1
1100     PRINT USING MASK$; OP$(I)
1110   NEXT
1120   LOCATE P(1, MAXOP) + 1, P(2, MAXOP) - 1
1130   PRINT "È" + STRING$(LM - 3, 205) + "Esc¼";
1140 RETURN
1150 :
1160 'SUB PresentaLineaAyuda
1170   LOCATE 25, 1
1180   COLOR C(CE, 3), C(CE, 4)
1190   IF LEN(M$) >= 80 - (LEN(EMPRESA$) + 3) THEN MASK$ = SPACE$(80) ELSE MASK$ = SPACE$(80 - (LEN(EMPRESA$) + 3)) + "³ " + EMPRESA$ + " "
1200   PRINT MASK$;
1210   LOCATE 25, 2
1220   COLOR C(CE, 9)
1230   PRINT M$;
1240 RETURN
1250 :
1260 'SUB PulsaTecla
1270   T$ = INKEY$: IF T$ = "" THEN 1270
1280 RETURN
1290 :
1300 'SUB Trabajadores
1310    LINEA = FALSE
1320    OP$(1) = " Altas      ": OP$(2) = " Bajas      ": OP$(3) = " Modificar  ": OP$(4) = " Visualizar ": OP$(5) = " Imprimir   "
1330    P(1, 1) = 3: P(2, 1) = 11: P(1, 2) = 4: P(2, 2) = 11: P(1, 3) = 5: P(2, 3) = 11: P(1, 4) = 6: P(2, 4) = 11: P(1, 5) = 7: P(2, 5) = 11
1340    H$(1) = "Dar de Alta a trabajadores": H$(2) = "Dar de Baja a trabajadores": H$(3) = "Modificar datos capturados": H$(4) = "Visualizar trabajadores activos": H$(5) = "Imprimir trabajadores activos"
1350    CADENT$ = "ABMVIabmvi": MAXOP = 5: GOSUB 470
1360    ON OP GOTO 1380, 1380, 1380, 1380, 1380
1370    RUN 10
1380    CHAIN "ALTAS" 'Altas
1390 'END
1400 :
1410 'SUB Archivo
1420    LINEA = FALSE
1430    OP$(1) = " Semanas     ": OP$(2) = " Inicializar ": OP$(3) = " Exportar    ": OP$(4) = " iMportar    ": OP$(5) = " saLida      "
1440    P(1, 1) = 3: P(2, 1) = 2: P(1, 2) = 4: P(2, 2) = 2: P(1, 3) = 5: P(2, 3) = 2: P(1, 4) = 6: P(2, 4) = 2: P(1, 5) = 7: P(2, 5) = 2
1450    H$(1) = "Establece las semanas del a¤o en proceso": H$(2) = "Elimina toda la informaci¢n de trabajadores": H$(3) = "Copia toda la informaci¢n capturada a A:"
1460    H$(4) = "Copia toda la informaci¢n completa de A:": H$(5) = "Fin de la ejecuci¢n del sistema": CADENT$ = "SIEMLsieml": MAXOP = 5: GOSUB 470
1470    ON OP GOTO 1650,1500,1480,1480,1490
1480 RUN 10
1490 COLOR 7, 0: CLS : SYSTEM
1500 'SUB Inicializa
1510   COLOR C(CE,1),C(CE,2): VIEW PRINT 2 TO 24: CLS: VIEW PRINT
1520   M$="­Cuidado! Esto borrar  toda la informaci¢n de sus trabajadores": GOSUB 1160
1530   COLOR C(CE,7),C(CE,8): LOCATE 10,1: PRINT " ­Precauci¢n! "
1540   COLOR C(CE,9),C(CE,2): LOCATE 12,1: PRINT "Esto borrar  toda la informaci¢n capturada (N¢minas y Semanas) de su disco y"
1550   PRINT "no deber  llevarse a cabo si no ha utilizado la opci¢n ";:COLOR C(CE,5),C(CE,6): PRINT "Exportar";:COLOR C(CE,9),C(CE,2):PRINT " del men£ ";:COLOR C(CE,5),C(CE,6):PRINT "Archivo";
1560   COLOR C(CE,9),C(CE,2):PRINT "Si no lo ha hecho oprima cualquier tecla para abortar o `S' para proceder..."
1570   PRINT :PRINT "¨Proceder con la inicializaci¢n? (S/N)"
1580   GOSUB 1260
1590   IF T$<>"S" AND T$<>"s" THEN RUN 10
1600   M$="En proceso de inicializaci¢n": GOSUB 1160
1610   KILL "nomina.??"
1620   KILL "semanas.dat"
1630  RUN 10
1640 :
1650  CHAIN "semanas"
1660 'SUB Rayas
1670    LINEA = FALSE
1680    OP$(1) = " Capturar   ": OP$(2) = " Modificar  ": OP$(3) = " Visualizar ": OP$(4) = " Imprimir   "
1690    P(1, 1) = 3: P(2, 1) = 23: P(1, 2) = 4: P(2, 2) = 23: P(1, 3) = 5: P(2, 3) = 23: P(1, 4) = 6: P(2, 4) = 23
1700    H$(1) = "Capturar nueva n¢mina": H$(2) = "Modificar n¢mina anterior": H$(3) = "Visualizar N¢mina indicada": H$(4) = "Imprimir N¢mina Indicada"
1710    CADENT$ = "CMVIcmvi": MAXOP = 4: GOSUB 470
1720    IF OP=>1 AND OP<=3 THEN CHAIN "Rayas" ELSE IF OP=4 THEN CHAIN "imprime"
1730 RUN 10
1740 :
1750 'SUB CambiarColores
1760   LINEA = FALSE
1770   OP$(1)=" Monocromo    ":OP$(2)=" Aqua         ":OP$(3)=" paStel       ": OP$(4)=" Esmeralda    ":OP$(5)=" Rub¡         ":OP$(6)=" Pink Panther "
1780   H$(1) = "Para monitores Monocrom ticos ¢ LCD": H$(2) = "Refrescante Cyan sobre azul": H$(3) = "Colores Pastel": H$(4) = "Verde en primer plano": H$(5) = "Rojo en primer plano": H$(6)="Tonos en Rosa"
1790   P(1, 1) = 3: P(2, 1) = 30: P(1, 2) = 4: P(2, 2) = 30: P(1, 3) = 5: P(2, 3) = 30: P(1, 4) = 6: P(2, 4) = 30: P(1, 5) = 7: P(2, 5) = 30: P(1,6)=8:P(2,6)=30
1800   CADENT$ = "MASERPmaserp": MAXOP = 6: GOSUB 470
1810   IF OP = 0 THEN 1840
1820   CE = OP
1830   GOSUB 1950
1840 RUN 10
1850 :
1860 'SUB PedirDatos
1870    CE = 1
1880    COLOR 7, 0
1890    CLS
1900    LINE INPUT "Nombre de la empresa: "; EMPRESA$
1910    IF EMPRESA$ = "" OR LEN(EMPRESA$) > 37 THEN BEEP: PRINT "!Nombre incorrecto­ Debe ser hasta de 37 caracteres.": GOTO 1900
1920    GOSUB 1950  'GrabaINI
1930 RETURN
1940 :
1950 'SUB GrabaINI
1960   OPEN "O", 3, "NOMINA.INI"
1970     WRITE #3, EMPRESA$, CE
1980   CLOSE #3
1990 RETURN
2000 :
2010 'SUB Informaci¢n
2020   LINEA = FALSE
2030   OP$(1)=" Acerca de... "
2040   H$(1) = "Datos del Sistema GMD N¢mina Versi¢n 2"
2050   P(1, 1) = 3: P(2, 1) = 39
2060   CADENT$ = "Aa": MAXOP = 1: GOSUB 470
2070   IF OP = 0 THEN 2090
2080   GOSUB 2110 'Presentar Informaci¢n del Programa
2090 RUN 10
2100 :
2110 'SUB Presenta Informaci¢n del Programa
2120   COLOR C(CE,1),C(CE,2)
2130   VIEW PRINT 2 TO 24
2140   CLS
2150   VIEW PRINT
2160   COLOR C(CE,3),C(CE,4)
2170   LOCATE 7,14
2180   PRINT "É"+STRING$(50,205)+"»"
2190   FOR I=8 TO 16
2200     LOCATE I,14
2210     PRINT "º"+SPACE$(50)+"º"
2220   NEXT
2230   LOCATE 17,14
2240   PRINT "È"+STRING$(50,205)+"¼"
2250   M$="GMD Microsistemas "+CHR$(34)+"N¢mina de Rayas Semanales"+CHR$(34)
2260   COLOR C(CE,9)
2270   LOCATE 8,40-LEN(M$)\2: PRINT M$
2280   COLOR C(CE,5): M$="Versi¢n 2": LOCATE 10,40-LEN(M$)\2: PRINT M$
2290   COLOR C(CE,5),C(CE,6): M$="Ing. A. David Garza Mar¡n": LOCATE 13,40-LEN(M$)\2: PRINT M$
2300   COLOR C(CE,9),C(CE,4): M$="Julio 19 de 1992 MicroSoft gw-BASIC Versi¢n 4.00": LOCATE 14,40-LEN(M$)\2: PRINT M$
2310   COLOR C(CE,7),C(CE,8): M$="Pulse una tecla para continuar": LOCATE 16,40-LEN(M$)\2: PRINT M$
2320   M$="Pulse una tecla para seguir": GOSUB 1160: GOSUB 1260
2330 RETURN
2340 :
2350 'DATOS de los colores del monitor
2360 '    PPlano Splano Pmenu Smenu Pelec Selec Pcursor Scursor Pmensaje
2370 DATA   7   ,   0  ,  0  ,  7  , 15  ,  0  ,   0   ,   7   ,  15
2380 DATA  14   ,   1  ,  0  ,  3  , 15  ,  6  ,  15   ,   3   ,  14
2390 DATA   1   ,   7  , 15  ,  3  , 15  ,  1  ,  11   ,   7   ,  14
2400 DATA   0   ,   7  , 14  ,  2  , 15  ,  0  ,  15   ,   2   ,  12
2410 DATA   0   ,   7  , 14  ,  4  , 15  ,  0  ,  15   ,   4   ,  15
2420 DATA   8   ,   7  , 15  ,  5  , 13  ,  0  ,  15   ,   5   ,  14
