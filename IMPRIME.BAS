10 'SAVE "IMPRIME"
20 ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '
30 ' IMPRIME                                                         '
40 ' Programa para imprimir las rayas de los trabajadores. Puede ser '
50 ' ejecutado por separado. GMD N╒minas V2                          '
60 ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '
70 'Inicializa los datos
80    DEFINT A-Z
90    OPTION BASE 1: DIM C(6, 9),DIN!(5),T!(5)
100    FOR I = 1 TO 6
110      FOR J = 1 TO 9
120        READ C(I, J)
130    NEXT J, I
140    OPEN "R",1,"SEM",2: FIELD #1,2 AS SEM$
150    IF LOF(1)/2=0 THEN CLOSE: KILL "SEM": GOTO 170
160    GET #1,1: SEM=CVI(SEM$): CLOSE: FLAG=-1: GOTO 180
170    IF LEN(EMPRESA$)=0 THEN RUN "NOMINA"
180    OPEN "I", 1, "nomina.ini"
190      INPUT #1, EMPRESA$, CE
200    CLOSE
210   IF NOT FLAG THEN VIEW PRINT 2 TO 24: COLOR C(CE,1),C(CE,2):CLS:VIEW PRINT
220   M$=" Impresi╒n ": GOSUB 2610
230   FOR I=1 TO 5: READ DIN!(I): NEXT
240   FALSE = FALSE: TRUE = NOT FALSE
250   CAMPO = 1
260   MASKN$ = "\                            \"
270   MASKP$ = "\        \"
280   MASKD$ = "$$###,###.##"
290   MASKO$ = "###"
300   IF NOT FLAG THEN GOSUB 1570
310 :
320 'L┌e la semana correspondiente
330  M$="Leyendo la semana correspondiente": GOSUB 2510
340  OPEN "R",1,"SEMANAS.DAT": IF LOF(1)=0 THEN CLOSE: M$="No se han capturado las semanas correspondientes. Pulse una tecla.": GOSUB 2510: GOSUB 2470: IF FLAG THEN CHAIN "RAYAS",90 ELSE RUN "NOMINA"
350  CLOSE
360  OPEN "I",1,"SEMANAS.DAT"
370  FOR I=1 TO SEM
380     INPUT #1,SEMANA$
390  NEXT
400  CLOSE
410  ARCH$="NOMINA."+MID$(STR$(SEM),2)
420  OPEN "R",1,ARCH$,70
430  FIELD #1,30 AS NOMBRE$,10 AS PUESTO$,4 AS SUELDO$,4 AS COMP$,2 AS DIAS$,2 AS HE$,2 AS HS$,4 AS PAGS$,4 AS PAGC$,4 AS PAGE$,4 AS TOTAL$
440  IF LOF(1)/70=0 THEN CLOSE: KILL ARCH$: M$="No se ha capturado esta n╒mina. Pulse una tecla": GOSUB 2510: GOSUB 2470: RUN "NOMINA"
450 'Pide el pi┌ de p═gina
460   GOSUB 1650 'Pi┌ de p═gina e impresiones
470 'Proceso de Impresi╒n
480   M$="Prepare la impresora y pulse una tecla. Esc Aborta": GOSUB 2510
490   WIDTH LPRINT 100
500   'ON ERROR GOTO 2390
510   FOR I=1 TO 5
520     ON I GOSUB 580,910,1240,1740
530   NEXT
540   CLOSE
550   IF FLAG THEN CHAIN "rayas",90 ELSE RUN "nomina"
560 END
570 :
580 'SUB ImprimeSueldosNormales
590  FOR COPIAS=1 TO NUMCOP
600    M$="Imprimiendo Sueldos ("+STR$(COPIAS)+" de"+STR$(NUMCOP)+")": GOSUB 2510
610    L=0
620    GOSUB 740 'Cabecera de Sueldos Normales
630    MASK$="╨###Ё\                        \Ё"+MASKP$+"Ё### Ё"+MASKD$+"Ё                  ╨"
640    FOR REG1=1 TO LOF(1)/70
650        GET #1,REG1
660        LPRINT USING MASK$;REG1,NOMBRE$,PUESTO$,CVI(DIAS$),CVS(PAGS$)
670        LPRINT "гдддеддддддддддддддддддддддддддеддддддддддеддддеддддддддддддедддддддддддддддддд╤"
680        L=L+2: IF L=>60 THEN GOSUB 840: L=0: GOSUB 740
690    NEXT REG1
700    GOSUB 840
710  NEXT COPIAS
720 RETURN
730 :
740 'SUB CabeceradeSueldosNormales
750   GOSUB 2190 'Cabeceras en general
760   M$="Cuotas Salariales del "+SEMANA$
770   GOSUB 2310 'Cabecera del tipo de pago
780   LPRINT "гдддеддддддддддддддддддддддддддбддддддддддбддддбддддддддддддбдддддддддддддддддд╤"
790   LPRINT "╨N╖ ЁNombre del Trabajador     ЁPuesto    ЁDiasЁ Sueldo     Ё                  ╨"
800   LPRINT "гдддеддддддддддддддддддддддддддеддддддддддеддддеддддддддддддедддддддддддддддддд╤"
810   L=L+3
820 RETURN
830 :
840 'SUB Pi┌deSueldosNormales
850   LPRINT "хмммоммммммммммммммммммммммммммоммммммммммоммммоммммммммммммомммммммммммммммммм╪"
860   M$=SPACE$(40-LEN(PIEPAG$)\2)+PIEPAG$
870   LPRINT M$
880   LPRINT CHR$(12)
890 RETURN
900 :
910 'SUB ImprimeCompensaciones
920  FOR COPIAS=1 TO NUMCOP
930    M$="Enviando datos de Compensaciones ("+STR$(COPIAS)+" de"+STR$(NUMCOP)+")": GOSUB 2510
940    L=0
950    GOSUB 1070 'Cabecera de Compensaciones
960    MASK$="╨###Ё\                        \Ё"+MASKP$+"Ё### Ё"+MASKD$+"Ё                  ╨"
970    FOR REG1=1 TO LOF(1)/70
980        GET #1,REG1
990        LPRINT USING MASK$;REG1,NOMBRE$,PUESTO$,CVI(DIAS$),CVS(PAGC$)
1000        LPRINT "гдддеддддддддддддддддддддддддддеддддддддддеддддеддддддддддддедддддддддддддддддд╤"
1010        L=L+2: IF L=>60 THEN GOSUB 1170: L=0: GOSUB 1070
1020    NEXT REG1
1030    GOSUB 1170
1040  NEXT COPIAS
1050 RETURN
1060 :
1070 'SUB CabeceradeCompensaciones
1080   GOSUB 2190 'Cabeceras en general
1090   M$="Ayuda Alimentaria del "+SEMANA$
1100   GOSUB 2310 'Cabecera del tipo de pago
1110   LPRINT "гдддеддддддддддддддддддддддддддбддддддддддбддддбддддддддддддбдддддддддддддддддд╤"
1120   LPRINT "╨N╖ ЁNombre del Trabajador     ЁPuesto    ЁDiasЁ Bono       Ё                  ╨"
1130   LPRINT "гдддеддддддддддддддддддддддддддеддддддддддеддддеддддддддддддедддддддддддддддддд╤"
1140   L=L+3
1150 RETURN
1160 :
1170 'SUB Pi┌deCompensaciones
1180   LPRINT "хмммоммммммммммммммммммммммммммоммммммммммоммммоммммммммммммомммммммммммммммммм╪"
1190   M$=SPACE$(40-LEN(PIEPAG$)\2)+PIEPAG$
1200   LPRINT M$
1210   LPRINT CHR$(12)
1220 RETURN
1230 :
1240 'SUB ImprimeAnticipos
1250  FOR COPIAS=1 TO NUMCOP
1260    M$="Enviando datos de Anticipos ("+STR$(COPIAS)+" de"+STR$(NUMCOP)+")": GOSUB 2510
1270    L=0
1280    GOSUB 1400 'Cabecera de Anticipos
1290    MASK$="╨###Ё\                        \Ё### Ё### Ё"+MASKD$+"Ё                        ╨"
1300    FOR REG1=1 TO LOF(1)/70
1310        GET #1,REG1
1311        IF CVS(PAGE$)=0 THEN 1350
1320        LPRINT USING MASK$;REG1,NOMBRE$,CVI(HE$),CVI(HS$),CVS(PAGE$)
1330        LPRINT "гдддеддддддддддддддддддддддддддеддддеддддеддддддддддддедддддддддддддддддддддддд╤"
1340        L=L+2: IF L=>60 THEN GOSUB 1500: L=0: GOSUB 1400
1350    NEXT REG1
1360    GOSUB 1500
1370  NEXT COPIAS
1380 RETURN
1390 :
1400 'SUB CabeceradeCompensaciones
1410   GOSUB 2190 'Cabeceras en general
1420   M$="Anticipos de la semana del "+SEMANA$
1430   GOSUB 2310 'Cabecera del tipo de pago
1440   LPRINT "гдддеддддддддддддддддддддддддддбддддбддддбддддддддддддбдддддддддддддддддддддддд╤"
1450   LPRINT "╨N╖ ЁNombre del Trabajador     ЁH.E.ЁH.X.Ё Anticipo   Ё                        ╨"
1460   LPRINT "гдддеддддддддддддддддддддддддддеддддеддддеддддддддддддедддддддддддддддддддддддд╤"
1470   L=L+3
1480 RETURN
1490 :
1500 'SUB Pi┌deAnticipos
1510   LPRINT "хмммоммммммммммммммммммммммммммоммммоммммоммммммммммммомммммммммммммммммммммммм╪"
1520   M$=SPACE$(40-LEN(PIEPAG$)\2)+PIEPAG$
1530   LPRINT M$
1540   LPRINT CHR$(12)
1550 RETURN
1560 :
1570 'SUB PideSemana
1580   M$="Semana a Imprimir (1 a 53)": GOSUB 2510
1590   GOSUB 2720  'EditaTexto
1600   IF CADENT$="" THEN RUN "Nomina"
1610   SEM=VAL(CADENT$)
1620   IF SEM<1 OR SEM>53 THEN M$="Tecl┌e un valor de 1 a 53":GOSUB 2510:T$="":GOTO 1590
1630 RETURN
1640 :
1650 'sub Pi┌DeP═ginaeImpresiones
1660    M$="Tecl┌e lo que dir═ el pi┌ de p═gina": GOSUB 2510
1670    T$="": GOSUB 2720 'EditaTexto
1680    PIEPAG$=CADENT$: IF PIEPAG$=CHR$(27) THEN IF FLAG THEN CHAIN "rayas",90 ELSE RUN "nomina"
1690    M$="Tecl┌e el nёmero de impresiones (Default 1)": GOSUB 2510
1700    T$="": GOSUB 2720 'EditaTexto
1701    IF CADENT$=CHR$(27) THEN IF FLAG THEN CHAIN "rayas",90 ELSE RUN "nomina"
1710    NUMCOP=VAL(CADENT$): IF NUMCOP=0 THEN NUMCOP=1
1720 RETURN
1730 :
1740 'SUB CalculaDesglose
1750    M$="Enviando datos del Desglose de Moneda": GOSUB 2510
1760    L=0
1770    GOSUB 1910 'Cabecera de Compensaciones
1780         MASK$="╨###Ё\                                     \Ё###Ё###Ё###Ё###Ё###Ё$$#,###,###.##╨"
1790    FOR REG1=1 TO LOF(1)/70
1800        GET #1,REG1
1810        GOSUB 2080
1820        LPRINT USING MASK$;REG1,NOMBRE$,P(5),P(4),P(3),P(2),P(1),T1!
1830        LPRINT "гдддедддддддддддддддддддддддддддддддддддддддедддедддедддедддедддедддддддддддддд╤"
1840        L=L+2: IF L=>60 THEN GOSUB 2010: L=0: GOSUB 1910
1850    NEXT REG1
1860    LPRINT USING MASK$;0,"T O T A L",T(5),T(4),T(3),T(2),T(1),TT!
1870    LPRINT "хмммомммммммммммммммммммммммммммммммммммммммомммомммомммомммомммомммммммммммммм╪"
1880    LPRINT CHR$(12)
1890 RETURN
1900 :
1910 'SUB CabeceradeDesglose
1920   GOSUB 2190 'Cabeceras en general
1930   M$="Desglose de Moneda del "+SEMANA$
1940   GOSUB 2310 'Cabecera del Desglose
1950   LPRINT "гдддбдддддддддддддддддддддддддддддддддддддддбдддбдддбдддбдддбдддбдддддддддддддд╤"
1960   LPRINT "╨N╖ ЁNombre del Trabajador                  Ё50MЁ20MЁ10MЁ 5MЁ 1MЁ   Total      ╨"
1970   LPRINT "гдддедддддддддддддддддддддддддддддддддддддддедддедддедддедддедддедддддддддддддд╤"
1980   L=L+3
1990 RETURN
2000 :
2010 'SUB Pi┌deDesglose
2020   LPRINT "хмммомммммммммммммммммммммммммммммммммммммммомммомммомммомммомммомммммммммммммм╪"
2030   M$=SPACE$(40-LEN(PIEPAG$)\2)
2040   LPRINT M$
2050   LPRINT CHR$(12)
2060 RETURN
2070 :
2080 'SUB C═lculo del desglose
2090  DINERO!=CVS(PAGS$)+CVS(PAGE$)
2100  TT!=TT!+DINERO!
2110  T1!=DINERO!
2120  FOR CALC=5 TO 1 STEP -1
2130    P(CALC)=INT(DINERO!/DIN!(CALC))
2140    T(CALC)=T(CALC)+P(CALC)
2150    DINERO!=DINERO!-(DIN!(CALC)*P(CALC))
2160  NEXT
2170 RETURN
2180 :
2190 'SUB CabecerasenGeneral
2200  L=5
2210  M$=CHR$(14)+SPACE$(20-LEN(EMPRESA$)\2)+EMPRESA$
2220  LPRINT M$
2230  LPRINT "и"+STRING$(78,205)+"╩"
2240  M$="N╒mina de la Semana"+STR$(SEM)
2250  M$="╨"+SPACE$(40-LEN(M$)\2)+M$
2260  M$=M$+SPACE$(79-LEN(M$))+"╨"
2270  LPRINT M$
2280  LPRINT "╨"+SPACE$(78)+"╨"
2290 RETURN
2300 :
2310 'SUB CabecerasTipodePago
2320  M$="╨"+SPACE$(40-LEN(M$)\2)+M$
2330  M$=M$+SPACE$(79-LEN(M$))+"╨"
2340  LPRINT M$
2350  LPRINT "╨"+SPACE$(78)+"╨"
2360  L=L+2
2370 RETURN
2380 :
2390 'SUB Error
2400  RESUME 2410
2410  ON ERROR GOTO 0
2420  M$=" Error ": GOSUB 2610
2430  M$="Error de Impresi╒n. Pulse una tecla": GOSUB 2510
2440  GOSUB 2470
2450 GOTO 540
2460 :
2470 'SUB PulseUnaTecla
2480  T$ = INKEY$: IF T$ = "" GOTO 2480
2490 RETURN
2500 :
2510 'SUB TextoAyuda
2520  LOCATE 25, 1
2530  IF LEN(M$) >= 80 - (LEN(EMPRESA$) + 3) THEN MASK$ = SPACE$(80) ELSE MASK$ = SPACE$(80 - (LEN(EMPRESA$) + 3)) + "Ё " + EMPRESA$ + " "
2540  COLOR C(CE, 3), C(CE, 4)
2550  PRINT MASK$;
2560  LOCATE 25, 2
2570  COLOR C(CE, 9)
2580  PRINT M$;
2590 RETURN
2600 :
2610 'SUB Mensaje24
2620   IF M = 0 THEN M = 60
2630   LOCATE 24, M
2640   COLOR C(CE, 1), C(CE, 2)
2650   PRINT SPACE$(80 - M);
2660   M = 80 - LEN(M$)
2670   LOCATE 24, M
2680   COLOR C(CE, 7), C(CE, 8)
2690   PRINT M$;
2700 RETURN
2710 :
2720 'SUB EditaTexto
2730   M$=" Etiqueta ": GOSUB 2610
2740   LOCATE 2,1: COLOR C(CE,1),C(CE,2): PRINT SPACE$(80);: LOCATE 2,1
2750   IF T$ >= "a" AND T$ <= "z" THEN CADENT$ = CHR$(ASC(T$) - 32) ELSE CADENT$ = T$
2760   IF T$ = "╓" THEN CADENT$ = "╔"
2770   'DO
2780      COLOR C(CE, 1), C(CE, 2)
2790      LOCATE 2, 1
2800      PRINT "> " + CADENT$;: COLOR C(CE,1)+16: PRINT "_ ";
2810      GOSUB 2470
2820      IF T$=CHR$(8) AND CADENT$<>"" THEN CADENT$=LEFT$(CADENT$,LEN(CADENT$)-1): GOTO 2770 ELSE IF T$=CHR$(8) AND CADENT$="" THEN BEEP: GOTO 2810
2830      IF T$=CHR$(13) OR LEN(T$)=2 THEN 2940  'Aceptado
2831      IF T$=CHR$(27) THEN CADENT$=T$: GOTO 2940  'Aceptado
2840      IF T$=>"A" AND T$<="Z" THEN 2920
2850      IF T$=>"a" AND T$<="z" THEN T$=CHR$(ASC(T$)-32): GOTO 2920
2860      IF T$="═" THEN T$="A": GOTO 2920
2870      IF T$="┌" THEN T$="E": GOTO 2920
2880      IF T$="║" THEN T$="I": GOTO 2920
2890      IF T$="╒" THEN T$="O": GOTO 2920
2900      IF T$="ё" THEN T$="U": GOTO 2920
2910      IF T$="╓" THEN T$="╔"
2920      IF LEN(CADENT$)=76 THEN BEEP ELSE CADENT$=CADENT$+T$
2930   GOTO 2770
2940   'Aceptado
2950   M$=" Impresi╒n ": GOSUB 2610
2960 RETURN
2970 :
2980 'DATOS de los colores del monitor
2990 '    PPlano Splano Pmenu Smenu Pelec Selec Pcursor Scursor Pmensaje
3000 DATA   7   ,   0  ,  0  ,  7  , 15  ,  0  ,   0   ,   7   ,  15
3010 DATA  14   ,   1  ,  0  ,  3  , 15  ,  6  ,  15   ,   3   ,  14
3020 DATA   1   ,   7  , 15  ,  3  , 15  ,  1  ,  11   ,   7   ,  14
3030 DATA   0   ,   7  , 14  ,  2  , 15  ,  0  ,  15   ,   2   ,  12
3040 DATA   0   ,   7  , 14  ,  4  , 15  ,  0  ,  15   ,   4   ,  15
3050 DATA   8   ,   7  , 15  ,  5  , 13  ,  0  ,  15   ,   5   ,  14
3060 :
3070 'Datos de moneda
3080 DATA 1000,5000,10000,20000,50000
