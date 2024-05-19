10 'SAVE "SEMANAS"
20 ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '
30 ' SEMANAS                                                         '
40 ' Programa para establecer las semanas del a¤o en curso. Es eje-  '
50 ' cutado desde el men£ principal. GMD N¢minas V2                  '
60 ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '
70 'Checa si se ejecut¢ desde el men£ principal
80 IF EMPRESA$="" THEN RUN "nomina"
90 'Inicializa los datos
100    DEFINT A-Z
110    OPTION BASE 1: DIM C(6, 9)
120    FOR I = 1 TO 6
130      FOR J = 1 TO 9
140        READ C(I, J)
150    NEXT J, I
160    OPEN "I", 1, "nomina.ini"
170      INPUT #1, EMPRESA$, CE
180    CLOSE
190    VIEW PRINT 2 TO 24: COLOR C(CE,1),C(CE,2): CLS: VIEW PRINT
200    M$=" Semanas ": GOSUB 720
210   FALSE = FALSE: TRUE = NOT FALSE
220 :
230 'Programa Principal
240   M$="A¤o de Elaboraci¢n (1992 a 2090)": GOSUB 620
250   GOSUB 830 'EditaTexto
260   IF CADENT$="" THEN RUN "nomina"
270   YEAR=VAL(CADENT$)
280   IF YEAR<1991 OR YEAR>2090 THEN M$="Tecl‚e un valor de 1991 a 2090":GOSUB 620: T$="": GOTO 250
290   M$="Un momento, Creando los rangos semanales de"+STR$(YEAR): GOSUB 620
300   DIM MES$(12),MES(12)
310   FOR I=1 TO 12: READ MES$(I),MES(I): NEXT
320   IF (YEAR/4)=INT(YEAR/4) THEN MES(2)=29
330   'Calcula el primer lunes del a¤o que se pide
340   DIA=7
350   FOR I=1992 TO YEAR
360     IF ((I-1)/4)=INT((I-1)/4) THEN DIA=DIA-2 ELSE DIA=DIA-1
370     IF DIA<1 THEN DIA=DIA+7
380   NEXT
390   'Graba las secuencias de d¡as del a¤o en el archivo
400   OPEN "O",1,"SEMANAS.DAT"
410   FIN=FALSE: MES=1
420   WHILE NOT FIN
430      IF DIA+6<=MES(MES) THEN SEMANA$=MID$(STR$(DIA),2)+" al"+STR$(DIA+6)+" de "+MES$(MES)+" de"+STR$(YEAR): DIA=DIA+7: GOTO 510
440      'La semana se complementa con otro mes
450      DIA2=ABS(MES(MES)-(DIA+6))
460      MES2=MES+1: IF MES2>12 THEN MES2=1: FIN=TRUE
470      IF NOT FIN THEN SEMANA$=MID$(STR$(DIA),2)+" de "+MES$(MES)+" al"+STR$(DIA2)+" de "+MES$(MES2)+" de"+STR$(YEAR) ELSE SEMANA$=MID$(STR$(DIA),2)+" de "+MES$(MES)+" de"+STR$(YEAR)+" al"+STR$(DIA2)+" de "+MES$(MES2)+" de"+STR$(YEAR+1)
480      DIA=DIA2+1
490      MES=MES2
500      'Escribe la semana en el archivo
510      WRITE #1,SEMANA$
520      IF DIA>MES(MES) THEN DIA=1: MES=MES+1: IF MES>12 THEN FIN=TRUE
530   WEND
540 CLOSE: RESET
550 RUN "nomina"
560 END
570 :
580 'SUB PulseUnaTecla
590  T$ = INKEY$: IF T$ = "" GOTO 590
600 RETURN
610 :
620 'SUB TextoAyuda
630  LOCATE 25, 1
640  IF LEN(M$) >= 80 - (LEN(EMPRESA$) + 3) THEN MASK$ = SPACE$(80) ELSE MASK$ = SPACE$(80 - (LEN(EMPRESA$) + 3)) + "³ " + EMPRESA$ + " "
650  COLOR C(CE, 3), C(CE, 4)
660  PRINT MASK$;
670  LOCATE 25, 2
680  COLOR C(CE, 9)
690  PRINT M$;
700 RETURN
710 :
720 'SUB Mensaje24
730   IF M = 0 THEN M = 60
740   LOCATE 24, M
750   COLOR C(CE, 1), C(CE, 2)
760   PRINT SPACE$(80 - M);
770   M = 80 - LEN(M$)
780   LOCATE 24, M
790   COLOR C(CE, 7), C(CE, 8)
800   PRINT M$;
810 RETURN
820 :
830 'SUB EditaTexto
840   M$=" Etiqueta ": GOSUB 720
850   LOCATE 2,1: COLOR C(CE,1),C(CE,2): PRINT SPACE$(80);: LOCATE 2,1
860   IF T$ >= "a" AND T$ <= "z" THEN CADENT$ = CHR$(ASC(T$) - 32) ELSE CADENT$ = T$
870   IF T$ = "¤" THEN CADENT$ = "¥"
880   'DO
890      COLOR C(CE, 1), C(CE, 2)
900      LOCATE 2, 1
910      PRINT "> " + CADENT$;: COLOR C(CE,1)+16: PRINT "_ ";
920      GOSUB 580
930      IF T$=CHR$(8) AND CADENT$<>"" THEN CADENT$=LEFT$(CADENT$,LEN(CADENT$)-1): GOTO 880 ELSE IF T$=CHR$(8) AND CADENT$="" THEN BEEP: GOTO 920
940      IF T$=CHR$(13) OR LEN(T$)=2 THEN 1060  'Aceptado
950      IF T$=CHR$(27) THEN CADENT$="": T$="": GOTO 1060  'Aceptado
960      IF T$=>"A" AND T$<="Z" THEN 1040
970      IF T$=>"a" AND T$<="z" THEN T$=CHR$(ASC(T$)-32): GOTO 1040
980      IF T$=" " THEN T$="A": GOTO 1040
990      IF T$="‚" THEN T$="E": GOTO 1040
1000      IF T$="¡" THEN T$="I": GOTO 1040
1010      IF T$="¢" THEN T$="O": GOTO 1040
1020      IF T$="£" THEN T$="U": GOTO 1040
1030      IF T$="¤" THEN T$="¥"
1040      IF LEN(CADENT$)=76 THEN BEEP ELSE CADENT$=CADENT$+T$
1050   GOTO 880
1060   'Aceptado
1070   LOCATE 2,1: COLOR C(CE,1),C(CE,2): PRINT SPACE$(80);
1080   M$=" Semanizaci¢n ": GOSUB 720
1090 RETURN
1100 :
1110 'DATOS de los colores del monitor
1120 '    PPlano Splano Pmenu Smenu Pelec Selec Pcursor Scursor Pmensaje
1130 DATA   7   ,   0  ,  0  ,  7  , 15  ,  0  ,   0   ,   7   ,  15
1140 DATA  14   ,   1  ,  0  ,  3  , 15  ,  6  ,  15   ,   3   ,  14
1150 DATA   1   ,   7  , 15  ,  3  , 15  ,  1  ,  11   ,   7   ,  14
1160 DATA   0   ,   7  , 14  ,  2  , 15  ,  0  ,  15   ,   2   ,  12
1170 DATA   0   ,   7  , 14  ,  4  , 15  ,  0  ,  15   ,   4   ,  15
1180 DATA   8   ,   7  , 15  ,  5  , 13  ,  0  ,  15   ,   5   ,  14
1190 :
1200 'Datos de Meses
1210 DATA Enero,31,Febrero,28,Marzo,31,Abril,30,Mayo,31,Junio,30,Julio,31
1220 DATA Agosto,31,Septiembre,30,Octubre,31,Noviembre,30,Diciembre,31
