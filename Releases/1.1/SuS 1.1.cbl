      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
000200 PROGRAM-ID.   ELMO.
000500*
000600 ENVIRONMENT DIVISION.
000700*
000800 DATA DIVISION.
000900*
001000 WORKING-STORAGE SECTION.
001100 01  A PIC 9(10).
001200 01  B PIC 9(10).
001300 01  C PIC 999999999999.999999999999(1).
       01  D PIC 9(14).
001400 01  INPUT1 PIC 9(14).
001500 01  RISULTATO PIC 9(14).
001600 PROCEDURE DIVISION.
001700*-----------------------------------------------------------------
001800 MAIN.

           DISPLAY "CALCOLATRICE".
           DISPLAY "ATTENZIONE, IL RISULTATO NON HA SEGNI E VIRGOLE!".
           DISPLAY "CHE VUOI FARE?".
           DISPLAY "1 ADDIZIONE".
           DISPLAY "2 SOTTRAZIONE".
           DISPLAY "3 MOLTIPLICAZIONE".
           DISPLAY "4 DIVISIONE".
           DISPLAY "5 ELEVAZIONE ALLA SECONDA"
           DISPLAY "6 ELEVAZIONE AL CUBO"
           DISPLAY "7 RADICE QUADRATA"
           DISPLAY "8 RADICE CUBICA"
           DISPLAY "9 SENO"
           DISPLAY "10 COSENO"
           DISPLAY "11 TANGENTE"
           DISPLAY "12 SENO^-1"
           DISPLAY "13 COSENO^-1"
           DISPLAY "14 TANGENTE^-1"
           DISPLAY "15 ESCI"
           DISPLAY "SCEGLI UN'OPZIONE"
           ACCEPT INPUT1

           IF INPUT1 = 15
                DISPLAY "OK, BUON LAVORO :)"
                STOP RUN
            END-IF.

           IF INPUT1 = 1
           DISPLAY "PRIMO NUMERO"
           ACCEPT A
           DISPLAY "SECONDO NUMERO"
           ACCEPT B
           COMPUTE C= A + B
                   DISPLAY "Computing"
                   DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY A"+" B " FA..."
                   DISPLAY C
           ELSE
               IF INPUT1 = 2
                   DISPLAY "PRIMO NUMERO"
                   ACCEPT A
                   DISPLAY "SECONDO NUMERO"
                   ACCEPT B
                   DISPLAY "Computing"
                   DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY A "-" B " FA..."
                   COMPUTE C= A - B
                   DISPLAY C
                       ELSE
                           IF INPUT1 = 3
                       DISPLAY "PRIMO NUMERO"
                       ACCEPT A
                       DISPLAY "SECONDO NUMERO"
                       ACCEPT B
                       COMPUTE C= A * B
                              DISPLAY "Computing"
                              DISPLAY "Computing."
                              DISPLAY "Computing.."
                              DISPLAY "Computing..."
                              DISPLAY "Computing...."
                              DISPLAY "Computing....."
                              DISPLAY "Computing......"
                              DISPLAY A "x" B " FA..."
                              DISPLAY C
                           ELSE
                               IF INPUT1 = 4
                               DISPLAY "PRIMO NUMERO"
                               ACCEPT A
                               DISPLAY "SECONDO NUMERO"
                               ACCEPT B
                               COMPUTE C= A / B
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY A ":" B " FA..."
                               DISPLAY C
                               ELSE
                               IF INPUT1 = 5
                               DISPLAY "NUMERO DA ELEVARE"
                               ACCEPT A
                               COMPUTE C= A * A
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY A"^2" " FA..."
                               DISPLAY C
                               ELSE
                                   IF INPUT1 = 6
                               DISPLAY "NUMERO DA ELEVARE (AL CUBO)"
                               ACCEPT A
                               COMPUTE C= A * A * A
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY A"^3" " FA..."
                               DISPLAY C
                               ELSE
                                   IF INPUT1 = 7
                               DISPLAY "NUMERO DA RADICARE"
                               ACCEPT A
                               COMPUTE C= FUNCTION SQRT(A)
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "/"A " FA..."
                   DISPLAY C
                   ELSE
                   IF INPUT1 = 8
                               DISPLAY "NUMERO DA RADICARE"
                               ACCEPT A
                               COMPUTE C= A ** 0.33
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "3/"A " FA..."
                               DISPLAY C
                               ELSE
                                   IF INPUT1 = 9
                               DISPLAY "NUMERO DI CUI FARE SENO"
                               ACCEPT A
                               COMPUTE C= FUNCTION SIN(A)
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "IL SENO DI " A " RISULTA..."
                               DISPLAY C " GRADI"
                                   ELSE
                                       IF INPUT1 = 10
                               DISPLAY "NUMERO DI CUI FARE IL COSENO"
                               ACCEPT A
                               COMPUTE C= FUNCTION COS(A)
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "IL COSENO DI " A " RISULTA..."
                               DISPLAY C" GRADI"
                               ELSE
                                   IF INPUT1 = 11
                               DISPLAY "NUMERO DI CUI FARE LA TANGENTE"
                               ACCEPT A
                               COMPUTE C= FUNCTION TAN(A)
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "A " "FA..."
                   DISPLAY "LA TANGENTE DI " A " RISULTA..."
                   DISPLAY C " GRADI"
                   ELSE
                   IF INPUT1 = 12
                               DISPLAY "NUMERO DI CUI FARE SENO ^-1"
                               ACCEPT A
                               COMPUTE C= FUNCTION ASIN(A)
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "IL SENO^-1 DI " A " RISULTA..."
                               DISPLAY C " GRADI"
                                   ELSE
                                       IF INPUT1 = 13
                               DISPLAY "NUMERO DI CUI FARE COSENO ^-1"
                               ACCEPT A
                               COMPUTE C= FUNCTION ASIN(A)
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "IL COSENO^-1 DI " A " RISULTA..."
                               DISPLAY C " GRADI"
                                   ELSE
                                       IF INPUT1 = 14
                               DISPLAY "NUMERO DI CUI FARE TANGENTE^-1"
                               ACCEPT A
                               COMPUTE C= FUNCTION ATAN(A)
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "LA TANGENTE^-1 DI " A " RISULTA..."
                               DISPLAY C " GRADI"
                    END-IF
                   END-IF
                  END-IF
                 END-IF
                END-IF
               END-IF
              END-IF.
                  STOP RUN.
