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
001100 01  A PIC 9(7).
001200 01  B PIC 9(7).
001300 01  C PIC 9(14).
001400 01  INPUT1 PIC 9(14).
001500 01  RISULTATO PIC 9(14).
001600 PROCEDURE DIVISION.
001700*-----------------------------------------------------------------
001800 MAIN.

           DISPLAY "CALCOLATRICE".
           DISPLAY "ATTENZIONE, IL RISULTATO NON HA SEGNI!".
           DISPLAY "CHE VUOI FARE?".
           DISPLAY "1 ADDIZIONE".
           DISPLAY "2 SOTTRAZIONE".
           DISPLAY "3 MOLTIPLICAZIONE".
           DISPLAY "4 DIVISIONE".
           DISPLAY "5 ELEVAZIONE ALLA SECONDA"
           DISPLAY "6 RADICE QUADRATA"
           DISPLAY "7 SENO"
           DISPLAY "8 COSENO"
           DISPLAY "9 TAN"
           DISPLAY "10 ESCI"
           ACCEPT INPUT1

           IF INPUT1 = 10
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
                   DISPLAY A "+" B  " FA..."
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
                                   IF INPUT1 = 7
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
                               DISPLAY C" GRADI"
                                   ELSE
                                       IF INPUT1 = 8
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
                                   IF INPUT1 = 9
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





                    END-IF
                   END-IF
                  END-IF
                 END-IF
                END-IF
               END-IF
              END-IF.
           STOP RUN.
           GOBACK.
