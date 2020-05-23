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
001100 01  A PIC S9(7)V9(7).
001200 01  B PIC S9(7)V9(7).
001300 01  C PIC S9(7)V9(7).
       01  D PIC S9(14)V9(7).
001400 01  INPUT1 PIC 9(14).
001500 01  Q PIC X VALUE "S".

001600     PROCEDURE DIVISION.
001700*-----------------------------------------------------------------
001800 MAIN.

           DISPLAY "CALCOLATRICE".
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
           EVALUATE INPUT1

           WHEN = 15 DISPLAY "OK, BUON LAVORO :)"
                       DISPLAY "ESCO TRA 3 SECONDI..."
                       CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL
                   DISPLAY "ESCO TRA 2 SECONDI.."
                   CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL
                   DISPLAY "ESCO TRA 1 SECONDO."
                   CALL "CBL_OC_NANOSLEEP" USING "200000000"
                   END-CALL
                STOP RUN

           WHEN = 1
           DISPLAY "PRIMO NUMERO"
           ACCEPT A
           DISPLAY "SECONDO NUMERO"
           ACCEPT B
           COMPUTE C= A + B
                   DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "+"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" B ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY " FA..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY "(" C ")"
                   CALL "CBL_OC_NANOSLEEP" USING "1000000000"

               WHEN = 2
                   DISPLAY "PRIMO NUMERO"
           ACCEPT A
           DISPLAY "SECONDO NUMERO"
           ACCEPT B
           COMPUTE C= A - B
                   DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "-"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" B ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY " FA..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY "(" C ")"
                   CALL "CBL_OC_NANOSLEEP" USING "1000000000"

                           WHEN = 3
                       DISPLAY "PRIMO NUMERO"
           ACCEPT A
           DISPLAY "SECONDO NUMERO"
           ACCEPT B
           COMPUTE C= A * B
                   DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "x"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" B ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY " FA..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY "(" C ")"


                               WHEN = 4
                               DISPLAY "PRIMO NUMERO"
           ACCEPT A
           DISPLAY "SECONDO NUMERO"
           ACCEPT B
           COMPUTE C= A / B
                   DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY ":"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" B ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY " FA..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY "(" C ")"
                   CALL "CBL_OC_NANOSLEEP" USING "1000000000"

                               WHEN = 5
                               DISPLAY "NUMERO DA ELEVARE"
                               ACCEPT A
                               COMPUTE C= A * A
                              DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY "(" A ")^2"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY "FA..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                               DISPLAY "(" C ")"
                              CALL "CBL_OC_NANOSLEEP" USING "1000000000"

                                   WHEN = 6
                               DISPLAY "NUMERO DA ELEVARE (AL CUBO)"
                               ACCEPT A
                               COMPUTE C= A * A * A
                             DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                    DISPLAY "(" A ")^3"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY "FA..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                               DISPLAY "(" C ")"
                              CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL

                                   WHEN = 7
                               DISPLAY "NUMERO DA RADICARE"
                               ACCEPT A
                               COMPUTE C= FUNCTION SQRT(A)
                               END-COMPUTE
                              DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "/(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY "FA..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                               DISPLAY "(" C ")"
                              CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL

                   WHEN = 8
                               DISPLAY "NUMERO DA RADICARE"
                               ACCEPT A
                               COMPUTE C= A ** 0.33
                               END-COMPUTE
                              DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "3/(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   DISPLAY "FA..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                               DISPLAY "(" C ")"
                              CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL
                               DISPLAY C

                                   WHEN = 9
                               DISPLAY "NUMERO DI CUI FARE SENO"
                               ACCEPT A
                             COMPUTE C= FUNCTION SIN(A)
                             COMPUTE C= FUNCTION COS(A * 3.14159 / 180)
                               END-COMPUTE
                              DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "IL SENO DI "
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "RISULTA..."
                               DISPLAY "(" C ")" " GRADI"
                             CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL

                                       WHEN = 10
                               DISPLAY "NUMERO DI CUI FARE IL COSENO"
                               ACCEPT A
                              COMPUTE C= FUNCTION COS(A * 3.14159 / 180)
                               END-COMPUTE
                              DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "IL COSENO DI "
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "RISULTA..."
                               DISPLAY "(" C ")" " GRADI"
                            CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL

                                   WHEN = 11
                               DISPLAY "NUMERO DI CUI FARE LA TANGENTE"
                               ACCEPT A
                              COMPUTE C= FUNCTION TAN(A * 3.14159 / 180)
                               END-COMPUTE
                              DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "LA TANGENTE DI "
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "RISULTA..."
                               DISPLAY "(" C ")" " GRADI"
                             CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL

                    WHEN= 12
                               DISPLAY "NUMERO DI CUI FARE SENO ^-1"
                               ACCEPT A
                             COMPUTE C= FUNCTION ASIN(A)
                               END-COMPUTE
                               COMPUTE C= (C / 3.14159 * 180)
                              DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                  DISPLAY "IL SENO^-1 DI "
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "RISULTA..."
                               DISPLAY "(" C ")" " GRADI"
                           CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL

                                   WHEN = 13
                               DISPLAY "NUMERO DI CUI FARE COSENO ^-1"
                               ACCEPT A
                             COMPUTE C= FUNCTION ASIN(A)
                               END-COMPUTE
                               COMPUTE C= (C / 3.14159 * 180)
                              DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "IL COSENO^-1 DI "
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "RISULTA..."
                               DISPLAY "(" C ")" " GRADI"
                             CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL

                                       WHEN = 14
                               DISPLAY "NUMERO DI CUI FARE TANGENTE^-1"
                               ACCEPT A
                           COMPUTE C= FUNCTION ATAN(A)
                               END-COMPUTE
                               COMPUTE C= (C / 3.14159 * 180)
                              DISPLAY "Computing"
                   CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL
                   DISPLAY "Computing."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing.."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing..."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing...."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing....."
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "Computing......"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "LA TANGENTE^-1 "
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "(" A ")"
                   CALL "CBL_OC_NANOSLEEP" USING "500000000"
                   END-CALL
                   DISPLAY "RISULTA..."
                               DISPLAY "(" C ")" " GRADI"
                              CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL
           END-EVALUATE

                   IF INPUT1 NOT = 15
               DISPLAY "VUOI FARE ALTRI CALCOLI?"
               ACCEPT Q
               IF Q = "SI" OR "S" OR "s" OR "si" GO TO MAIN
                   ELSE DISPLAY "OK, BUON LAVORO :)"
                       DISPLAY "ESCO TRA 3 SECONDI..."
                       CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL
                   DISPLAY "ESCO TRA 2 SECONDI.."
                   CALL "CBL_OC_NANOSLEEP" USING "1000000000"
                   END-CALL
                   DISPLAY "ESCO TRA 1 SECONDO."
                   CALL "CBL_OC_NANOSLEEP" USING "300000000"
                   END-CALL


                  END-IF
                  STOP RUN.
