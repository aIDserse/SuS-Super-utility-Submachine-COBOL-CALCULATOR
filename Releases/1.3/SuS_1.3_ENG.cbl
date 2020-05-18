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
       01  SLEEP-SEC PIC S9(2)V9(2).
001100 01  A PIC S9(7)V9(7).
001200 01  B PIC S9(7)V9(7).
001300 01  C PIC S9(7)V9(7).
       01  D PIC S9(14)V9(7).
001400 01  INPUT1 PIC 9(14).
001500 01  Q PIC X VALUE "Y".

001600     PROCEDURE DIVISION.
001700*-----------------------------------------------------------------
001800 MAIN.
           PERFORM WITH TEST AFTER UNTIL Q ="YES" OR "Y" OR "y" OR "yes" OR "Yes"
           END-PERFORM.
           DISPLAY "CALCULATOR".
           DISPLAY "WHAT DO YOU WANT DO DO?".
           DISPLAY "1 ADDITION".
           DISPLAY "2 SUBTRACTION".
           DISPLAY "3 MOLTIPLICATION".
           DISPLAY "4 DIVISION".
           DISPLAY "5 SQUARING"
           DISPLAY "6 CUBING"
           DISPLAY "7 SQUARE ROOT"
           DISPLAY "8 CUBE ROOT"
           DISPLAY "9 SINUS"
           DISPLAY "10 COSINE"
           DISPLAY "11 TANGENT"
           DISPLAY "12 SIN^-1"
           DISPLAY "13 COS^-1"
           DISPLAY "14 TAN^-1"
           DISPLAY "15 EXIT"
           DISPLAY "CHOOSE AN OPTION"
           ACCEPT INPUT1
           EVALUATE  INPUT1

           WHEN = 15
                DISPLAY "OK, GOOD JOB :)"
                STOP RUN

           WHEN = 1
           DISPLAY "FIRST NUMBER"
           ACCEPT A
           DISPLAY "SECOND NUMBER"
           ACCEPT B
           COMPUTE C= A + B
                   DISPLAY "Computing"
                   DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "(" A ")" "+" "(" B ")" "RESULTS..."
                   DISPLAY C

                WHEN = 2
                   DISPLAY "FIRST NUMBER"
                   ACCEPT A
                   DISPLAY "SECOND NUMBER"
                   ACCEPT B
                   DISPLAY "Computing"
                   DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "(" A ")" "-" "(" B ")" " RESULTS..."
                   COMPUTE C= A - B
                   DISPLAY C

                           WHEN = 3
                       DISPLAY "FIRST NUMBER"
                       ACCEPT A
                       DISPLAY "SECOND NUMBER"
                       ACCEPT B
                       COMPUTE C= A * B
                              DISPLAY "Computing"
                              DISPLAY "Computing."
                              DISPLAY "Computing.."
                              DISPLAY "Computing..."
                              DISPLAY "Computing...."
                              DISPLAY "Computing....."
                              DISPLAY "Computing......"
                          DISPLAY "(" A ")" "X" "(" B ")" " RESULTS..."
                              DISPLAY C

                               WHEN = 4
                               DISPLAY "FIRST NUMBER"
                               ACCEPT A
                               DISPLAY "SECOND NUMBER"
                               ACCEPT B
                               COMPUTE C= A / B
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "(" A ")" ":" "(" B ")" " RESULTS..."
                               DISPLAY C

                               WHEN = 5
                               DISPLAY "NUMERO TO SQUARE"
                               ACCEPT A
                               COMPUTE C= A * A
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "(" A ")""^2" " RESULTS..."
                               DISPLAY C

                                   WHEN = 6
                               DISPLAY "NUMERO DA ELEVARE (AL CUBO)"
                               ACCEPT A
                               COMPUTE C= A * A * A
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "(" A ")""^3" " FA..."
                               DISPLAY C

                                   WHEN = 7
                               DISPLAY "SQUARE ROOT OF"
                               ACCEPT A
                               COMPUTE C= FUNCTION SQRT(A)
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "/""(" A ")" " FA..."
                   DISPLAY C

                   WHEN = 8
                               DISPLAY "CUBE ROOT OF..."
                               ACCEPT A
                               COMPUTE C= A ** 0.33
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "3/""(" A ")" " RESULTS..."
                               DISPLAY C

                                   WHEN = 9
                               DISPLAY "SIN OF..."
                               ACCEPT A
                             COMPUTE C= FUNCTION SIN(A * 3.14159 / 180)
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "SIN OF " "(" A ")" " RESULTS..."
                               DISPLAY C " DEG"

                                       WHEN = 10
                               DISPLAY "COS OF"
                               ACCEPT A
                              COMPUTE C= FUNCTION COS(A * 3.14159 / 180)
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "COS OF"  "(" A ")" "RESULTS"
                               DISPLAY C" DEGREES"

                                   WHEN = 11
                               DISPLAY "TAN OF..."
                               ACCEPT A
                              COMPUTE C= FUNCTION TAN(A * 3.14159 / 180)
                               END-COMPUTE
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "TAN OF" "(" A ")" " RESULTS..."
                   DISPLAY C " DEGREES"

                    WHEN= 12
                               DISPLAY "SIN ^-1 OF..."
                               ACCEPT A
                             COMPUTE C= FUNCTION ASIN(A * 3.14159 / 180)
                               END-COMPUTE
                               COMPUTE C= (C / 3.14159 * 180)
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "IL SIN^-1 OF " "(" A ")" " RESULTS..."
                               DISPLAY C " DEGREES"

                                   WHEN = 13
                               DISPLAY "COS ^-1 OF..."
                               ACCEPT A
                             COMPUTE C= FUNCTION ASIN(A * 3.14159 / 180)
                               END-COMPUTE
                               COMPUTE C= (C / 3.14159 * 180)
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "IL COS^-1 OF " "(" A ")" " RESULTS..."
                               DISPLAY C " DEGREES"

                                       WHEN = 14
                               DISPLAY "TAN^-1 OF..."
                               ACCEPT A
                           COMPUTE C= FUNCTION ATAN(A)
                               END-COMPUTE
                               COMPUTE C= (C / 3.14159 * 180)
                              DISPLAY "Computing."
                   DISPLAY "Computing.."
                   COMMIT
                   DISPLAY "Computing..."
                   DISPLAY "Computing...."
                   DISPLAY "Computing....."
                   DISPLAY "Computing......"
                   DISPLAY "TAN^-1 OF... " "(" A ")" " RESULTS..."
                               DISPLAY C " GRADI"
           END-EVALUATE

                   IF INPUT1 NOT = 15
               DISPLAY "DO YOU WANT TO DO OTHER CALCULATIONS?"
               ACCEPT Q
               IF Q = "YES" OR "Y" OR "y" OR "yes" OR "Yes" GO TO MAIN
                   ELSE DISPLAY "OK, GOOD JOB :)"
                  END-IF
                  STOP RUN.
