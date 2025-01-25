       IDENTIFICATION DIVISION.
       PROGRAM-ID. FileHandlingMainMenu.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALBUM-FILE ASSIGN TO "albums.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ALBUM-FILE.
       01 ALBUM-RECORD.
           05 ALBUM-NAME   PIC X(50).
           05 ARTIST-NAME  PIC X(50).
           05 RELEASE-YEAR PIC 9(4).

       WORKING-STORAGE SECTION.
       01 WS-USER-CHOICE PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-MENU.
           PERFORM WITH TEST AFTER UNTIL WS-USER-CHOICE = 6
               DISPLAY "======================================="
               DISPLAY "       FILE HANDLING MAIN MENU         "
               DISPLAY "======================================="
               DISPLAY "1. Add New Album Record (Aleck)"
               DISPLAY "2. Display the list (Jasmine)"
               DISPLAY "3. Update existing Recid (Loise)"
               DISPLAY "4. Delete a record (Ash)"
               DISPLAY "5. Search for a record (Mik)"
               Display "6. Exit"
               DISPLAY "======================================="
               DISPLAY "Enter your choice (1-6): "
               ACCEPT WS-USER-CHOICE

               EVALUATE WS-USER-CHOICE
                   WHEN 1
                       PERFORM ADD-NEW-RECORD
                   WHEN 2
                       DISPLAY "Displaying the records"
                   WHEN 3
                       DISPLAY "Update the records"
                   WHEN 4
                       DISPLAY "Delete the records"
                   WHEN 5
                       DISPLAY "View the records"
                   WHEN 6
                       DISPLAY "Exiting Program. Goodbye!"
                   WHEN OTHER
                       DISPLAY "Invalid choice. Please try again."
               END-EVALUATE
           END-PERFORM.

           STOP RUN.

       ADD-NEW-RECORD.
           DISPLAY "Add New Record functionality called."
           DISPLAY "Enter Album Name: "
           ACCEPT ALBUM-NAME
           DISPLAY "Enter Artist Name: "
           ACCEPT ARTIST-NAME
           DISPLAY "Enter Release Year: "
           ACCEPT RELEASE-YEAR

           DISPLAY "Album Name: " ALBUM-NAME
           DISPLAY "Artist Name: " ARTIST-NAME
           DISPLAY "Release Year: " RELEASE-YEAR.

           DISPLAY "Record added successfully.".
