       IDENTIFICATION DIVISION.
       PROGRAM-ID. BorrowedBooks.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BorrowedBooksFile ASSIGN TO 'borrowed_books.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BookTitle
               FILE STATUS IS FileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD  BorrowedBooksFile.
       01  BorrowedBookRecord.
           05  BookTitle         PIC X(50).
           05  Author            PIC X(30).
           05  BorrowerName      PIC X(30).
           05  DateBorrowed      PIC X(10).

       WORKING-STORAGE SECTION.
       01  WS-Choice            PIC 9.
       01  WS-Continue          PIC X VALUE 'Y'.
       01  EOF                  PIC X VALUE 'N'.
       01  FileStatus           PIC X(2).
       01  WS-DeleteTitle       PIC X(50).
       01  WS-BookFound         PIC X VALUE 'N'.

       PROCEDURE DIVISION.

           OPEN I-O BorrowedBooksFile
           IF FileStatus = '35'  *> If file does not exist, create it
               DISPLAY "File not found, creating new file."
               OPEN OUTPUT BorrowedBooksFile
           ELSE
               IF FileStatus NOT EQUAL '00'  *> If there's another file error
                   DISPLAY "Error opening file. Status: " FileStatus
                   STOP RUN
               END-IF
           END-IF

           PERFORM UNTIL WS-Continue = 'N'
               DISPLAY "1. Add a new borrowed book"
               DISPLAY "2. Display borrowed books"
               DISPLAY "3. Delete Book"
               DISPLAY "4. Exit"
               ACCEPT WS-Choice
               EVALUATE WS-Choice
                   WHEN 1
                       PERFORM ADD-BOOK
                   WHEN 2
                       PERFORM DISPLAY-BOOKS
                   WHEN 3
                       PERFORM DELETE-BOOK
                   WHEN 4
                       MOVE 'N' TO WS-Continue
                   WHEN OTHER
                       DISPLAY "Invalid choice, please try again."
               END-EVALUATE
           END-PERFORM
           
           CLOSE BorrowedBooksFile
           STOP RUN.

       ADD-BOOK.
           DISPLAY "Enter book title: "
           ACCEPT BookTitle
           DISPLAY "Enter author: "
           ACCEPT Author
           DISPLAY "Enter borrower's name: "
           ACCEPT BorrowerName
           DISPLAY "Enter date borrowed (YYYY-MM-DD): "
           ACCEPT DateBorrowed
           WRITE BorrowedBookRecord
               INVALID KEY
                   DISPLAY "Error: Duplicate book title. Not added."
               NOT INVALID KEY
                   DISPLAY "Book successfully added."
           END-WRITE.
       
       DISPLAY-BOOKS.
           DISPLAY "List of borrowed books:"
           MOVE 'N' TO EOF  *> Reset EOF flag before reading
           PERFORM UNTIL EOF = 'Y'
               READ BorrowedBooksFile INTO BorrowedBookRecord
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       DISPLAY "Title: " BookTitle
                       DISPLAY "Author: " Author
                       DISPLAY "Borrower: " BorrowerName
                       DISPLAY "Date Borrowed: " DateBorrowed
                       DISPLAY "--------------------------"
               END-READ
           END-PERFORM
           CLOSE BorrowedBooksFile.
       
       DELETE-BOOK.
           DISPLAY "Enter the title of the book to delete: "
           ACCEPT WS-DeleteTitle
           MOVE 'N' TO WS-BookFound
           MOVE 'N' TO EOF  *> Reset EOF flag before reading
           PERFORM UNTIL EOF = 'Y'
               READ BorrowedBooksFile INTO BorrowedBookRecord
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF WS-DeleteTitle = BookTitle
                           DELETE BorrowedBooksFile
                               INVALID KEY
                                   DISPLAY "Error: Book not found."
                               NOT INVALID KEY
                       DISPLAY "Book successfully deleted."
                       PERFORM DISPLAY-BOOKS

                                   MOVE 'Y' TO WS-BookFound
                           END-DELETE
                       END-IF
               END-READ
           END-PERFORM
           IF WS-BookFound = 'N' 
               DISPLAY "Error: Book not found."

               DISPLAY "Error: Book not found."
           END-IF.

       END PROGRAM BorrowedBooks.
