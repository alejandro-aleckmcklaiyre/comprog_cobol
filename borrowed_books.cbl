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
           05  Author-Name       PIC X(30).
           05  BorrowerName      PIC X(30).
           05  Date-input        PIC X(10).

       WORKING-STORAGE SECTION.
       01  WS-Choice            PIC 9 VALUE 0.
       01  WS-Continue          PIC X VALUE 'Y'.
       01  FileStatus           PIC X(2).
       01  WS-DeleteTitle       PIC X(50).
       01  New-Author           PIC X(30).
       01  New-BorrowerName     PIC X(30).
       01  New-DateBorrowed     PIC X(10).
       01  InvalidDate          PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN-PROCESS.
           PERFORM UNTIL WS-Continue = 'N'
               DISPLAY '=============================='
               DISPLAY '     BORROWED BOOKS MENU      '
               DISPLAY '=============================='
               DISPLAY '1. Add a new borrowed book'
               DISPLAY '2. Display borrowed books'
               DISPLAY '3. Delete a book'
               DISPLAY '4. Update details'
               DISPLAY '5. Search for a book'
               DISPLAY '6. Exit'
               DISPLAY '=============================='
               DISPLAY 'Enter your choice: '
               ACCEPT WS-Choice

               EVALUATE WS-Choice
                   WHEN 1
                       PERFORM ADD-BOOK
                   WHEN 2
                       PERFORM DISPLAY-BOOKS
                   WHEN 3
                       PERFORM DELETE-BOOK
                   WHEN 4
                       PERFORM UPDATE-BOOK
                   WHEN 5
                       PERFORM SEARCH-BOOK
                   WHEN 6
                       MOVE 'N' TO WS-Continue
                   WHEN OTHER
                       DISPLAY 'Invalid choice. Please try again.'
               END-EVALUATE
           END-PERFORM.

           STOP RUN.

       ADD-BOOK.
           OPEN I-O BorrowedBooksFile
           IF FileStatus = '35'
               DISPLAY 'File not found. Creating a new file.'
               OPEN OUTPUT BorrowedBooksFile
               CLOSE BorrowedBooksFile
               OPEN I-O BorrowedBooksFile
           END-IF.

           DISPLAY 'Enter book title: '
           ACCEPT BookTitle
           DISPLAY 'Enter author name: '
           ACCEPT Author-Name
           DISPLAY 'Enter borrower''s name: '
           ACCEPT BorrowerName
       

           MOVE 'Y' TO InvalidDate
           PERFORM UNTIL InvalidDate = 'N'
               DISPLAY 'Enter date borrowed (YYYY-MM-DD): '
               ACCEPT Date-input
               IF Date-input(1:4) NUMERIC AND Date-input(5:1) = '-' AND
                  Date-input(6:2) NUMERIC AND Date-input(8:1) = '-' AND
                  Date-input(9:2) NUMERIC AND
                  FUNCTION NUMVAL(Date-input(6:2)) > 0 AND 
                  FUNCTION NUMVAL(Date-input(6:2)) <= 12 AND
                  FUNCTION NUMVAL(Date-input(9:2)) > 0 AND 
                  FUNCTION NUMVAL(Date-input(9:2)) <= 31
                   MOVE 'N' TO InvalidDate
               ELSE
                   DISPLAY 'Invalid date format. Please try again.'
               END-IF
           END-PERFORM.

           WRITE BorrowedBookRecord
               INVALID KEY
                   DISPLAY 'Error: Duplicate book title. Not added.'
               NOT INVALID KEY
                   DISPLAY 'Book successfully added.'
           END-WRITE.

           CLOSE BorrowedBooksFile.

       DISPLAY-BOOKS.
           OPEN I-O BorrowedBooksFile
           DISPLAY 'List of borrowed books:'
           DISPLAY '=============================='
           PERFORM UNTIL FileStatus = '10'
               READ BorrowedBooksFile NEXT INTO BorrowedBookRecord
                   AT END
                       MOVE '10' TO FileStatus
                   NOT AT END
                       DISPLAY 'Title: ' BookTitle
                       DISPLAY 'Author: ' Author-Name
                       DISPLAY 'Borrower: ' BorrowerName
                       DISPLAY 'Date Borrowed: ' Date-input
                       DISPLAY '------------------------------'
               END-READ
           END-PERFORM.

           CLOSE BorrowedBooksFile.

       DELETE-BOOK.
           OPEN I-O BorrowedBooksFile
           DISPLAY 'Enter the title of the book to delete: '
           ACCEPT WS-DeleteTitle
           MOVE WS-DeleteTitle TO BookTitle
           DELETE BorrowedBooksFile
               INVALID KEY
                   DISPLAY 'Error: Book not found.'
               NOT INVALID KEY
                   DISPLAY 'Book successfully deleted.'
           END-DELETE.

           CLOSE BorrowedBooksFile.

       UPDATE-BOOK.
           OPEN I-O BorrowedBooksFile
           DISPLAY 'Enter the title of the book to update: '
           ACCEPT BookTitle
           START BorrowedBooksFile KEY IS EQUAL TO BookTitle
               INVALID KEY
                   DISPLAY 'Book not found.'
               NOT INVALID KEY
                   READ BorrowedBooksFile INTO BorrowedBookRecord
                   DISPLAY 'Current details:'
                   DISPLAY 'Title: ' BookTitle
                   DISPLAY 'Author: ' Author-Name
                   DISPLAY 'Borrower: ' BorrowerName
                   DISPLAY 'Date Borrowed: ' Date-input
                   DISPLAY '==========================================='
                   DISPLAY 'INSTRUCTION: leave [blank] to keep current.'
                   DISPLAY '==========================================='
                   DISPLAY 'Enter new author: '
                   ACCEPT New-Author
                   IF New-Author NOT = SPACE
                       MOVE New-Author TO Author-Name
                   END-IF
                   DISPLAY 'Enter new borrower: '
                   ACCEPT New-BorrowerName
                   IF New-BorrowerName NOT = SPACE
                       MOVE New-BorrowerName TO BorrowerName
                   END-IF

                   MOVE 'Y' TO InvalidDate
                   PERFORM UNTIL InvalidDate = 'N'
                       DISPLAY 'Enter new date borrowed: '
                       ACCEPT New-DateBorrowed
                       IF New-DateBorrowed = SPACE
                           MOVE Date-input TO New-DateBorrowed
                       END-IF
                       IF New-DateBorrowed(1:4) NUMERIC AND 
                          New-DateBorrowed(5:1) = '-' AND 
                          New-DateBorrowed(8:1) = '-' AND 
                          FUNCTION NUMVAL(New-DateBorrowed(6:2)) > 0 AND 
                          FUNCTION NUMVAL(New-DateBorrowed(6:2))<=12 AND
                          FUNCTION NUMVAL(New-DateBorrowed(9:2)) > 0 AND 
                          FUNCTION NUMVAL(New-DateBorrowed(9:2)) <= 31
                           MOVE 'N' TO InvalidDate
                       ELSE
                           DISPLAY 'Invalid date format.'
                       END-IF
                   END-PERFORM

                   MOVE New-DateBorrowed TO Date-input
                   REWRITE BorrowedBookRecord
                       INVALID KEY
                           DISPLAY 'Error updating book record.'
                       NOT INVALID KEY
                           DISPLAY 'Book updated successfully.'
                   END-REWRITE
           END-START.

           CLOSE BorrowedBooksFile.

       SEARCH-BOOK.
           OPEN I-O BorrowedBooksFile
           DISPLAY 'Enter book title to search: '
           ACCEPT BookTitle
           START BorrowedBooksFile KEY IS EQUAL TO BookTitle
               INVALID KEY
                   DISPLAY 'Book not found.'
               NOT INVALID KEY
                   READ BorrowedBooksFile INTO BorrowedBookRecord
                   DISPLAY 'Book found!'
                   DISPLAY 'Title: ' BookTitle
                   DISPLAY 'Author: ' Author-Name
                   DISPLAY 'Borrower: ' BorrowerName
                   DISPLAY 'Date Borrowed: ' Date-input
           END-START.

           CLOSE BorrowedBooksFile.

       END PROGRAM BorrowedBooks.
       