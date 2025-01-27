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
           05  DateBorrowed      PIC X(10).

       WORKING-STORAGE SECTION.
       01  WS-Choice            PIC 9.
       01  WS-Continue          PIC X VALUE 'Y'.
       01  EOF                  PIC X VALUE 'N'.
       01  FileStatus           PIC X(2).
       01  WS-DeleteTitle       PIC X(50).
       01  New-Author           PIC X(30).
       01  New-BorrowerName     PIC X(30).
       01  New-DateBorrowed     PIC X(10).
       01  SEARCH-TITLE         PIC X(50).
       01  BOOKFOUND            PIC X VALUE 'N'.
      


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
               DISPLAY "4. Update Book"
               DISPLAY "5. Search for a book"
               DISPLAY "6. Exit"
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
                       DISPLAY "Invalid choice, please try again."
               END-EVALUATE
           END-PERFORM
           
           CLOSE BorrowedBooksFile
           STOP RUN.

       ADD-BOOK.
           DISPLAY "Enter book title: "
           ACCEPT BookTitle
           DISPLAY "Enter author: "
           ACCEPT Author-Name
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
                       DISPLAY "Author: " Author-Name
                       DISPLAY "Borrower: " BorrowerName
                       DISPLAY "Date Borrowed: " DateBorrowed
                       DISPLAY "--------------------------"
               END-READ
           END-PERFORM
           CLOSE BorrowedBooksFile.
       
       DELETE-BOOK.
           DISPLAY "Enter the title of the book to delete: "
           ACCEPT WS-DeleteTitle
           MOVE WS-DeleteTitle TO BookTitle
           DELETE BorrowedBooksFile
               INVALID KEY
                   DISPLAY "Error: Book not found."
               NOT INVALID KEY
                   DISPLAY "Book successfully deleted."
                   PERFORM DISPLAY-BOOKS  *> Call to display updated list

           END-DELETE.
      
        UPDATE-BOOK.
           DISPLAY "Enter the title of the book to update: "
           ACCEPT BookTitle
           START BorrowedBooksFile KEY IS EQUAL TO BookTitle
               INVALID KEY
                   DISPLAY "Book with title " BookTitle " not found."
               NOT INVALID KEY
                   DISPLAY "Current details of the book:"
                   DISPLAY "Title: " BookTitle
                   DISPLAY "Author: " Author-Name
                   DISPLAY "Borrower: " BorrowerName
                   DISPLAY "Date Borrowed: " DateBorrowed
                   DISPLAY "--------------------------"
                   DISPLAY "--------------------------"
                   DISPLAY "Enter new author name: "
                   ACCEPT New-Author
                   IF New-Author NOT = SPACE
                       MOVE New-Author TO Author-name
                   END-IF

                   DISPLAY "Enter new borrower name: "
                   ACCEPT New-BorrowerName
                   IF New-BorrowerName NOT = SPACE
                       MOVE New-BorrowerName TO BorrowerName
                   END-IF

                   DISPLAY "Enter new date borrowed: "
                   ACCEPT New-DateBorrowed
                   IF New-DateBorrowed NOT = SPACE
                       MOVE New-DateBorrowed TO DateBorrowed
                   END-IF

                   REWRITE BorrowedBookRecord
                       INVALID KEY
                           DISPLAY "Error updating book record."
                       NOT INVALID KEY
                           DISPLAY "Book record updated successfully."
                   END-REWRITE.

       SEARCH-BOOK.
           DISPLAY "Enter book title to search: "
           ACCEPT BookTitle
           MOVE 'N' TO BOOKFOUND
           MOVE 'N' TO EOF
           DISPLAY "Searching for book with title: " BookTitle

           START BorrowedBooksFile KEY IS EQUAL TO BookTitle
               INVALID KEY
                   DISPLAY "Book not found."
               NOT INVALID KEY
                   READ BorrowedBooksFile INTO BorrowedBookRecord
                       AT END
                           MOVE 'Y' TO EOF
                       NOT AT END
                           DISPLAY "Book found!"
                           DISPLAY "Title: " BookTitle
                           DISPLAY "Author: " Author-Name
                           DISPLAY "Borrower: " BorrowerName
                           DISPLAY "Date Borrowed: " DateBorrowed
                           MOVE 'Y' TO BOOKFOUND
                   END-READ
           END-START.

           IF BOOKFOUND = 'N'
               DISPLAY "No book record matches the title."
           END-IF.



       END PROGRAM BorrowedBooks.
