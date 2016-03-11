
data BookInfo = Book Int String [String]
                deriving (Show)



data MagazineInfo = Magazine Int String [String]
                    deriving (Show)


type CustomerID = Int        --- Type synonym 
type ReviewBody = String 
type Address = [String] 

data BookReview = BookReview BookInfo CustomerID String 

data BetterReviw = BetterReviw BookInfo CustomerID ReviewBody 

type BookRecord = (BookInfo, BookReview) 

bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors


data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
              ["Richard Bird", "Oege de Moor"]

customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]

customer2 = Customer {
              customerID = 271828
            , customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            , customerName = "Jane Q. Citizen"
            }
