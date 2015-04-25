
do1test = do
    c <- getChar 
    putChar 'x'
    putChar c
    putChar '\n'

make_string :: Char -> String
make_string achar = "\nThe character is : " ++ [achar]

do2test = do
    let mychar = 'U'
    c <- getChar     
    putStrLn (make_string c)
    putChar mychar
    putChar '\n'


do3test = do   
    c <- getChar     
    let phrase = make_string c
    putStrLn phrase   
    putChar '\n'

doReturn = do
    c <- getChar
    let test = c == 'y'
    return test
