;; Creates Java GUI
;;
;; Similar to: http://www.wideskills.com/java-tutorial/java-swing-tutorial 
;; 
(define JFrame javax.swing.JFrame)
(define JLabel javax.swing.JLabel)

(define jlbHelloWorld (JLabel "Hello World Kawa"))

(define HelloWorldFrame (JFrame))

(HelloWorldFrame:add jlbHelloWorld)
(HelloWorldFrame:setSize 100 100)
(HelloWorldFrame:setVisible #t)
