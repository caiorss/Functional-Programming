;; Must be used with Kawa
;; 
;; Based on: http://www.cs.dartmouth.edu/~cs5/lectures/0509/FahrenheitGUI.java 
;; 
;;
;;
(module-name <myprogram>)
(module-compile-options main: #t)

;; import java.awt.*;
;; import java.awt.event.*;
;;import javax.swing.*;
;;

(define JFrame javax.swing.JFrame)
(define JLabel javax.swing.JLabel)
(define JPanel javax.swing.JPanel)
(define JTextField javax.swing.JTextField)

(define WIDTH 300)
(define HEIGHT 75)

(define (fahrenheit2celcius f) (* (- f 32.0) (/ 5.0 9.0)))

;; frame = new JFrame("Temperature Conversion");
;;
(define frame (JFrame "Temperature Conversion"))

;;frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
;;
(frame:setDefaultCloseOperation JFrame:EXIT_ON_CLOSE)

(define inputLabel (JLabel "Enter Fahrenheit temperature (Hit Return to Compute the temperature):"))

;;JLabel outputLabel = new JLabel("Temperature in Celsius: ") 
(define outputLabel (JLabel "Temperature in Celsius: ")) 

(define resultLabel (JLabel "---"))  
 
;; fahrenheit = new JTextField(5); 
;; 
(define fahrenheit (JTextField 5))  
  
;; panel = new JPanel(); 
;; 
(define panel (JPanel))

;; panel.setPreferredSize(new Dimension(WIDTH, HEIGHT));
(panel:setPreferredSize (java.awt.Dimension WIDTH HEIGHT))
(panel:setBackground java.awt.Color:yellow)

;;(panel:setPreferredSize (new Dimension(WIDTH, HEIGHT))

(panel:add inputLabel)
(panel:add fahrenheit)
(panel:add outputLabel)
(panel:add resultLabel)
(frame:add panel)
(frame:pack)

(define show_frame (lambda () (frame:setVisible #t)))


(define (update-temperature)   
   (resultLabel:setText 
     (number->string (fahrenheit2celcius (string->number (fahrenheit:getText))))))

(define-namespace ActionListener <java.awt.event.ActionListener>)
(define-namespace ActionEvent <java.awt.event.ActionEvent>)


; ; fahrenheit.addActionListener(new TempListener());    
;;(frame:addActionListener (lambda (evt) (update-temperature)))
;;
(define action-listener
  (object (ActionListener)
    ((action-performed e :: ActionEvent) :: <void>
      (update-temperature))))

(fahrenheit:addActionListener action-listener)

(show_frame)
