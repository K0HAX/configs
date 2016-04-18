(require 'tls)

; M-x start-irc
(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "10.5.0.20" :port 6697
	   :nick "K0HAX" :full-name "Michael Englehorn"))
  
