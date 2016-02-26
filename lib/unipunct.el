(require 'quail)

;; Modified winkeys, punctuation is the same as english
(quail-define-package
 "russian-unipunct" "Russian" "RU" nil
 "ХБУКЕ Russian computer layout"
 nil t t t t nil nil nil nil nil t)

;;  1! 2" 3№ 4; 5% 6: 7? 8* 9( 0) -_ =+ \/ ёЁ
;;   Й  Ц  У  К  Е  Н  Г  Ш  Щ  З  Х  Ъ
;;    Ф  Ы  В  А  П  Р  О  Л  Д  Ж  Э
;;     Я  Ч  С  М  И  Т  Ь  Б  Ю  .,

(quail-define-rules
 ("1" ?1) ("2" ?2) ("3" ?3) ("4" ?4) ("5" ?5) ("6" ?6) ("7" ?7) ("8" ?8) ("9" ?9) ("0" ?0) ("-" ?-) ("=" ?=)
 ("|" ?|)
 ("`" ?’)
 ("q" ?х) ("w" ?б) ("e" ?у) ("r" ?к) ("t" ?е) ("y" ?н) ("u" ?г) ("i" ?ш) ("o" ?ж) ("p" ?з) ;; ("[" ?\[) ;; ("]" ?\])
 ("a" ?ф) ("s" ?ы) ("d" ?в) ("f" ?а) ("g" ?п) ("h" ?р) ("j" ?о) ("k" ?л) ("l" ?д) (";" ?\;) ("'" ?') ("\\" ?\\)
 ("z" ?я) ("x" ?ч) ("c" ?с) ("v" ?м) ("b" ?и) ("n" ?т) ("m" ?ь) ("," ?,) ("." ?.) ("/" ?/)
 ("!" ?!)
 ("@" ?@)
 ("#" ?#)
 ("$" ?$)
 ("%" ?%)
 ("^" ?^)
 ("&" ?&)
 ("*" ?*)
 ("(" ?\()
 (")" ?\))
 ("_" ?_)
 ("+" ?+)
 ("~" ?~)
 ("Q" ?Х)
 ("W" ?Б)
 ("E" ?У)
 ("R" ?К)
 ("T" ?Е)
 ("Y" ?Н)
 ("U" ?Г)
 ("I" ?Ш)
 ("O" ?Щ)
 ("P" ?З)
 ("{" ?{)
 ("}" ?})
 ("A" ?Ф)
 ("S" ?Ы)
 ("D" ?В)
 ("F" ?А)
 ("G" ?П)
 ("H" ?Р)
 ("J" ?О)
 ("K" ?Л)
 ("L" ?Д)
(":" ?:)
("\"" ?\")
("|" ?|)
 ("Z" ?Я)
 ("X" ?Ч)
 ("C" ?С)
 ("V" ?М)
 ("B" ?И)
 ("N" ?Т)
 ("M" ?Ь)
 ("<" ?<)
 (">" ?>)
 ("?" ??))

(set-language-info-alist
 "Russian-Uni" `((charset cyrillic-iso8859-5)
                 (nonascii-translation
                  . ,(get 'cyrillic-koi8-r-nonascii-translation-table
                          'translation-table))
                 (coding-system utf-8-unix)
                 (coding-priority utf-8-unix cyrillic-koi8 cyrillic-iso-8bit)
                 (input-method . "russian-unipunct")
                 (features cyril-util)
                 (unibyte-display . cyrillic-koi8)
                 (sample-text . "Russian (Русский)	Здравствуйте!")
                 (documentation . "\
Support for Russian using UTF8 and the russian-unipunct input method.")
                 (tutorial . "TUTORIAL.ru"))
 '("Cyrillic"))

(setq default-input-method "russian-unipunct")

(provide 'unipunct)
