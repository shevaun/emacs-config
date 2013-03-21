(fset 'wrap_with_it
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([105 116 32 123 32 5 32 125 19 115 104 111 117 108 100 13 M-left] 0 "%d")) arg)))

(fset 'replace_context_with_describe
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217828 100 101 115 99 114 105 98 101 right 34 35 kp-delete 19 32 13 left 34 19 99 111 110 116 101 120 116 13 M-left] 0 "%d")) arg)))
