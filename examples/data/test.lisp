
(:dir "root"
      (:file "a" "abcdefghijklmnopqrstuvwxyz")
      (:file "b" "123456789012345678901234567890")
      (:file "c" "")
      (:file "d" "456")
      (:dir "dir1"
            (:file "k" "1a2b3c")
            (:file "a" "")
            (:file "b" "a")
            (:file "c" "c"))
      (:dir "dir2")
      (:link "f" "c")
      (:dir "dir3"
            (:file "a" "12345"))
      (:link "g" "g") ; circular link
      (:link "h" "/usr")
      )

