(defun beforep (x y l)
  "Returns true if x appears before Y in L"
  (member y (member x l)))

(defun make-palindrome (list1)
  "Makes a palindrome, takes a list as input and returns an appended list"
  (append list1 (reverse list1)))

(defun my-butlast (list1)
  "Returns a copy of the list given without the last element"
  (reverse (rest (reverse list1))))

(defun palindromep (list1)
  "Checks if a list is a palindrome"
  (equalp list1 (reverse list1)))

(defun next-to-last-2 (list1)
  "Returns the next to last element of a list, using or"
  (let ((length-list (length list1)))
    (or (and (<= length-list 2) (nth 0 list1))
        (nth (- length-list 2 ) list1))))

(defun next-to-last (list1)
  "Returns the next to last element of a list"
  (second (reverse list1)))

(defun last-element-3 (list1)
  "Returns the last element of a list, using nth"
  (nth (- (length list1) 1) list1))

(defun last-element-2 (list1)
  "Returns the last element of a list, using reverse"
  (first (reverse list1)))

(defun last-element (list1)
  "Returns the last element of a list"
  (car (last list1)))

(defun throw-die ()
  "Function that simulates a dice throw, returns a random number between 1 in 6 inclusively"
  (let ((die-number (random 7)))
    (if (equal die-number 0) 1 die-number)))

(defun try-for-point (point)
  (let* ((dice-throw (throw-dice))
         (first-die (first dice-throw))
         (second-die (second dice-throw))
         (sum-of-dice (+ first-die second-die)))
    (list 'THROW first-die 'AND second-die '-- sum-of-dice
          (or (and (equal sum-of-dice 7) 'YOU-LOSE)
              (and (equal sum-of-dice point) 'YOU-WIN)
              'THROW-AGAIN))))

(defun craps (dice-throw)
  (let ((first-die (first dice-throw))
        (second-die (second dice-throw))
        (throw (say-throw dice-throw)))
    (list 'THROW
          first-die
          'AND
          second-die
          '-- throw
          '--
          (or (and (instant-loss-p dice-throw) 'YOU-LOSE)
              (and (instant-win-p dice-throw) 'YOU-WIN)
              (list 'POINT throw)))))

(defun say-throw (dice-throw)
  "Function that returns symbol snake-eyes if the sum of the dice is 2, boxcars if it is 12 or the sum of the dice"
  (let  ((sum-of-dice (+ (first dice-throw) (second dice-throw))))
    (or (and (equalp sum-of-dice 12) 'BOXCARS)
        (and (equalp sum-of-dice 2) 'SNAKE-EYES)
        sum-of-dice)))

(defun instant-loss-p (dice-throw)
  "Functions that calculates the sum of dice throw and checks if it is equal to 2, 3, or 12"
  (let ((sum-of-dice (+ (first dice-throw) (second dice-throw))))
    (or (equal sum-of-dice 2) (equal sum-of-dice 3) (equal sum-of-dice 12))))

(defun instant-win-p (dice-throw)
  "Function that detects if the sum of the dice throw equals to 7 or 11"
  (let ((sum-of-dice (+ (first dice-throw) (second dice-throw))))
    (or (equal sum-of-dice 7) (equal sum-of-dice 11))))

(defun boxcars-p (dice-throw)
  "Function that checks if the dice throw was a boxcars"
  (let ((first-die (first dice-throw))
        (second-die (second dice-throw)))
    (and (equal first-die 6) (equal first-die second-die))))

(defun snake-eyes-p (dice-throw)
  "Function that checks if the dice throw was a snake eyes"
  (let ((first-die (first dice-throw))
        (second-die (second dice-throw)))
    (and (equal first-die 1) (equal first-die second-die))))

(defun throw-dice ()
  "Function that simulates a dice throw, returns a list containing 2 elements of type number between 1 and 6"
  (list (throw-die) (throw-die)))

(defun price-change (old new)
  (let* ((diff (- new old))
         (proportion (/ diff old))
         (percentage (* proportion 100.0)))
    (list 'widgets
          'changed
          'by
          percentage
          'percent)))

(defun add-vowels (list1)
  (union list1 '(A E I O U)))

(defun my-subsetp (subset set)
  (not (set-difference subset set)))

(defvar a '(SOAP WATER))

(defun set-equal (set1 set2)
  (and (subsetp set1 set2)
       (subsetp set2 set1)))

(defun proper-subsetp (set1 set2)
  (if (set-equal set1 set2) nil
      (subsetp set1 set2)))

(defun right-side (set1)
  (rest (member '-vs- set1)))

(defun left-side (set1)
  (right-side (reverse set1)))

(defun count-common (set1 set2)
  (let ((words-in-common (intersection set1 set2)))
    (or (and (null words-in-common) 0)
        (length words-in-common))))

(defun compare (list1)
  (if (member '-vs- list1)
      (list (count-common (right-side list1)
                          (left-side list1))
            'COMMON 'FEATURES)))

(defvar books '((WAR-AND-PEACE LEO-TOLSTOY)
                (THE-ILIAD HOMER)
                (HISTORIES HERODOTUS)
                (THE-PURPLE-COW SETH-GODIN)
                (THE-ODYSSEY EDITH-HAMILTON)))

(defun who-wrote (book-name)
  (cadr (assoc book-name books)))

(defvar ATLAS '((pennsylvania pittsburgh johnstown)
               (new-jersey newark princeton trenton)
                (ohio columbus)))

(defvar nerd-states
  '((Sleeping Eating)
    (Eating Waiting-for-a-computer)
    (Waiting-for-a-computer Programming)
    (Programming Debugging)
    (Debugging Sleeping)))

(defun nerdus (state)
  (second (assoc state nerd-states)))

(defun sleepless-nerd (state)
  (if (equalp state 'debugging) (nerdus 'sleeping)
      (nerdus state)))

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))

(defun swap-first-last (list1)
  (let ((first-element (first list1))
        (last-element (first (last list1))))
    (reverse (cons first-element (rest (reverse (cons last-element (rest list1))))))))

(defun swap-first-last-2 (list1)
  (let ((first-element (first list1))
        (last-element (first (last list1))))
    (append (cons last-element (remove last-element (rest list1))) (list first-element))))

(defun rotate-left (list1)
  (reverse (cons (first list1) (reverse (rest list1)))))

(defun rotate-left-2 (list1)
  (append (rest list1) (list (first list1))))

(defun rotate-right (list1)
     (let ((last-element (first (last list1))))
       (cons last-element (remove last-element list1))))

(defvar rooms
  '((living-room
     (north front-stairs)
     (south dining-room)
     (east kitchen))
    (upstairs-bedroom
     (west library)
     (south pantry))
    (dining-room
     (north living-room)
     (east pantry)
     (west downstairs-bedroom))
    (kitchen
     (west living-room)
     (south pantry))
    (pantry
     (north kitchen)
     (west dining-room))
    (downstairs-bedroom
     (north back-stairs)
     (east dining-room))
    (back-stairs
     (south downstairs-bedroom)
     (north library))
    (front-stairs
     (north upstairs-bedroom)
     (south living-room))
    (library
     (east upstairs-bedroom)
     (south back-stairs)))
  )

(defun choices (room-name)
  (rest (assoc room-name rooms)))

(defun look (direction room)
  (second (assoc direction (choices room))))

(defvar loc)

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting the variable LOC."
  (setf loc place))

(defun how-many-choices ()  (length (choices loc)))

(defun upstairsp (location)
  (subsetp (list location) '(library upstairs-bedroom)))

(defun onstairsp (location)
  (subsetp (list location) '(front-stairs back-stairs)))

(defun where ()
  (cond ((upstairsp loc) (list 'ROBBIE 'IS 'UPSTAIRS 'IN 'THE loc))
        ((onstairsp loc) (list 'ROBBIE 'IS 'ON 'THE loc))
        (t (list 'ROBBIE 'IS 'DOWNSTAIRS 'IN 'THE loc))))

(defun move (direction)
  (if (null (look direction loc)) (list 'OUCH! 'ROBBIE 'HIT 'A 'WALL)
      (and (set-robbie-location (look direction loc))  (where))))

#|| CL-USER> (set-robbie-location 'pantry)
PANTRY
CL-USER> (move 'west)
(ROBBIE IS DOWNSTAIRS IN THE DINING-ROOM)
CL-USER> (move 'north)
(ROBBIE IS DOWNSTAIRS IN THE LIVING-ROOM)
CL-USER> (move 'north)
(ROBBIE IS ON THE FRONT-STAIRS)
CL-USER> (move 'north)
(ROBBIE IS UPSTAIRS IN THE UPSTAIRS-BEDROOM)
CL-USER> (move 'west)
(ROBBIE IS UPSTAIRS IN THE LIBRARY) ||#

(defun royal-we (list1)
  (subst 'we 'i list1))

(defun royal-we-2 (list1)
  (sublis '((i . we)) list1))

#||(defun add1 (n)
(+ n 1)) ||#


(mapcar #'add1 '(13 5 7 9))

(mapcar #'(lambda (n) (+ 1 n)) '(13 5 7 9))

(defvar daily-planet '((olsen jimmy 123-76-4535 cub-reporter)
                       (kent clark 089-52-67-87 reporter)
                       (lane lois 951-26-1438 reporter)
                       (white perry 355-16-7439 editor)))

(mapcar #'caddr daily-planet)

(mapcar #'third daily-planet)

(mapcar #'(lambda (list1) (nth 2 list1)) daily-planet)

(mapcar #'zerop '(20340 -5 -6))

(defun greater-than-five-p (number)
  (> number 5))

(mapcar #'greater-than-five-p '(20 -5 0 4))

(mapcar #'(lambda (n) (or (equal t n) (equal nil n))) '(t hi nil 34 67))

(mapcar #'(lambda (element) (if (equal element 'UP) 'DOWN 'UP)) '(DOWN UP DOWN DOWN))

(defun roughly-equal-to-k (x k)
  (find-if #'(lambda (num)
               (and (>= num (- k 10)) (<= num (+ k 10)) num)) x))

(defun find-nested (nested-list)
  (find-if #'(lambda (list1) (and (not (null list1)) list1)) nested-list))

(defvar note-table '((C 1) (C-SHARP 2) (D 3)
                     (D-SHARP 4) (E 5) (F 6)
                     (F-SHARP 7) (G 8) (G-SHARP 9)
                     (A 10) (A-SHARP 11) (B 12)))

(defun numbers (notes-list)
  (mapcar #'(lambda (number) (second (assoc number note-table))) notes-list))

(defun notes (numbers-list)
  (mapcar #'(lambda (number)
              (first (find-if #'(lambda (note-pair)
                                  (member number note-pair)) note-table))) numbers-list))

(defun raise (half-steps numbers-list)
  (mapcar #'(lambda (number) (+ number half-steps))  numbers-list))

(defun normalize (number-list)
  (mapcar #'(lambda (number) (cond ((> number 12) (- number 12))
                                   ((< number 1) (+ number 12))
                                   (t number))) number-list))

(defun transpose (half-steps song)
  (notes (normalize (raise half-steps (numbers song)))))

(defun numbers-between-1-and-5 (numbers-list)
  (remove-if-not #'(lambda (number)
                     (and (> number 1) (< number 5))) numbers-list))

(defun count-the (word-list)
  (length (remove-if-not #'(lambda (word)
                             (equal word 'the)) word-list)))

(defun get-list-with-two-elements (list-of-lists)
  (remove-if-not #'(lambda (list1) (equal 2 (length list1))) list-of-lists))

(defun my-intersec (list1 list2)
  (remove-if-not #'(lambda (element) (member element list2)) list1))

(defun my-union (list1 list2)
  (append list1 (remove-if #'(lambda (element)
                               (member element list1)) list2)))

(defun get-card-rank (card)
  (first card))

(defun get-card-suit (card)
  (second card))

(defvar my-hand '((3 hearts)
                 (5 clubs)
                 (2 diamonds)
                 (4 diamonds)
                 (ace spades)
                  ))

(defun count-suit (suit hand-of-cards)
  (length (remove-if-not
           #'(lambda (card) (equal suit (get-card-suit card)))
           hand-of-cards)))

(defvar colors '((clubs black)
                 (diamonds red)
                 (hearts red)
                 (spades black)))

(defun color-of (card)
  (second (assoc (get-card-suit card) colors)))

(defun get-first-red-card (hand-of-cards)
  (find-if #'(lambda (card) (equal 'red (color-of card))) hand-of-cards))

(defun get-black-cards (hand-of-cards)
  (remove-if-not #'(lambda (card) (equal 'black (color-of card))) hand-of-cards))

(defun get-cards-of-a-suit (suit hand-of-cards)
  (remove-if-not #'(lambda (card) (equal suit (get-card-suit card))) hand-of-cards))

(defun what-ranks (suit hand-of-cards)
  (mapcar #'(lambda (card) (first card)) (get-cards-of-a-suit suit hand-of-cards)))


(defun get-ranks (hand-of-cards)
  (mapcar #'(lambda (card) (first card)) hand-of-cards))

(defvar all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (card-1 card-2)
  (not (member (first card-2) (member (first card-1) all-ranks))))

(defun high-card (hand-of-cards)
  (assoc (find-if #'(lambda (rank)
                      (assoc rank hand-of-cards))
                  (reverse all-ranks))
         hand-of-cards))

(defvar test '((A B C) (C D A) (F B D) (G)))

(defun get-total-length (list1)
  (reduce #'+ (mapcar #'length list1)))

(defun all-odd (list1)
  (every #'oddp list1))

(defun none-odd (list1)
  (every #'evenp list1))

(defun not-all-odd (list1)
  (not (all-odd list1)))

(defun not-none-odd (list1)
  (not (none-odd list1)))


(defvar database '((b1 shape brick)
                   (b1 color green)
                   (b1 size small)
                   (b1 supported-by b2)
                   (b1 supported-by b3)
                   (b2 shape black)
                   (b2 color red)
                   (b2 size small)
                   (b2 supports b1)
                   (b2 left-of b3)
                   (b3 shape brick)
                   (b3 color red)
                   (b3 size small)
                   (b3 supports b1)
                   (b3 right-of b2)
                   (b4 shape pyramid)
                   (b4 color blue)
                   (b4 size large)
                   (b4 supported-by b5)
                   (b5 shape cube)
                   (b5 color green)
                   (b5 size large)
                   (b5 supports b4)
                   (b6 shape brick)
                   (b6 color purple)
                   (b6 size large)) )


(defun match-element (symbol1 symbol2)
  (or (equal symbol1 symbol2) (equal symbol2 '?)))


(defun match-triple (assertion pattern)
  (let ((first-assertion-elmnt (first assertion)) (first-pattern-elmnt (first pattern))
        (second-assertion-elmnt (second assertion))
        (second-pattern-elmnt (second pattern))
        (third-assertion-elmnt (third assertion))
        (third-pattern-elmnt (third pattern)))

    (if (and (= 3 (length assertion)) (= 3 (length pattern))) (and (match-element first-assertion-elmnt first-pattern-elmnt)
                                                                   (match-element second-assertion-elmnt second-pattern-elmnt)
                                                                   (match-element third-assertion-elmnt third-pattern-elmnt)))
    ))

(defun fetch (pattern)
  (remove-if-not #'(lambda (assertion) (match-triple assertion pattern)) database))

(fetch '(b4 shape ?))

(fetch '(? shape brick))

(fetch '(B2 ? B3))

(fetch '(? COLOR ?))

(fetch '(B4 ? ?))

(defun ask-block-color (block-name)
  (list block-name 'COLOR '?))


(defun get-block-supporters (block-name)
  (mapcar #'(lambda (assertion) (third assertion) ) (fetch (list block-name 'supported-by '?))))

(defun supp-cube (block-name)
  (find-if #'(lambda (block) (equal 'cube (third (first (fetch (list block 'shape '?)))))) (get-block-supporters block-name)) )

(defun desc1 (block)
  (fetch (list block '? '?)))

(defun desc2 (block)
  (mapcar #'(lambda (assertion) (rest assertion)) (desc1 block)))

(defun description (block)
  (reduce #'append (desc2 block)))

(description 'B1)

(description 'B4)

(defvar words '((one un)
                (two deux)
                (three trois)
                (four quatre)
                (five cinq)))

(mapcar #'(lambda (english-and-french-words spanish-word) (append english-and-french-words (list spanish-word))) words '(UNO DOS TRES CUATRO CINCO))

(defun anyoddp (x)
  (cond ((null x) nil)
        ((oddp (first x)) t)
        (t (anyoddp (rest x)))))

(defun anyoddp-if (x)
  (if (null x) nil (if (oddp (first x)) t (anyoddp (rest x)))))


(defun fact (number)
  (cond ((zerop number) 1)
        (t (* number (fact (- number 1))))))

(defun laugh (number)
  (cond ((zerop number) nil)
        (t (cons 'laugh (laugh (- number 1))))))

(defun add-up (list-of-numbers)
  (cond ((null list-of-numbers) 0)
        (t (+ (first list-of-numbers) (add-up (rest list-of-numbers))))))

(defun allodp (list-of-numbers)
  (cond ((null list-of-numbers) t)
        ((not (oddp (first list-of-numbers))) nil)
        (t (allodp (rest list-of-numbers)))))

(defun rec-member (item sequence)
  (cond ((null sequence) nil)
        ((equal (first sequence) item) sequence)
        (t (rec-member item (rest sequence)))))

(defun rec-assoc (item table)
  (cond ((null table) nil)
        ((equal item (first (first table))) (first table))
        (t (rec-assoc item (rest table)))))

(defun rec-nth (number list)
  (cond ((zerop number) (first list))
        (t (rec-nth (- number 1) (rest list)))))

(defun add1 (number)
  (+ number 1))

(defun sub1 (number)
  (- number 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (rec-plus (add1 x ) (sub1 y)))))

(defun fib (number)
  (cond ((< number 2) 1)
        (t ( + (fib (- number 1)) (fib (- number 2))))))

(defun any-7-p (x)
  (cond ((equal (first x) 7) t)
        (t (any-7-p (rest x)))))

(defun shortest-inf-func ()
  (shortest-inf-func))

(defun find-first-odd (list1)
  (cond ((null list1) nil)
        ((oddp (first list1))) (first list1)
        (t (find-first-odd (rest list1)))))

(defun add-nums (number)
  (cond ((zerop number) 0)
        (t (+ number (add-nums (- number 1))))))

(defun all-equal (list1)
  (cond ((< (length list1) 2) t)
        ((not (equal (first list1) (second list1))) nil)
        (t (all-equal (rest list1)))))

(defun count-down (number)
  (cond ((zerop number) nil)
        (t (cons number (count-down (- number 1))))))

(defun fact-count-down (number)
  (reduce #'* (count-down number)))

(defun count-down-modified (number)
  (cond ((zerop number) (cons number nil))
        (t (cons number (count-down-modified (- number 1))))))

(defun count-down-modified-2 (number)
  (cond ((< number 0) nil)
        (t (cons number (count-down-modified (- number 1))))))

(defun square-list (list-of-numbers)
  (cond ((null list-of-numbers) nil)
        (t (cons (* (first list-of-numbers) (first list-of-numbers)) (square-list (rest list-of-numbers))))))

(defun my-nth (n x)
  (cond ((null x) nil)
        ((zerop n) (first x))
        (t (my-nth (- n 1) (rest x)))))

(defun compare-lengths (list1 list2)
  (cond ((and (null list1) (null list2)) 'SAME-LENGTH)
        ((null list2) 'FIRST-IS-LONGER)
        ((null list1) 'SECOND-IS-LONGER)
        (t (compare-lengths (rest list1) (rest list2)))))

(defun sum-numeric-elements (list-of-items)
  (cond ((null list-of-items) 0)
        ((numberp (first list-of-items)) (+ (first list-of-items)
                                            (sum-numeric-elements (rest list-of-items))))
        (t (sum-numeric-elements (rest list-of-items)))))

(defun my-remove (it
                  em sequence)
  (cond ((null sequence) nil)
        ((not (equal item (first sequence))) (cons (first sequence) (my-remove item (rest sequence))))
        (t (my-remove item (rest sequence)))))

(defun my-intersection (list1 list2)
  (cond ((or (null list1) (null list2)) nil)
        ((member (first list1) list2) (cons (first list1) (my-intersection (rest list1) list2)))
        (t (my-intersection (rest list1) list2))))

(defun my-set-difference (set1 set2)
  (cond ((null set1) nil)
        ((not (member (first set1) set2)) (cons (first set1) (my-set-difference (rest set1) set2)))
        (t (my-set-difference (rest set1) set2))))

(defun count-odd (list-of-numbers)
  (cond ((null list-of-numbers) 0)
        ((oddp (first list-of-numbers)) (+ 1 (count-odd (rest list-of-numbers))))
        (t (count-odd (rest list-of-numbers)))))

(defun count-odd-2 (list-of-numbers)
  (cond ((null list-of-numbers) 0)
        ((evenp (first list-of-numbers)) (count-odd-2 (rest list-of-numbers)))
        (t (+ 1 (count-odd-2 (rest list-of-numbers))))))

(defun combine (number1 number2)
  (+ number1 number2))

(defun fib-combine (number)
  (cond ((< number 2) 1)
        (t ( combine (fib (- number 1)) (fib (- number 2))))))

(defun atoms-to-q (x)
  (cond ((null x) nil)
        ((atom x) 'q)
        (t (cons (atoms-to-q (car x)) (atoms-to-q (cdr x))))))

(defun count-atoms (tree)
  (cond ((atom x) 1)
        (t (+ (count-atoms (car x)) (count-atoms (cdr x))))))

(defun count-cons (tree)
  (cond ((atom tree) 0)
        (t (+ 1 (count-cons (car tree)) (count-cons (cdr tree))))))

(defun sum-tree (tree)
  (cond ((or (null tree) (and (atom tree) (not (numberp tree)))) 0)
        ((and (atom tree) (numberp tree)) tree)
        (t (+ (sum-tree (car tree)) (sum-tree (cdr tree))))))

(defun my-subst (x y z)
  (cond ((and (atom z) (not (equal z y))) z)
        ((and (atom z) (equal z y)) x)
        (t (cons (my-subst x y (car z)) (my-subst x y (cdr z))))))

(defun flatten (nested-list)
  (cond ((null nested-list) nil)
        ((atom (car nested-list)) (cons (car nested-list) (flatten (cdr nested-list))))
        (t (append (flatten (car nested-list)) (flatten (cdr nested-list))))))

(defun tree-depth (tree)
  (cond ((atom tree) 0)
        (t (max (+ 1 (tree-depth (car tree))) (+ 1 (tree-depth(cdr tree)))))))

(defun paren-depth (tree)
  (cond ((atom tree) 0)
        (t (max (+ 1 (paren-depth (car tree))) (paren-depth (cdr tree))))))

(defun count-up ( number)
  (cond ((= number 0) nil)
        (t  (append (count-up (- number 1)) (cons number nil)))))

(defun make-loaf (number)
  ( if (= number 0) nil
       (append (make-loaf (- number 1)) (cons 'x nil))))

(defun bury (item paren-numbers)
  (cond ((= paren-numbers 0) item)
        (t (cons (bury item (- paren-numbers 1)) nil))))

(defun pairings (list-of-items1 list-of-items2)
  (cond ((null list-of-items1) nil)
        (t (cons (append (cons (car list-of-items1) nil) (cons (car list-of-items2) nil))  (pairings (cdr list-of-items1) (cdr list-of-items2))))))

(defun sublists (list-of-items-1)
  (cond ((null list-of-items-1) nil)
        (t (cons list-of-items-1 (sublists (cdr list-of-items-1))))))

(defun my-union (set1 set2)
  (cond ((null set2) set1)
        ((not (member (car set2) set1)) (append (my-union set1 (cdr set2)) (cons (car set2) nil)))
        (t (my-union set1 (cdr set2)))))

(defun largest-even (set1)
  (cond ((null set1) 0)
        (t (max (car set1) (largest-even (cdr set1))))))
 
(defun huge-recursive (number1 counter)
  (cond ((= counter number1) 1)
        (t (* number1 (huge-recursive number1 (+ 1 counter))))))

(defun huge (number)
  (huge-recursive number 0))

(defun every-other-recursive (n set1)
  (cond ((null set1) nil)
        ((oddp n) (cons (car set1) (every-other-recursive (+ 1 n) (cdr set1))))
        (t (every-other-recursive (+ 1 n) (cdr set1)))))

(defun every-other (set1)
  (every-other-recursive 1 set1))

(defun left-half-recursive (num set1)
  (cond((>= num (/ (length set1) 2)) nil)
       (t (cons (nth num set1) (left-half-recursive (+ 1 num) set1)))))

(defun left-half (set1)
  (left-half-recursive 0 set1))

(defun merge-lists (set1 set2)
  (cond ((null set1) set2)
        ((null set2) set1)
        ((> (first set2) (first set1)) (append (cons (first set1) nil)  (merge-lists (cdr set1) set2)))
        ((< (first set2) (first set1)) (append (cons (first set2) nil) (merge-lists set1 (cdr set2))))
        (t (append (cons (first set2) nil) (merge-lists (cdr set1) set2)))))

(defun diff-factorial (num)
  (cond ((zerop num) 1)
        (t  (/ (diff-factorial (+ 1 num)) (+ 1 num)))))

(defvar family '((colin nil nil)
                 (deidre nil nil)
                 (arthur nil nil)
                 (kate nil nil)
                 (frank nil nil)
                 (linda nil nil)
                 (suzanne colin deidre)
                 (bruce arthur kate)
                 (charles arthur kate)
                 (david arthur kate)
                 (ellen arthur kate)
                 (george frank linda)
                 (hillary frank linda)
                 (andre nil nil)
                 (tamara bruce suzanne)
                 (vincent bruce suzanne)
                 (wanda nil nil)
                 (ivan george ellen)
                 (julie george ellen)
                 (marie george ellen)
                 (nigel andre hillary)
                 (frederick nil tamara)
                 (zelda vincent wanda)
                 (joshua ivan wanda)
                 (quentin nil nil)
                 (robert quentin julie)
                 (olivia nigel marie)
                 (peter nigel marie)
                 (erica nil nil)
                 (yvette robert zelda)
                 (diane peter erica)))

(defun get-father (child-name)
  (second (assoc child-name family)))

(defun get-mother (child-name)
  (third (assoc child-name family)))

(defun get-parents (child-name)
  (remove-if #'null (rest (assoc child-name family))))

(defun get-children (parent-name)
  (if (not (null parent-name)) (mapcar #'first (remove-if-not #'(lambda (listx) (and (not (equal (first listx) parent-name)) (member parent-name listx))) family))))

(defun get-siblings (person-name)
  (remove-if
   #'(lambda (name) (equal name person-name)) (reduce #'append (rest(mapcar #'(lambda (parent-name) (get-children parent-name)) (get-parents person-name))))))

(defun get-siblings (person-name)
  (remove person-name (union (get-children (get-father person-name))
                             (get-children (get-mother person-name)))))
