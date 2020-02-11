;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: S-SQL-TESTS; -*-
(in-package :s-sql-tests)

(fiveam:def-suite :s-sql-arrays
    :description "Array suite for s-sql"
    :in :s-sql)

(fiveam:in-suite :s-sql-arrays)

(test basic-arrays
  "Testing sql arrays and comparing the sql generated by the postmodern call.
   Simple insert-rows-into a database table array and then selecting particular array items."
  (with-test-connection
      (is (equal (sql (:create-table array-provinces ((name :type text) (provinces :type text[]))))
                 "CREATE TABLE array_provinces (name TEXT NOT NULL, provinces TEXT[] NOT NULL)"))
      (is (equal (sql (:insert-rows-into 'array-provinces
                                         :columns 'name 'provinces
                                         :values '(("Germany" #("Baden-Wurttemberg" "Bavaria" "Berlin" "Brandenburg"))
                                                   ("Switzerland" #("Aargau" "Appenzell Ausserrhoden" "Basel-Landschaft" "Fribourg")))))
                 "INSERT INTO array_provinces (name, provinces) VALUES (E'Germany', ARRAY[E'Baden-Wurttemberg', E'Bavaria', E'Berlin', E'Brandenburg']), (E'Switzerland', ARRAY[E'Aargau', E'Appenzell Ausserrhoden', E'Basel-Landschaft', E'Fribourg'])"))
      (is (equal (sql (:insert-into 'array-provinces :set 'name "Canada" 'provinces #("Alberta" "British Columbia" "Manitoba" "Ontario")))
                 "INSERT INTO array_provinces (name, provinces)  VALUES (E'Canada', ARRAY[E'Alberta', E'British Columbia', E'Manitoba', E'Ontario'])"))
      (is (equal (sql (:select (:[] 'provinces 2) :from 'array-provinces))
                 "(SELECT (provinces)[2] FROM array_provinces)"))
      (is (equal (sql (:select (:[] 'provinces 1) :from 'array-provinces :where (:= 'name "Germany")))
                 "(SELECT (provinces)[1] FROM array_provinces WHERE (name = E'Germany'))"))
      (is (equal (sql (:select (:[] 'provinces 2) :from 'array-provinces :where (:= (:[] 'provinces 2) "Bavaria")))
                 "(SELECT (provinces)[2] FROM array_provinces WHERE ((provinces)[2] = E'Bavaria'))"))
      (is (equal (sql (:select '* :from 'array-provinces :where (:= (:[] 'provinces 2) "Bavaria")))
                 "(SELECT * FROM array_provinces WHERE ((provinces)[2] = E'Bavaria'))"))
      (is (equal (sql (:limit
        (:order-by
         (:select 'doc-id
                  :from 'doc-tags-array
                  :where (:@> 'tags (:array[] "math")))
         'doc-id)
        25 0))
                 "(((SELECT doc_id FROM doc_tags_array WHERE (tags @> ARRAY[E'math'])) ORDER BY doc_id) LIMIT 25 OFFSET 0)"))
      (is (equal (sql (:limit
        (:order-by
         (:with 'find-docs
                (:as
                 (:select 'doc-id
                          :from 'doc-tags-array
                          :where (:@> 'tags (:array[] "thrift shop" "blogging")))
                 (:select '* :from 'find-docs)))
         'doc-id)
        25))
                 "((WITH find_docs(SELECT doc_id FROM doc_tags_array WHERE (tags @> ARRAY[E'thrift shop', E'blogging'])) AS (SELECT * FROM find_docs) ORDER BY doc_id) LIMIT 25)"))))

(test multi-dimension-arrays
  "Testing multi-dimensional arrays and examining the sql call generated by the postmodern call."
      (is (equal (sql (:create-table sal-emp ((name :type text) (pay-by-quarter :type integer[]) (schedule :type  text[][]))))
                 "CREATE TABLE sal_emp (name TEXT NOT NULL, pay_by_quarter INTEGER[] NOT NULL, schedule TEXT[][] NOT NULL)"))
      (is (equal (sql (:insert-into 'sal-emp :set 'name "Bill" 'pay-by-quarter #(10000 10000 10000 10000) 'schedule #(#( "meeting" "lunch") #("training" "presentation"))))
                 "INSERT INTO sal_emp (name, pay_by_quarter, schedule)  VALUES (E'Bill', ARRAY[10000, 10000, 10000, 10000], ARRAY[ARRAY[E'meeting', E'lunch'], ARRAY[E'training', E'presentation']])"))
      (is (equal (sql (:select 'name :from 'sal-emp :where (:<> (:[] 'pay-by-quarter 1) (:[] 'pay-by-quarter 2))))
                 "(SELECT name FROM sal_emp WHERE ((pay_by_quarter)[1] <> (pay_by_quarter)[2]))"))
      (is (equal (sql (:update 'sal-emp :set 'schedule #(#( "breakfast" "consulting") #("meeting" "lunch"))
                               :where (:= 'name "Carol")))
                 "UPDATE sal_emp SET schedule = ARRAY[ARRAY[E'breakfast', E'consulting'], ARRAY[E'meeting', E'lunch']] WHERE (name = E'Carol')")))

(test use-of-arrays-tags-table
  "Create a table with arrays and then test the array operators and functions. Remember that lisp
equality tests with arrays requires equalp, not equal."
  (is (equal (sql (:create-table doc-tags-array ((doc-id :type integer :references ((documents doc-id)))
                                                 (tags :type text[] :default "{}"))
                                 (:unique 'index 'doc-id)))
             "CREATE TABLE doc_tags_array (doc_id INTEGER NOT NULL REFERENCES documents(doc_id) MATCH SIMPLE ON DELETE RESTRICT ON UPDATE RESTRICT, tags TEXT[] NOT NULL DEFAULT E'{}', UNIQUE (index, doc_id))"))
  (is (equal (sql (:create-table array_int ((vector :type (or int[][] db-null)))))
             "CREATE TABLE array_int (vector INT[][])"))
  (with-test-connection
    (query (:drop-table :if-exists 'receipes :cascade))
    (query (:drop-table :if-exists 'receipe-tags-array :cascade))
    (query (:create-table receipes
                          ((receipe-id :type serial :constraint 'receipekey-id :primary-key 't :unique)
                           (name :type text)
                           (text :type text))))

    (query (:create-table receipe-tags-array ((receipe-id :type integer :references ((receipes receipe-id)))
                                              (tags :type text[] :default "{}"))))

    (query (:create-unique-index 'receipe-tags-id-receipe-id :on "receipe-tags-array"  :fields 'receipe-id))
    (query (:create-index 'receipe-tags-id-tags :on "receipe-tags-array" :using gin :fields 'tags))


    (loop for x in '(("Fattoush" #("greens" "pita bread" "olive oil" "garlic" "lemon" "salt" "spices"))
                     ("Shawarma" #("meat" "tahini sauce" "pita bread"))
                     ("Baba Ghanoush" #("pita bread" "olive oil" "eggplant" "tahini sauce"))
                     ("Shish Taouk" #("chicken" "lemon juice" "garlic" "paprika" "yogurt" "tomato paste" "pita bread"))
                     ("Kibbe nayeh" #("raw meat" "bulgur" "onion" "spices" "pita bread"))
                     ("Manakeesh" #("meat" "cheese" "zaatar" "kishik" "tomatoes" "cucumbers" "mint leaves" "olives"))
                     ("Fakafek" #("chickpeas" "pita bread" "tahini sauce"))
                     ("Tabbouleh" #("bulgur" "tomatoes" "onions" "parsley"))
                     ("Kofta" #("minced meat" "parsley" "spices" "onions"))
                     ("Kunafeh" #("cheese" "sugar syrup" "pistachios"))
                     ("Baklava" #("filo dough" "honey" "nuts"))) do
         (query (:insert-into 'receipes :set 'name (first x) 'text (format nil "~a" (rest x))))
         (query
          (:insert-into 'receipe-tags-array
                        :set 'receipe-id
                        (:select 'receipe-id
                                 :from 'receipes
                                 :where (:= 'receipes.name (first x)))
                        'tags (second x))))

;;; 1. checking for records that have a specific tag
    (is (equal (query (:limit
                     (:order-by
                      (:select 'receipe-id 'name 'text
                               :from 'receipes
                               :where (:in 'receipe-id
                                           (:set
                                            (:select 'receipe-id
                                                     :from 'receipe-tags-array
                                                     :where (:@> 'tags (:array[] "bulgur"))))))
                      'receipe-id)
                     25 0))
             '((5 "Kibbe nayeh" "(#(raw meat bulgur onion spices pita bread))")
               (8 "Tabbouleh" "(#(bulgur tomatoes onions parsley))"))))

;;; 2. checking for records that have two specific tags
    (is (equalp (query (:limit
                       (:order-by
                        (:select '*
                                 :from 'receipes
                                 :where (:in 'receipe-id
                                             (:set
                                              (:select 'receipe-id
                                                       :from 'receipe-tags-array
                                                       :where (:@> 'tags (:array[] "bulgur" "parsley"))))))
                        'receipe-id)
                       25))
               '((8 "Tabbouleh" "(#(bulgur tomatoes onions parsley))"))))

;;; 3. Test the update array with an lisp array (changing onion to onions in the one row where it is singular
    (query (:update 'receipe-tags-array
                    :set 'tags #("raw meat" "bulgur" "onions" "spices" "pita bread")
                    :where (:= 'receipe-id 5)))

    (is (equalp (query (:select 'tags :from 'receipe-tags-array :where (:= 'receipe-id 5)))
               '((#("raw meat" "bulgur" "onions" "spices" "pita bread")))))

;;; 4. Checking array-length function
    (is (equalp (query (:select 'receipe-id (:array-length 'tags 1) 'tags
                               :from 'receipe-tags-array
                               :where (:= 'receipe-id 6)))
               '((6 8
                  #("meat" "cheese" "zaatar" "kishik" "tomatoes" "cucumbers" "mint leaves"
                    "olives")))))

;;; 5. checking cardinality functions (same as array-length)
    (is (equal (query (:select (:cardinality 'tags)
                                              :from 'receipe-tags-array
                                              :where (:= 'receipe-id 6))
                                     :single)
        8))

;;; 6. checking that it is returning an array for the tags
    (is (equalp (type-of (query (:select 'tags
                                        :from 'receipe-tags-array
                                        :where (:= 'receipe-id 6))
                               :single))
               '(SIMPLE-VECTOR 8)))

;;; 7. checking unnest, which expands every array entry into a separate row. In this case we will
;;; limit it to distinct items and order it for easy checking
    (is (equal (query (:order-by (:select (:as (:unnest 'tags) 'tag) :distinct
                                          :from 'receipe-tags-array)
                                 'tag))
               '(("bulgur") ("cheese") ("chicken") ("chickpeas") ("cucumbers") ("eggplant")
                 ("filo dough") ("garlic") ("greens") ("honey") ("kishik") ("lemon")
                 ("lemon juice") ("meat") ("minced meat") ("mint leaves") ("nuts")
                 ("olive oil") ("olives") ("onions") ("paprika") ("parsley")
                 ("pistachios") ("pita bread") ("raw meat") ("salt") ("spices") ("sugar syrup")
                 ("tahini sauce") ("tomatoes") ("tomato paste") ("yogurt") ("zaatar"))))

;;; 8 counting each unique tag

    (is (equal (query (:order-by (:with (:as 'p (:select (:as (:unnest 'tags) 'tag)
                                                         :from 'receipe-tags-array))
                                        (:select 'tag (:as (:count 'tag) 'cnt)
                                                 :from 'p :group-by 'tag))
                                 (:desc 'cnt) 'tag))
               '(("pita bread" 6) ("onions" 3) ("spices" 3) ("tahini sauce" 3) ("bulgur" 2)
                 ("cheese" 2) ("garlic" 2) ("meat" 2) ("olive oil" 2) ("parsley" 2)
                 ("tomatoes" 2) ("chicken" 1) ("chickpeas" 1) ("cucumbers" 1) ("eggplant" 1)
                 ("filo dough" 1) ("greens" 1) ("honey" 1) ("kishik" 1) ("lemon" 1)
                 ("lemon juice" 1) ("minced meat" 1) ("mint leaves" 1) ("nuts" 1) ("olives" 1)
                 ("paprika" 1) ("pistachios" 1) ("raw meat" 1) ("salt" 1) ("sugar syrup" 1)
                 ("tomato paste" 1) ("yogurt" 1) ("zaatar" 1))))

;;; 9. checking array-append function
    (query (:update 'receipe-tags-array :set 'tags (:array-append 'tags "appended-items")
                    :where (:= 'receipe-id 5)))

    (is (equalp (query (:select 'tags :from 'receipe-tags-array :where (:= 'receipe-id 5)))
               '((#("raw meat" "bulgur" "onions" "spices" "pita bread" "appended-items")))))

;;; Resetting the array in that row
    (query (:update 'receipe-tags-array
                    :set 'tags #("raw meat" "bulgur" "onions" "spices" "pita bread")
                    :where (:= 'receipe-id 5)))

;;; checking array-replace function
;;; this version checks all the rows, even those without the target string
    (query (:update 'receipe-tags-array :set 'tags (:array-replace 'tags "spices" "chocolate")))

    (is (equal (query (:order-by (:with (:as 'p (:select (:as (:unnest 'tags) 'tag)
                                                         :from 'receipe-tags-array))
                                        (:select 'tag (:as (:count 'tag) 'cnt)
                                                 :from 'p :group-by 'tag))
                                 (:desc 'cnt) 'tag))
               '(("pita bread" 6) ("chocolate" 3) ("onions" 3) ("tahini sauce" 3) ("bulgur" 2)
                 ("cheese" 2) ("garlic" 2) ("meat" 2) ("olive oil" 2) ("parsley" 2)
                 ("tomatoes" 2) ("chicken" 1) ("chickpeas" 1) ("cucumbers" 1) ("eggplant" 1)
                 ("filo dough" 1) ("greens" 1) ("honey" 1) ("kishik" 1) ("lemon" 1)
                 ("lemon juice" 1) ("minced meat" 1) ("mint leaves" 1) ("nuts" 1) ("olives" 1)
                 ("paprika" 1) ("pistachios" 1) ("raw meat" 1) ("salt" 1) ("sugar syrup" 1)
                 ("tomato paste" 1) ("yogurt" 1) ("zaatar" 1)) ))

;;; reseting chocolate back to spices using a different method that just updates the arrays with chocolate
;;; This is accomplished with the use of :<@
    (query (:update 'receipe-tags-array :set 'tags (:array-replace 'tags  "chocolate" "spices")
                    :where (:<@ "{\"chocolate\"}" 'tags)))

;;; checking the the reset happened correctly
    (is (equal (query (:order-by (:with (:as 'p (:select (:as (:unnest 'tags) 'tag)
                                                         :from 'receipe-tags-array))
                                        (:select 'tag (:as (:count 'tag) 'cnt)
                                                 :from 'p :group-by 'tag))
                                 (:desc 'cnt) 'tag))
               '(("pita bread" 6) ("onions" 3) ("spices" 3) ("tahini sauce" 3) ("bulgur" 2)
                 ("cheese" 2) ("garlic" 2) ("meat" 2) ("olive oil" 2) ("parsley" 2)
                 ("tomatoes" 2) ("chicken" 1) ("chickpeas" 1) ("cucumbers" 1) ("eggplant" 1)
                 ("filo dough" 1) ("greens" 1) ("honey" 1) ("kishik" 1) ("lemon" 1)
                 ("lemon juice" 1) ("minced meat" 1) ("mint leaves" 1) ("nuts" 1) ("olives" 1)
                 ("paprika" 1) ("pistachios" 1) ("raw meat" 1) ("salt" 1) ("sugar syrup" 1)
                 ("tomato paste" 1) ("yogurt" 1) ("zaatar" 1))))

;;; checking use of any and passing in an array. Note the need to use :any* instead of :any
    (is (equal (query (:select 'receipe-id 'name
                               :from 'receipes
                               :where (:= 'name (:any* #("Trout" "Shish Taouk" "Hamburger")))))
               '((4 "Shish Taouk"))))
    (is (equalp (query (:select '*
                                :from 'receipe-tags-array
                                :where (:= "chicken" (:any* 'tags ))))
                '((4
                   #("chicken" "lemon juice" "garlic" "paprika" "yogurt" "tomato paste"
                     "pita bread")))))

;;; checking use of the or operator :&& (two different ways of passing the tested items in an array
    (is (equalp (query (:order-by (:select '*
                                           :from 'receipe-tags-array
                                           :where (:&& 'tags (:array[] "parsley" "cheese")))
                                  'receipe-id))
                '((6
                   #("meat" "cheese" "zaatar" "kishik" "tomatoes" "cucumbers" "mint leaves"
                     "olives"))
                  (8 #("bulgur" "tomatoes" "onions" "parsley"))
                  (9 #("minced meat" "parsley" "spices" "onions"))
                  (10 #("cheese" "sugar syrup" "pistachios")))))
    (is (equalp (let ((tst-arry #("parsley" "cheese")))
                  (query (:order-by (:select '*
                                             :from 'receipe-tags-array
                                             :where (:&& 'tags tst-arry))
                                    'receipe-id)))
                '((6
                   #("meat" "cheese" "zaatar" "kishik" "tomatoes" "cucumbers" "mint leaves"
                     "olives"))
                  (8 #("bulgur" "tomatoes" "onions" "parsley"))
                  (9 #("minced meat" "parsley" "spices" "onions"))
                  (10 #("cheese" "sugar syrup" "pistachios")))))

;;; checking contains :@> and contained by :<@ by operators
    (is (equalp (query (:order-by (:select '* :from 'receipe-tags-array
                                           :where (:<@ (:array[] "tomatoes" "cheese")
                                                       'tags))
                                  'receipe-id))
                '((6
                   #("meat" "cheese" "zaatar" "kishik" "tomatoes" "cucumbers" "mint leaves"
                     "olives")))))

    (is (equalp (query (:order-by (:select '* :from 'receipe-tags-array
                                           :where (:@> 'tags
                                                       (:array[] "tomatoes" "cheese")))
                                  'receipe-id))
                '((6
                   #("meat" "cheese" "zaatar" "kishik" "tomatoes" "cucumbers" "mint leaves"
                     "olives")))))

    (is (equalp (query (:order-by (:select '* :from 'receipe-tags-array
                                           :where (:@> (:array[] "tomatoes" "cheese")
                                                       'tags))
                                  'receipe-id))
                nil))
    (is (equalp (query (:order-by (:select '* :from 'receipe-tags-array
                                           :where (:<@ 'tags
                                                       (:array[] "tomatoes" "cheese")))
                                  'receipe-id))
                nil))


;;; checking selection of an item or slice of items from an array. Note the use of the :[] sql-op.
;;; Also remember that postgrsql arrays start at 1
    (is (equal (query (:select 'receipe-id (:[] 'tags 2)
                               :from 'receipe-tags-array
                               :where (:= 'receipe-id 3)))
               '((3 "olive oil"))))
    (is (equalp (query (:select 'receipe-id (:[] 'tags 2 3)
                               :from 'receipe-tags-array
                               :where (:= 'receipe-id 3)))
               '((3 #("olive oil" "eggplant")))))

    (is (equal (sql (:select 'receipe-id (:[] 'tags 2 3)
                             :from 'receipe-tags-array
                             :where (:= 'receipe-id 3)))
               "(SELECT receipe_id, (tags)[2:3] FROM receipe_tags_array WHERE (receipe_id = 3))"))

;;; checking array-dims function
    (is (equal (query (:select 'receipe-id (:array-dims 'tags)
                               :from 'receipe-tags-array
                               :where (:= 'receipe-id 3)))
               '((3 "[1:4]"))))

;;; checking passing a lisp array as a variable
    (let ((lisp-arry #("wine" "garlic" "soy sauce")))
      (query (:update 'receipe-tags-array
                      :set 'tags '$1
                      :where (:= 'receipe-id 11))
             lisp-arry))

    (is (equalp (query (:select  '*
                                :from 'receipe-tags-array
                                :where (:= 'receipe-id 11)))
               '((11 #("wine" "garlic" "soy sauce")))))))

(test array-operators
  "testing array operators"
  ;;; checking array operators. Per postgresql documentation
;;; Array comparisons compare the array contents element-by-element,
;;; using the default B-tree comparison function for the element data type.
;;; In multidimensional arrays the elements are visited in row-major order
;;; (last subscript varies most rapidly). If the contents of two arrays
;;; are equal but the dimensionality is different, the first difference in
;;; the dimensionality information determines the sort order.


;;; checking := with arrays
  (with-test-connection
    (is (equal (query (:select (:= (:type (:array[] 1 2 3) integer[]) (:array[] 1 2 3)))
                      :single)
               t))
    (is (equal (let ((arry1 #(1 2 3)) (arry2 #(1 2 3)))
                 (query (:select (:= arry1 arry2))
                        :single))
               t))
;;; checking :<> with arrays (not equal)
    (is (equal (query (:select (:<> (:array[] 1 2 3) (:array[] 1 2 4)))
                      :single)
           t))
;;; checking :< with arrays
    (is (equal (query (:select (:< (:array[] 1 2 3) (:array[] 1 2 4)))
                      :single)
               t))
;;; checking :> with arrays
    (is (equal (query (:select (:> (:array[] 1 4 3) (:array[] 1 2 4)))
                      :single)
               t))
;;; checking greater than or equal
    (is (equal (query (:select (:>= (:array[] 1 4 3) (:array[] 1 4 3)))
                      :single)
               t))
;;; checking lesser than or equal
    (is (equal (query (:select (:<= (:array[] 1 2 3) (:array[] 1 2 3)))
                      :single)
               t))
;;; checking element to array concatentation
    (is (equalp (query (:select (:|| 3 (:array[] 4 5 6) )))
                '((#(3 4 5 6)))))
    (is (equalp (query (:select (:|| (:array[] 4 5 6) 7)))
                '((#(4 5 6 7)))))
;;; checking array-concatenation
    (is (equalp (query (:select (:|| (:array[] 1 2) (:array[] 3 4))))
               '((#(1 2 3 4)))))

    (is (equalp (query (:select (:|| 1 (:type "[0:1]={2,3}" int[])))
                       :single)
                #(1 2 3)))
;;; checking :|| with multi-dimentional arrays
    (is (equalp (query (:select (:|| (:array[] 1 2 3) (:array[] (:array[] 4 5 6) (:array[] 7 8 9)))))
                '((#2A((1 2 3) (4 5 6) (7 8 9))))))

;;; checking contains
    (is (equal (query (:select (:@> (:array[] 1 4 3)(:array[] 3 1)))
                      :single)
               t))
;;; checking is contained by
    (is (equal (query (:select (:<@ (:array[] 2 7)(:array[] 1 7 4 2 6)))
                      :single)
               t))
;;; checking overlap (has elements in common)
    (is (equal (query (:select (:&& (:array[] 1 4 3)(:array[] 2 1)))
                      :single)
               t))))

(test array-functions
  "Testing various array functions"
  (with-test-connection
;;; checking postgresql array functions

;;; checking array-prepend
    (is (equalp (query (:select (:array-prepend 1 (:array[] 2 3))))
                '((#(1 2 3)))))
;;; checking array-append
    (is (equalp (query (:select (:array-append (:array[] 4 5 6) 7)))
                '((#(4 5 6 7)))))
;;; checking array-cat
    (is (equalp (query (:select (:array-cat (:array[] 1 2) (:array[] 3 4))))
                '((#(1 2 3 4)))))
    (is (equalp (query (:select (:array-cat (:array[] (:array[] 1 2) (:array[] 3 4)) (:array[] 5 6))))
                '((#2A((1 2) (3 4) (5 6))))))
;;; checking array-ndims (returns the number of dimensions of an array)
    (is (equal (query (:select (:array-ndims (:array[] (:array[] 1 2 3) (:array[] 4 5 6))))
                      :single)
               2))
;;; checking array-dims (returns a ntex representation of an array's dimensions)
    (is (equal (query (:select (:array-dims (:array[] (:array[] 1 2 3) (:array[] 4 5 6))))
                      :single)
               "[1:2][1:3]"))
;;; checking array-fill (returns an array initialized with supplied value and dimensions
    (is (equalp (query (:select (:array-fill 7 (:array[] 3) (:array[] 2))))
                '((#(7 7 7)))))
;;; checking array-length
    (is (equal (query (:select (:array-length (:array[] 1 2 3) 1 ))
                      :single)
               3))
;;; checking array-lower (returns lower bound of the requested array dimension
    (is (equal (query (:select (:array-lower (:type "[0:2]={1,2,3}" integer[]) 1))
                      :single)
               0))
;;; checking array-position (returns the subscript of the first occurrence of the
;;; second argument in the array, starting at the element indicated by the third
;;; argument or at the first element (array must be one-dimensional))
    (is (equal (query (:select (:array-position (:array[] "sun" "mon" "tue" "wed" "thu" "fri" "sat") "mon"))
                      :single)
               2))
;;; checking array-positions (returns an array of subscripts of all occurrences
;;; of the second argument in the array given as first argument (array must be one-dimensional))
    (is (equalp (query (:select (:array-positions (:array[] "A" "A" "B" "A") "A"))
                       :single)
                #(1 2 4) ))
;;; checking array-remove (remove all elements equal to the given value
;;;    from the array (array must be one-dimensional))
   (is (equalp (query (:select (:array-remove (:array[] "A" "A" "B" "A") "B")) :single)
               #("A" "A" "A")))
;;; checking array-replace (replace each array element equal to the given value with a new value)
    (is (equalp (query (:select (:array-replace (:array[] 1 2 5 4) 5 3)) :single)
                #(1 2 3 4)))
;;; checking array-to-string (concatenates array elements using supplied delimiter and optional null string)
	  (is (equal (query (:select (:array-to-string (:array[] 1 2 3 :NULL 5) "," "*")) :single)
               "1,2,3,*,5"))
;;; checking array-upper (returns upper bound of the requested array dimension)
    (is (equal (query (:select (:array-upper (:array[] 1 8 3 7) 1)) :single)
               4))
;;; checking string-to-array (splits string into array elements using
;;;    supplied delimiter and optional null string)
    (is (equalp (query (:select (:string-to-array "xx~^~yy~^~zz" "~^~" "yy")) :single)
                #("xx" :NULL "zz")))
;;; checking unnest (expand an array to a set of rows)
    (is (equalp (query (:select (:unnest (:array[] 1 2))))
                '((1) (2))))))

(test array-agg
  "Array-agg returns the result in an array (both sql and, in postmodern, a lisp array).
   Note the first example filters out null values as well as separating the y and n users."
  (with-test-connection
    (query (:drop-table :if-exists 'agg-data))
    (query (:create-table agg-data ((id :type integer))))
    (query (:insert-into 'agg-data (:select '* :from (:generate-series 1 5))))
    (is (equalp (query (:select '* :from 'agg-data))
               '((1) (2) (3) (4) (5))))

    (is (equalp (query (:select '* (:over (:array-agg 'id) (:order-by 'id))
                               :from 'agg-data))
               '((1 #(1)) (2 #(1 2)) (3 #(1 2 3)) (4 #(1 2 3 4)) (5 #(1 2 3 4 5)))))
    (is (equalp (query (:order-by (:select '*
                                           (:over (:array-agg 'id) (:order-by 'id))
                                           (:over (:array-agg 'id) (:order-by (:desc 'id)))
                                           :from 'agg-data)
                                  (:desc 'id)))
               '((5 #(1 2 3 4 5) #(5)) (4 #(1 2 3 4) #(5 4)) (3 #(1 2 3) #(5 4 3))
                 (2 #(1 2) #(5 4 3 2)) (1 #(1) #(5 4 3 2 1)))))
    (is (equalp (query (:order-by (:select '*
                                          (:over (:array-agg 'id) (:order-by (:desc 'id)))
                                          (:over (:array-agg 'id) (:order-by 'id))
                                          :from 'agg-data)
                                 'id))
               '((1 #(5 4 3 2 1) #(1)) (2 #(5 4 3 2) #(1 2)) (3 #(5 4 3) #(1 2 3))
                 (4 #(5 4) #(1 2 3 4)) (5 #(5) #(1 2 3 4 5)))))

    (is (equalp (sql (:select 'g.id
                             (:as (:array-agg 'g.users :filter (:= 'g.canonical "Y")) 'canonical-users)
                             (:as (:array-agg 'g.users :filter (:= 'g.canonical "N")) 'non-canonical-users)
                             :from (:as 'groups 'g)
                             :group-by 'g.id))
               "(SELECT g.id, ARRAY_AGG(g.users) FILTER (WHERE (g.canonical = E'Y')) AS canonical_users, ARRAY_AGG(g.users) FILTER (WHERE (g.canonical = E'N')) AS non_canonical_users FROM groups AS g GROUP BY g.id)"))
    (is (equalp (query (:select (:array-agg 'name) :from 'receipes) :single)
               #("Fattoush" "Shawarma" "Baba Ghanoush" "Shish Taouk" "Kibbe nayeh" "Manakeesh"
                 "Fakafek" "Tabbouleh" "Kofta" "Kunafeh" "Baklava")))
    (is (equal (query (:select (:array-to-string (:array-agg 'tags) ", ")
                               :from 'receipe-tags-array
                               :left-join 'receipes
                               :on (:= 'receipes.receipe-id 'receipe-tags-array.receipe-id)
                               :where (:= 'receipes.name "Shawarma"))
                      :single)
               "meat, tahini sauce, pita bread"))))
