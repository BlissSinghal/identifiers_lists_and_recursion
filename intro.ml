(* Homework Assignment 1: OCaml Finger Exercises! *)

(* See the web pages for instructions on how to get started using Codio. *)

(* The following command tells OCaml to use the "Assert" library, which
   defines the run_test command used below. *)
;; open Assert

(* The Assert library by default will run _all_ of the test cases
   associated with this program.  While debugging, you may prefer to
   have testing stop and report the first failure that is encountered;
   the command below requests this behavior.  Remove or comment out
   the line if you want to see all of the test errors at once. *)
;; stop_on_failure ()


(* NOTE: you should _not_ use any of the functions that are built into
   OCaml, especially the ones in the List module, except where they
   are explicitly allowed in the comments.  The purpose of this
   assignment is to get famil with the basics of OCaml programming, so
   we want you to explicitly write out solutions to each of these
   problems, even though there is often a built-in function that would
   achieve the same result. You will not receive credit for solutions
   that are contrary to the spirit of the assignment. *)

(*************************************************************************)
(* Problem 1 (counting coins) *)

(* Your job in this problem is to calculate the smallest number of
   pennies, nickels, and dimes that can be used to add up to a given
   total monetary value. For example, to make 7 cents, a total of 3
   coins are needed (two pennies and a nickel). To make 99 cents, 14
   coins are needed (9 dimes, 1 nickel, and 4 pennies).
  
   To begin, have a look at our tests (just below the definition of the
   coins function), make sure you understand them, and fill in TWO
   MORE tests of your own.
  
   Then come back and fill in the body of the function `coins` below
   so that it returns the right answers on all the tests. Start by
   deleting the line beginning `failwith`. *)
(*NEED TO FIGURE OUT HOW TO KEEP TRACK OF NUMBER OF TIMES IT RECURSED*)
let rec coins (amount) : int =
  (*check to see if the amount is negative or zero*) 
  if amount < 0 then -1 else if amount = 0 then 0 else 
  (* use division and modulus*)
  (*need to divide as much as we can by each and then do it*) 
  let dimes = amount / 10 in
  let amount_after_dimes = amount mod 10 in
  let nickels = amount_after_dimes / 5 in
  let pennies = amount mod 5 in
  dimes + nickels + pennies
(* Here are two test cases for this problem. *)

let test () : bool =
  (coins (7)) = 3
;; run_test "coins nickels and pennies" test

let test () : bool =
  (coins (99)) = 14
;; run_test "coins dimes, nickels, and pennies" test

(* Here are two more test case stubs. Please edit them to produce real
   tests for the coins function.

   Note: For each of the problems in the assignment, we've provided
   some test cases like the ones above.  However, just because your
   code passes our tests does not mean that you will get full
   credit. When you submit your assignment, we will test it using
   DIFFERENT tests. To make sure that your solution is robust enough
   to pass our tests, you should think about what tests you can add to
   make sure that your program is correct.
  
   STARTING FROM HW 02, WE WILL GRADE YOU ON THE QUALITY AND ROBUSTNESS
   OF YOUR TEST CASES AS PART OF YOUR "STYLE GRADE."
  
   Please refer to the FAQ page for an explanation about test cases. *)

let test () : bool =
 (coins(-20)) = -1
;; run_test "coins -20 cents" test

let test () : bool =
 (coins(0)) = 0
;; run_test "coins 0 cents" test

(*************************************************************************)
(* Example (printing) *)

(* Printing is a useful tool, letting you see the output of your code
   on the console. In this part, we will show you how to print in
   OCaml. *)

(* Recall that OCaml files are composed of top-level definitions,
   which begin with the `let` keyword, and commands, which begin with
   two semicolons. One useful command instructs OCaml to print
   text. *)

(* The `print_endline` function causes its string argument to appear in the
   output window (much like `System.out.println` in Java). *)

;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~"
;; print_endline "Start of printing example"

(* Adding commands to print things can be very useful for debugging
   your assignment. For example, consider the following buggy
   function: *)

let day_after (day: string) : string =
  begin match day with
  | "Monday"    -> "Tuesday"
  | "Tuesday"   -> "Wednesday"
  | "Wednesday" -> "Thursday"
  | "Thursday"  -> "Friday"
  | "Friday"    -> "Saturday"
  | "Saturday"  -> "Sunday"
  | "Sunday"    -> "Monday"
  | _           -> failwith "not a valid day"
  end

(* The following test case for this definition fails, telling us that
   this definition definitely has a bug (since the test case matches
   our understanding of what the function is supposed to do). But
   running the program just tells us that the answer is wrong, without
   showing the actual answer. *)

let test () : bool =
  (day_after "Tuesday") = "Wednesday"

;; print_endline ("The day after Tuesday is " ^ (day_after "Tuesday") ^ ".") 

;; run_test "day_after Tuesday" test

(* Adding a print command will let us see what the erroneous result
   actually is.

   Try moving the `print_endline` command from the line below to
   before the failing test case (so that its output is displayed
   before the test fails); then run the code again. *)



(* (After running this example, go ahead and fix the bug in the
   `day_after` function so that the test passes). *)

(* Note: If the result that you want to print is not a string, you
   need to convert it to a string. OCaml includes two library
   functions for this conversion: `string_of_int` and `string_of_bool`.
  
   After you finish problem 1 above, uncomment the next command
   to demonstrate printing integer values. *)

(*
;; print_endline ("Coins to make 99 cents is "
                  ^ (string_of_int (coins 99)))
*)

(* Feel free to add whatever printing commands you like to this
   file. The testing and grading infrastructure will ignore any output
   that your code produces. *)

;; print_endline "End of printing example"
;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~"


(*************************************************************************)
(* Problem 2 (geometry) *)

(* Street magicians often use crates as tables in their acts.  Given
   the dimensions of a crate, your job in this part is to write a
   function to find the largest surface area it can provide when used
   as a table -- i.e., the area of its largest face.
  
   Hint: OCaml provides built-in `max` and `min` functions that take
   two arguments and behave exactly as you might expect: `max 5 2`
   returns 5, for example.  This problem can be solved using just `max`,
   `min`, and simple arithmetic.
  
   Your function's behavior when one or more of the input side lengths
   is zero or negative is not important. Your `maximum_table_area`
   function may return any value in such cases; we will not test them.
  
   Once again, you should look at our test cases first, then add your
   own test cases, and THEN come back and implement `maximum_table_area`. *)

let rec maximum_table_area (side1: int) (side2: int) (side3: int) : int =
  if side1 <= side2 && side1 <= side3 then side2 * side3
  else if side2 <= side1 && side2 <= side3 then side1 * side3
  else side1 * side2

let test () : bool =
  (maximum_table_area 1 2 3) = 6
;; run_test "maximum_table_area three different side lengths" test

let test () : bool =
  (maximum_table_area 4 3 3) = 12
;; run_test "maximum_table_area two sides the same length" test

let test () : bool =
  (maximum_table_area 10 10 10) = 100
;; run_test "maximum_table_area three same side lengths" test

let test () : bool =
  (maximum_table_area 0 10 20) = 200
;; run_test "maximum_table_area one side length is 0" test


(*************************************************************************)
(* Problem 3 (simulating robot movement) *)

(* Help a robot move along a (linear) track, with spaces numbered 0
   through 99, by calculating its new position when given `dir`, a
   string that will be either "forward" or "backward", and `num_moves`
   indicating a (non-negative) number of spaces.  Keep in mind that
   the robot can't move past the 0 or 99 spot, so when it reaches
   either end it stays there. *)

let rec move_robot (pos: int) (dir: string) (num_moves: int) : int =
  (*checking if positoin is beyond endpoint*)
  if pos < 0 || pos > 99 then -1 

  else 
  let new_forward_pos = pos + num_moves in 
  let new_backward_pos = pos - num_moves in
  (*checking if robot is moving forward*)
  if dir = "forward" then
    (*checking if the new pos is after endpoint*)
    if new_forward_pos >= 99 then 99
    else new_forward_pos
  (*robot is moving backward*)
  else 
    (*checking if the new pos is after endpoint*)
    if new_backward_pos <= 0 then 0
    else new_backward_pos

   

let test () : bool =
  (move_robot 10 "forward" 5 ) = 15
;; run_test "move_robot forward in bounds" test

let test () : bool =
  (move_robot 1 "backward" 2) = 0
;; run_test "move_robot backward out of bounds" test

let test () : bool =
 (move_robot 200 "backward" 150) = -1
;; run_test "move_robot when its already out of bounds" test

let test () : bool =
 (move_robot 0 "forward" 99) = 99
;; run_test "move_robot from one endpoint to the other" test


(*************************************************************************)
(* Problem 4 (Philadelphia geography) *)

(* Philadelphia has a fairly logical layout: the numbered streets
   are typically one-way, and their direction is determined by their
   number and where you are in the city.
  
   Even streets go one way and odd streets the other way:
  
     East of Broad (< 14th): even go south, odd go north
     West of Broad (> 14th): even go north, odd go south
     West Philly  (>= 32nd): even go south, odd go north
     West Philly  (>= 46th): two-way
  
   There are, however, a few exceptions.
     - Broad (14th), 25th, 38th, 41st, and 42nd are all two-way.
     - 24th and 59th go south.
     - 58th goes north.
     - 1st and 14th do not actually exist as street names -- they're
       called Front and Broad. We'll ignore this and pretend they do.
  
   Write a program that returns one of four string values for each street
   number:
     - "N/A" when the street doesn't exist. We only consider 1st
       through 69th Streets.
     - "N" when the street goes north.
     - "S" when the street goes south.
     - "NS" when the street is two-way.
  
   Hints:
     - You may find the infix `mod` (modulo) function useful: for example,
       `x mod 2` evaluates to 0 if x is even and 1 otherwise.
     - Sometimes there is no simple/clever way of writing down a
       complex case analysis: you just have to write out all the cases.
  
   Welcome to Philadelphia! *)

let rec street_direction (st: int) : string =
  (*checking for invalid streets (n<= 0 or > 69)*)
  if st <= 0 || st > 69 then "N/A"
  (*doing the exceptions*)
  (*two way exceptions:  14th, 25th, 38th, 41st, and 42nd*)
  else if st = 14 || st = 25 || st = 38 || st = 41 || st = 42 then "NS"
  (*south exceptions: 24th and 59th go south.*)
  else if st = 24 || st = 59 then "S"
  (*north exceptions: 58*)
  else if st = 58 then "N"
  (*East of Broad (< 14th): even go south, odd go north*)
  else if st < 14 then
    if st mod 2 = 0 then "S"
    else "N"
  (*West of Broad (> 14th): even go north, odd go south up to 32nd*)
  else if st < 32 then 
    if st mod 2 = 0 then "N"
    else "S"
  (*West Philly  (>= 32nd): even go south, odd go north up to 46th*)
  else if st < 46 then 
    if st mod 2 = 0 then "S"
    else "N"
  (*any street beyond 46th is two way*)
  else "NS"
 
  

let test () : bool =
  (street_direction 14) = "NS"
;; run_test "street_direction Broad is two-way" test

let test () : bool =
  (street_direction 9) = "N"
;; run_test "street_direction 9th goes north" test

let test () : bool =
  (street_direction 18) = "N"
;; run_test "street_direction 18th goes north" test

let test () : bool =
  (street_direction 58) = "N"
;; run_test "street_direction 58th goes north bc its an exception" test

let test () : bool =
 (street_direction 0) = "N/A"
;; run_test "street_direction 0th is invalid" test

let test (): bool = 
 (street_direction 70) = "N/A"
 ;; run_test "street_direction 70th is invalid"


(*************************************************************************)

(* NOTE: The remaining exercises are for practice with lists and recursion.
   It is best to wait until after these topics are covered in lecture before
   tackling these problems. *)

(*************************************************************************)
(* Problem 5 (exists) *)

(* Write a function that determines whether at least one boolean value
   in the input list is true. *)

let rec exists (bools: bool list) : bool =
  (*need to iterate through the boolean list*)
  (*need to use recursion without the first element*)
  begin match bools with
  | [] -> false
  | head::tail -> head || exists(tail)
  end


(* (The `not` function below takes in a boolean value and returns its
   complement.) *)

let test () : bool =
  not (exists [false; false])
;; run_test "exists all false" test

let test () : bool =
  (exists [true; false; true])
;; run_test "exists multiple true" test

let test () : bool =
 not (exists [])
;; run_test "exists empty list" test

let test () : bool =
 (exists [false; false; false; false; true])
;; run_test "exists true at end" test


(*************************************************************************)
(* Problem 6 (join) *)

(* Write a function that takes a list of strings and "flattens" it
   into a single string. Your function should also take an additional
   argument, a separator string, which is placed between adjacent
   strings in the list.
  
   Hint: the ^ operator concatenates two strings together. For example,
   `"a" ^ "bc"` evaluates to "abc". *)
let rec join (separator: string) (l: string list) : string =
  begin match l with
  | [] -> ""
  (*if this is the last element, not adding a seperator to the end*)
  | head::tail -> if tail <> [] then head ^ separator ^ (join separator tail)
    else head
  end
 
let test () : bool =
  (join "," ["a"; "b"; "c"]) = "a,b,c"
;; run_test "test_join nonempty separator" test

let test () : bool =
  (join "" ["a"; "b"; "d"]) =  "abd"
;; run_test "test_join empty separator" test

let test () : bool =
 (join ", " []) = ""
;; run_test "test_join empty list" test

let test () : bool =
 (join ", " ["a"]) = "a"
;; run_test "test_join list with one element" test


(*************************************************************************)
(* Example (printing lists) *)

;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~"
;; print_endline "Start of list printing example"

(* Once you have implemented the `join` function above, you can use it
   to print out lists of strings. This may be useful for debugging the
   remaining tasks in this assignment, allowing you to print the
   results of your functions to help you understand why test cases are
   failing. For example, try uncommenting the following line and
   examine the output of the assignment when you do so.  *)

(*
;; print_endline (join "," ["a"; "b"; "c"])
*)

(* If you would like to print a list of `int`s, you'll need a variant
   of the `join` function for this purpose.  Complete this function so
   that you can use it to help debug the last few problems in this
   homework. *)

let rec int_join (separator: string) (l: int list) : string =
  (*almost same exact thing, just need to keep converting from int to list*)
  begin match l with
  | [] -> ""
  (*if this is the last element not adding a seperator to the end*)
  | head::tail -> if tail <> [] 
    then string_of_int(head) ^ separator ^ (int_join separator tail)
    else string_of_int(head)
  end

(* print_endline ("[" ^ (int_join ";" [1; 2; 3]) ^ "]")*)


;; print_endline "End of list printing example"
;; print_endline "~~~~~~~~~~~~~~~~~~~~~~~~"

(*************************************************************************)
(* Problem 7 (append) *)

(* Write a function that takes lists l1 and l2 and returns a list
   containing all the items in l1 followed by all the items in l2.
  
   NOTE: OCaml actually already provides this function. In future
   homeworks you can use built in operator `@`, which appends l1 and l2,
   (as in l1 @ l2). Do *not* use the @ operator in your solution to this
   problem. *)


let rec append (l1: string list) (l2: string list) : string list =
  (*create a new list and keep adding recursively to it?*)
  (*when you reach the absolute end of first list, 
  skip bc we dont want two empty lists in list*)
  begin match l1, l2 with 
  | [], [] -> []
  | [], head2::tail2 -> head2::tail2
  | head1::tail1, [] -> head1::tail1
  | head1::tail1, head2::tail2 -> head1::(append tail1 l2) 
  end



let test () : bool =
  (append [] []) = []
;; run_test "append two empty lists" test

let test () : bool =
  (append ["1"; "2"] ["3"]) = ["1"; "2"; "3"]
;; run_test "append different lengths" test

let test () : bool =
 (append ["1"] []) = ["1"]
;; run_test "append list with one element with empty list" test

let test () : bool =
 (append ("hi my name"::[]) ("is"::[])) = "hi my name"::"is"::[]
;; run_test "append two lists (written with cons) with only one element" test


(*************************************************************************)
(* Problem 8 (finding names in a list) *)

(* Write a function that checks whether a list of names contains a
   given name. *)

let rec contains_str (l: string list) (name: string) : bool =
  begin match l with
  | [] -> false
  | head::tail -> head = name || (contains_str tail name)
  end

let test () : bool =
  (contains_str ["Garnet"; "Amethyst"; "Diamond"] "Amethyst")
;; run_test "contains_str name in list once" test

let test () : bool =
  not (contains_str ["Garnet"; "Amethyst"; "Diamond"] "Steven")
;; run_test "contains_str name not in list" test

let test () : bool =
 not (contains_str [] "bliss")
;; run_test "contains empty list" test

let test () : bool =
 (contains_str ["bliss"] "bliss")
;; run_test "contains list with one element" test


(* Next, write a function that, given two lists of names, filters the
   first list so that only those that are also in the second list
   remain. That is, your function should return a list containing all
   the elements that appear in both lists, in the order that they
   appear in the first list. *)

let rec in_both (names1: string list) (names2: string list) : string list =
  (*need to traverse through all of names2 for each element of names1*) 
  begin match names1, names2 with
  | head1::tail1, head2::tail2 -> if head1 = head2 
    then head1::(in_both tail1 tail2)
  (*calling in_both to look thru the names1 head with tail of names2*)
  (*also calling in_both to look through the names1's 2nd element with all
  of names2 elements *)
    else (append (in_both (head1::[]) tail2) (in_both tail1 (head2::tail2)))
  | _, _ -> []
  end
  
let test () : bool =
  (in_both ["Garnet"; "Amethyst"; "Pearl"] ["Pearl"; "Steven"]) = ["Pearl"]
;; run_test "in_both Pearl in both lists" test

let test () : bool =
  (in_both [] ["Pearl"; "Steven"]) = []
;; run_test "in_both empty name list" test

let test () : bool =
 (in_both [] []) = []
;; run_test "in_both two empty lists" test

let test () : bool =
 (in_both ["bliss"] ["bliss"]) = ["bliss"]
;; run_test "in_both single element list with matching name" test

let test () : bool =
 (in_both [] []) = []
;; run_test "in_both two empty lists" test

let test () : bool =
 (in_both ["amy"; "jane"; "james"; "john"] ["jake"; "jane"; "mandy"; "james"])
  = ["jane";"james"]
;; run_test "in_both multiple names in both lists" test


(*************************************************************************)
(* Problem 9 (merging lists) *)

(* Write a function that merges two input lists into a single list
   containing all the elements from both input lists in alternating
   order: the first, third, etc. elements come from the first input
   list and the second, fourth, etc. elements come from the second
   input list.
  
   The lengths of the two lists need not be the same: any left-over
   elements should appear at the very end of the result. *)

let rec merge (l1: int list) (l2: int list) : int list =
  begin match l1, l2 with 
  |[], [] -> []
  |[], head2::tail2 -> head2::tail2
  |head1::tail1, [] -> head1::tail1
  |head1::tail1, head2::tail2 -> head1::head2::(merge tail1 tail2)
  end

let test () : bool =
  (merge [1; 3; 5; 7] [2; 4; 6; 8]) = [1; 2; 3; 4; 5; 6; 7; 8]
;; run_test "merge lists same size" test

let test () : bool =
  (merge [] [1; 2; 3]) = [1; 2; 3]
;; run_test "merge one empty list" test

let test () : bool =
 (merge [1; 3] [2; 4; 6; 7; 8]) = [1; 2; 3; 4; 6; 7; 8]
;; run_test "merge lists different sizes" test

let test () : bool =
 (merge [1] [1]) = [1; 1]
;; run_test "merge two lists with one element in each" test


(*************************************************************************)
(* Problem 10 (is_sorted) *)

(* Write a function that determines whether a given list of integers
   is sorted in ascending order -- that is, whether each element is less 
   than or equal to the element immediately after it in the list.

   A sorted list may have repeated elements, so long as they are next
   to each other.  Lists containing zero or one elements are
   considered sorted. *)

let rec is_sorted (l: int list) : bool =
  begin match l with 
  |first::second::rest -> first <= second && is_sorted(second::rest) 
  (* if there is 1 or 0 elements in the list, say that its sorted*)
  |_ -> true
  end

let test () : bool =
  (is_sorted [1; 2; 3])
;; run_test "is_sorted sorted list" test

let test () : bool =
  not (is_sorted [3; 2; 1])
;; run_test "is_sorted unsorted list" test

let test () : bool =
 (is_sorted [])
;; run_test "is_sorted empty list" test

let test () : bool =
 (is_sorted [2])
;; run_test "is_sorted list with only one element" test

let test () : bool =
  (is_sorted [1; 5; 9; 10])
;; run_test "is_sorted sorted list whose numbers aren't consecutive" test

let test() : bool = 
  (is_sorted [1; 2; 2; 3])
;; run_test "is_sorted repeated elements" test

let test() : bool =
  not (is_sorted [1; 2; 3; 2])
;; run_test "is_sorted repeated elements not consecutive" test

(*************************************************************************)
(* Problem 11 (merge_sorted) *)

(* Write a function that takes two sorted lists (in ascending order)
   and yields a merged list that is also sorted and contains all the
   elements from the two input lists. *)

let rec merge_sorted (l1: int list) (l2: int list) : int list =
  begin match l1, l2 with
  |[], [] -> []
  (*what to do when only one element left in either l1 or l2*)
  |[], [head2] -> head2::[]
  |[head1],[] -> head1::[]
  
  (*what to do if 2+ elements left in either l1 or l2 but other list is empty*)
  |[], first2::second2::tail2 -> first2::second2::tail2
  |first1::second1::tail1, [] -> first1::second1::tail1
  
  (*what to do if one list has only one element but other has 2+*)
  |[head1], first2::second2::tail2 -> if head1 <= first2 
    then head1::first2::second2::tail2 
    else if head1 <= second2 then first2::head1::second2::tail2 
    else first2::second2::(merge_sorted (head1::[]) tail2)
  |first1::second1::tail1, [head2] -> if head2 <= first1 
    then head2::first1::second1::tail1 
    else if head2 <= second1 then first1::head2::second1::tail1
    else first1::second1::(merge_sorted tail1 (head2::[]))
    
  (*what to do if both lists have only one element*)
  |[head1], [head2] -> if head1 <= head2 then head1::head2::[] 
    else head2::head1::[]
  
  (*sort if both lists have at least 2 elements in them*)
  |first1::second1::tail1, first2::second2::tail2 -> 
    (*this is for a case if [1, 2, 3] and [4, 5, 6]*)
    if first1 <= first2 && second1 <= first2 
      then first1::second1::(merge_sorted tail1 (first2::second2::tail2))
    (* check for a case like this: [1, 3, 5] [2, 4, 6]*)
    else if first1 <= first2 && first2 <= second1 
      then first1::first2::(merge_sorted (second1::tail1) (second2::tail2))
    (*this is for a case if [4, 5, 6] and [1, 2, 3]*)
    else if first2 <= first1 && second2 <= first1 
      then first2::second2::(merge_sorted (first1::second1::tail1) tail2)
    (*this is for a case if [2, 4, 6] [1, 3, 5]*)
    else if first2 <= first1 && first1 <= second2 
      then first2::first1::second2::(merge_sorted (second1::tail1) (tail2))
    else []
  end

let test () : bool =
  (merge_sorted [2; 7] [3; 5; 12]) = [2; 3; 5; 7; 12]
;; run_test "merge_sorted lists different size" test

let test () : bool =
  (merge_sorted [1; 2; 3] [4; 5; 6]) = [1; 2; 3; 4; 5; 6]
;; run_test "merge_sorted lists same size" test

let test () : bool =
  (merge_sorted [2; 4] [1; 2; 3; 5]) = [1; 2; 2; 3; 4; 5]
;; run_test "merge_sorted lists that have the same # in them" test

let test () : bool =
 (merge_sorted [] []) = []
;; run_test "merge_sorted empty lists" test

let test () : bool =
 (merge_sorted [2] [4]) = [2; 4]
;; run_test "merge_sorted two lists with one element in each" test

let test () : bool =
 (merge_sorted [2; 3] [1]) = [1; 2; 3]
;; run_test "merge_sorted 1 list with 2 elements other list with 1 element" test






(*************************************************************************)
(* Problem 12 (sublist) *)

(* Write a function that takes two integer lists (not necessarily
   sorted) and returns true precisely when the first list is a sublist
   of the second.
  
   The first list may appear anywhere within the second, but its elements
   must appear contiguously.
  
   HINT: You should define (and test!) a helper function that you can use
   to define sublist. *)

(*helper function that defines sublist*)
let rec def_sublist (l1: int list) (l2: int list) : bool =
  (*returns true if all elements in list 1 matches list 2*)
  begin match l1, l2 with 
  (*this should go until there is nothing left in the first list*)
  | head1::tail1, head2::tail2 -> head1 = head2 && (def_sublist tail1 tail2)
  (*if list 1 isn't empty but list 2 is, 
  there is no way that list 1 can be sublist of list 2*)
  | head1::tail1, [] -> false
  | _, _ -> true
  end 
  
let rec sublist (l1: int list) (l2: int list) : bool =
  begin match l1, l2 with
  (*keeps calling def sublist for list 1 starting at diff elements in list 2*)
  | head1::tail1, head2::tail2 -> 
    (def_sublist (head1::tail1) (head2::tail2)) || 
    (sublist (head1::tail1) (tail2))
  (* if both empty lists, still sublist*)
  |[], [] -> true
  (* if list 1 is not empty but list 2 is, 
  there is no way that list 1 could be sublist of list 2*)
  | _, [] -> false
  (*if list1 is empty or both lists empty then still sublist*)
  | _, _ -> true
  end

let test () : bool =
  (sublist [] [])
;; run_test "sublist two empty lists" test

let test () : bool =
  (sublist [] [1;2;3;4])
;; run_test "sublist list 1 is empty but list 2 isn't" test

let test () : bool =
  (sublist [2;3] [1;2;3])
;; run_test "sublist is a sublist" test

let test () : bool =
  not (sublist [2;3] [2;1;3])
;; run_test "sublist elements are not contiguous" test

(*my own test cases*)
let test () : bool =
  not (sublist [2;3] [])
;; run_test "sublist list 1 is not empty but list 2 is empty" test

let test () : bool =
  (sublist [4; 5] [2; 3; 4; 3; 4; 5])
;; run_test "sublist partial sublist before then real sublist after" test


(*************************************************************************)

(*TO_DO: FIGURE OUT THE PROB*)

(* Problem 13 (rainfall) *)

(* Design and implement a function called `rainfall` that consumes a
   list of ints representing daily rainfall readings. The list may
   also contain the number -999, indicating the end of the data of
   interest. There may also be negative numbers other than -999 in the
   list; these represent faulty readings and should be skipped.
  
   Produce the average of the non-negative values in the list up to
   and including the one just before the first -999 (if any). If you
   cannot compute an average, for whatever reason, your function
   should return -1.  *)

(*helper function that finds the length excluding elements that are negative *)
let rec find_length (l: int list): int = 
  (*need to completely stop if we encounter -999*) 
    begin match l with 
    | [] -> 0
    (*=completely stops if we encounter -999*) 
    | head::tail -> if head = -999 then 0
      (*if we encounter a good data point then adding 1 to the length*) 
      else if head >= 0 then 1 + find_length(tail) 
      (*if we encounter a invalid datapoint then not adding to the length*)
      else 0 + find_length(tail)
    end

(*this helper function finds the sum of all the valid datapoints up till -999*)
let rec find_sum (l: int list): int = 
  begin match l with
  | [] -> 0
  (*=completely stops if we encounter -999*)
  | head::tail -> if head = -999 then 0 
    (*if we encounter a good data point then adding it to the sum*)
    else if head >= 0 then head + find_sum(tail)
    (*if we encounter a invalid datapoint then not adding to the sum*)
    else 0 + find_sum(tail)
  end

let rainfall (readings: int list) : int =
  (*getting the length of valid datapoints in reading up till -999*)
  let readings_length: int = (find_length readings) in
  (*getting the sum of valid data up till -999*)
  let readings_sum: int = (find_sum readings) in
  (*computing the average*)
  (*need to make sure readings_length is not zero because can't divide by zero*)
  if readings_length > 0 then readings_sum / readings_length
  else -1

  

(* For example, if we have readings [1; 2; 3], then the average
   rainfall is (1 + 2 + 3) / 3 = 6/3 = 2. *)
let test () : bool =
  rainfall [1; 2; 3] = 2
;; run_test "example" test

(* NOTE: For simplicity, you should only use int operations in this
   problem, even though this may lead to slightly wrong answers, since
   integer division discards the fractional part of its result instead
   of rounding. *)

let test () : bool =
  rainfall [2; 2; 2; 2; 1] = 1
;; run_test "use integer division to calculate average" test

(* HINT: Before you implement anything, make sure that you add some
   more test cases. The two tests above do not cover all of the
   situations in the problem description. *)

let test () : bool =
 rainfall [-10; 1; 2; 3; -1; -999] = 2
;; run_test "rainfall includes negative terms and -999 term" test

let test () : bool =
 rainfall [-2; -4; -3] = -1
;; run_test "rainfall no positive datapoints" test

let test () : bool =
 rainfall [] = -1
;; run_test "rainfall empty list" test

let test () : bool =
 rainfall [2] = 2
;; run_test "list with one element (valid datapoint)" test

let test () : bool =
 rainfall [-999] = -1
;; run_test "rainfall list with the only element being -999" test

let test () : bool =
 rainfall [2; -999; 1] = 2
;; run_test "rainfall list with only one valid element being -999" test

let test() : bool = 
  rainfall [0; 0; 0; -999] = 0
;; run_test "rainfall list with only zeroes" test

(*************************************************************************)
(* Kudos Problem (permutations) *)

(* This is a challenge problem, worth 0 points -- "kudos only." *)

(* A PERMUTATION of a list l is a list that has the same elements as l
   but not necessarily in the same order.

   Write a function that, given a list l, calculates ALL of the
   permutations of l and returns them as a list. For example,

       permutations [1;2;3]

   might yield the list (of lists)

       [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]].

   (We say "might yield" here instead of "yields" because we haven't
   specified the order of the permutations in the list returned by
   your function.  For example, the result

       [[1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]; [1;2;3]]

   would also be correct.)

   Hint: Begin by writing several unit tests to make sure you
   understand the problem. (You may need to rewrite them if your
   answer comes out in a different order, but the exercise of writing
   them first is useful.) Also, you'll probably want to break the
   problem down into one or more sub-problems, each of which can be
   solved by recursion. *)


(* Note: Do not remove or comment out this function stub, even if you
   choose not to attempt the challenge problem. Your file will not
   compile when you upload it for grading if `permutations` is
   missing. *)

let rec permutations (l: int list) : int list list =
  failwith "permutations: unimplemented"

(* The last thing in this file is a print statement. When you see this
   line after running your code, you will know that all of the tests
   in this file have succeeded. *)
;; print_endline "intro.ml: ran to completion"
