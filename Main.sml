datatype 'a inflist = NIL
                    | CONS of 'a * (unit -> 'a inflist);

exception Empty;
exception Subscript;

fun HD (CONS(a,b)) = a
  | HD NIL = raise Empty;

fun TL (CONS(a,b)) = b()
  | TL NIL = raise Empty;

fun NUL NIL = true
  | NUL _ = false;

fun NTH 0 L = HD L
  | NTH n L = NTH (n-1) (TL L);

fun TAKE (xs, 0) = []
  | TAKE (NIL, n) = raise Subscript
  | TAKE (CONS(x, xf), n) = x::TAKE(xf(), n-1);

fun FROMN n = CONS(n, fn () => FROMN (n+1));

fun FIB n m = CONS(n, fn () => FIB m (n+m));

fun STUB _ = CONS(0, fn () => STUB 0);

fun FILTER f l =
  if NUL l
  then NIL
  else if f (HD l)
       then CONS(HD l, fn() => (FILTER f (TL l)))
       else FILTER f (TL l);

fun SIFT NIL = NIL
  | SIFT l =
     let val a = HD l
     in CONS(a, fn () => SIFT(FILTER (fn x => x mod a <> 0) (TL l)))
     end;


(**********************
 *
 * FUNCTION AND INFLIST STUBS -- YOU MUST IMPLEMENT THESE
 *
 * printList and printPairList must write to the file named by f.
 * Anything printed to the terminal will not be graded.
 *
 **********************)
fun even (x : int) : bool = if (x mod 2=0) then true else false;
fun odd  (x : int) : bool = if even x then false else true;

val even1 = FILTER even;
val odd1 = FILTER odd;
val fibs     = FIB 0 1;
val evenFibs = even1 fibs;
val oddFibs  = odd1 fibs;

fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end
fun combineString(xs: int list) : string = if (null xs) then "" else (Int.toString (hd xs)) ^ " " ^combineString(tl xs)
fun writeString (f: string,l: int list) : unit = if (null l) then () else writeFile(f)(Int.toString (hd l))
fun combineMoreStrings(xs: (int*int) list) : string = if (null xs) then "" else "(" ^ (Int.toString (#1 (hd xs))) ^ "," ^ " " ^ (Int.toString (#2 (hd xs))) ^ ")" ^ " " ^combineMoreStrings(tl xs)

fun printGenList (f : ('a -> 'b)) (l : ('a list)) : unit = if (null l) then () else printGenList(f) (tl l)
fun printList (f : string, l : int list) : unit = if (null l) then () else writeFile (f)(combineString(l))
fun printPairList (f : string, l : (int * int) list) : unit = if (null l) then () else writeFile (f)(combineMoreStrings(l))
fun ZIP (infL1 : 'a inflist, infL2 : 'b inflist) : ('a * 'b) inflist = if (NUL infL1) then  NIL else CONS((HD infL1,HD infL2), fn() => ZIP(TL infL1,TL infL2))
