(*read/parse file function modified fro mstack overflow- cant find the link to it*)
fun parse file =
  let
    fun next_String input = (TextIO.inputAll input) 
    val stream = TextIO.openIn file
    val a = next_String stream
    val a_list= String.explode(a)
  in
   a_list
  end
(*converted given latin frequencies list in log10*)

(*https://smlfamily.github.io/Basis/char.html*)
fun isUpper c = #"A" <= c andalso c <= #"Z" 
fun isLower c = #"a" <= c andalso c <= #"z" 
fun isAlpha c = isUpper c orelse isLower c 
fun toLower c=	if isUpper c then chr (ord c + 32) else c
fun toUpper c=	if isLower c then chr (ord c - 32) else c
fun minimum [] = raise Empty
  | minimum [x] = x
  | minimum (x::xs) = Real.min (x, minimum xs)
(*count occurences of an element in a list*)
fun index (x, []) = raise Subscript
  | index (x, y::ys) =
    if Real.toString x = Real.toString y  then 0 else 1 + index (x, ys)
fun count list e = 
    let
    fun count_help []_acc= acc
        | count_help (h::t) e acc = if toLower h = toLower e  then count_help t e (acc+1.0) else count_help t e acc
    in
    count_help list e 0.0
    end
  (*https://gist.github.com/eldesh/1867638*)
  fun let2int c = ord c - ord #"a"
  fun int2let n = chr (ord #"a" + n)
  fun Upperlet2int c = ord c - ord #"A"
  fun Upperint2let n = chr (ord #"A" + n) 
  fun shift n c = if Char.isLower c then int2let((let2int c + n) mod 26)
                  else if Char.isUpper c then Upperint2let((Upperlet2int c + n) mod 26)
                  else c

fun rotate xs n =List.drop (xs,n) @ List.take (xs,n)
fun frequency (filecharlist)= 
    let val alvavita= explode"abcdefghijklmnopqrstuvwxyz"
        val input = parse filecharlist
        val listlength= length input
        val occurences =map (count input) alvavita
        fun divlength b a = a/b 
    in map (divlength (Real.fromInt listlength) ) occurences
    end
fun innerProduct L1 L2 =
  let
    fun innerProduct_help []  [] acc =acc
    | innerProduct_help (h1::t1)  (h2::t2)  acc = innerProduct_help t1  t2 (acc + h1 *h2)
  in
    innerProduct_help L1 L2 0.0
  end

fun decrypt(file)=
 let   
 val shiftamount= [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,16,18,19,20,21,22,23,24,25,26]
 val list2 =[2.50507, 4.20505, 3.582, 3.15755, 2.06341, 3.80407,
                                                3.90455, 2.79787, 2.66413, 6.48249, 4.86394, 3.21265,
                                                3.7272, 2.69578, 2.58933, 3.94817, 6.95905, 2.81558,
                                                2.76034, 2.40174, 3.59066, 4.62742, 3.74651, 6.50229,
                                                3.92511, 7.20886]
     fun entropynshifts file n = innerProduct (frequency file) (rotate list2 n)
     val allshiftslist = map (entropynshifts file) shiftamount
     val minEntropy=  minimum (allshiftslist)
     val deciphershift =index(minEntropy,allshiftslist)+1 
     val output=implode(map (shift deciphershift)(parse file))
  in 
     print(output)
  end          