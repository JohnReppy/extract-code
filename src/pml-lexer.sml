structure PMLLexer  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
C | F | S | C0 | C1 | INITIAL
    structure UserDeclarations = 
      struct

(* pml-lexer
 *
 * COPYRIGHT (c) 1989,1992 by AT&T Bell Laboratories
 *
 * A scanner for mapping mapping Manticore code to pretty print form.
 *
 * TODO: spaces at the beginning of multi-line comments.
 *)

structure T = Tokens
structure KW = PMLKeywords

datatype lexresult
  = EOF
  | NL
  | TOK of {space : int, kind : T.token_kind, text : string}
  | INSERT_BEGIN
  | INSERT_END
  | CMD of (string * string)
  | COM of lexresult list
  | STR of lexresult list

val comLevel = ref 0
val inInsert = ref false

val resultStk = ref ([] : lexresult list)

val charList = ref ([] : string list)
fun makeString () = (concat(rev(!charList)) before charList := [])

val cmdStr = ref ""

fun inc r = (r := !r + 1)
fun dec r = (r := !r - 1)

val col = ref 0
val space = ref 0
fun tab () = let
      val n = !col
      val skip = 8 - (n mod 8)
      in
	space := !space + skip;
	col := n + skip
      end
fun expandTab () = let
      val n = !col
      val skip = 8 - (n mod 8)
      in
	charList := (StringCvt.padLeft #" " skip "") :: (!charList);
	col := n + skip
      end
fun addString s = (charList := s :: (!charList); col := !col + size s)
fun token tok = (
      space := 0;
      col := !col + size (#text tok);
      TOK tok)
fun newline () = (space := 0; col := 0; NL)
fun pushLine kind = let
      val tok = TOK{space = !space, kind = kind, text = makeString()}
      in
	space := 0;
	newline(); resultStk := NL :: tok :: !resultStk
      end
fun dumpStk kind = let
      val tok = TOK{space = !space, kind = kind, text = makeString()}
      in
	space := 0;
	(rev (tok :: !resultStk)) before resultStk := []
      end
fun mkId s = token(KW.mkToken{space = !space, text = s})
fun mkKW s = token({space = !space, kind = T.Keyword, text = s})
fun mkSym s = token({space = !space, kind = T.Symbol, text = s})
fun mkDelim s = token({space = !space, kind = T.Delim, text = s})
fun mkTyvar s = token({space = !space, kind = T.TyVar, text = s})
fun mkCon s = token({space = !space, kind = T.Literal, text = s})

fun eof () = (
      charList := []; resultStk := [];
      space := 0; col := 0;
      comLevel := 0;
      EOF)
fun error s = raise Fail s



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[
]

    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (tab(); continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inc space; inc col; continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm; (newline()))
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction6 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction15 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction16 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkDelim yytext)
      end
fun yyAction18 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkTyvar yytext)
      end
fun yyAction19 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkId yytext)
      end
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkCon yytext)
      end
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkCon yytext)
      end
fun yyAction22 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkCon yytext)
      end
fun yyAction23 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkCon yytext)
      end
fun yyAction24 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkCon yytext)
      end
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkCon yytext)
      end
fun yyAction26 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkCon yytext)
      end
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (inInsert := true; INSERT_BEGIN))
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (mkKW(substring(yytext, 7, size yytext - 14)))
      end
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN C0; continue()))
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (YYBEGIN C1; cmdStr := yytext; continue())
      end
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL;
		    space := 0; col := 0;
		    CMD(!cmdStr, makeString())))
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString yytext; continue())
      end
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN C; addString yytext; comLevel := 1; continue())
      end
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (space := 0; col := 0;
			    if !inInsert
			      then (inInsert := false; INSERT_END)
			      else error "unmatched close comment"))
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString yytext; inc comLevel; continue())
      end
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (pushLine T.Comment; continue()))
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (addString yytext;
		    dec comLevel;
		    if (!comLevel = 0)
		      then (YYBEGIN INITIAL; COM(dumpStk T.Comment))
		      else continue())
      end
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (expandTab(); continue()))
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString yytext; continue())
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (YYBEGIN S; addString yytext; continue())
      end
fun yyAction41 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (YYBEGIN S; addString yytext; continue())
      end
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (YYBEGIN INITIAL; addString yytext; STR(dumpStk T.Literal))
      end
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (error "unexpected newline in unclosed string"))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN F; pushLine T.Literal; continue()))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (expandTab(); continue()))
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString yytext; continue())
      end
fun yyAction47 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString yytext; continue())
      end
fun yyAction48 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (addString yytext; continue())
      end
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (resultStk := (newline ()) :: !resultStk; continue()))
fun yyAction50 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (YYBEGIN S; addString yytext; continue())
      end
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (error "unclosed string"))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (error "non-Ascii character"))
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yylineno = ref(yygetlineNo(!(yystrm)))
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error(concat[
		      "illegal character \"", String.toString yytext, "\" at line ",
		      Int.toString(!yylineno)
		    ]))
      end
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ66(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"0"
              then yyAction20(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ66(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ66(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ66(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ66(strm', lastMatch)
            else if inp = #"~"
              then yyQ67(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp <= #"/"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ68(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"E"
              then yyQ65(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ68(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ68(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ63(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ64(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"E"
              then yyQ65(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"E"
              then if inp <= #"9"
                  then yyQ63(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ70(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ70(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ70(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ70(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ70(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ70(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ70(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ70(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ70(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ70(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ70(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ70(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ64(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyAction23(strm, yyNO_MATCH)
                  else yyQ63(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp = #"F"
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ65(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"x"
              then yyQ69(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #","
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #","
                  then if inp = #"#"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                          else yyAction19(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp <= #")"
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"/"
                  then if inp = #"-"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"`"
              then if inp = #"]"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"]"
                  then if inp = #"A"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"A"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp = #"\\"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"^"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #","
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #","
                  then if inp = #"#"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                          else yyAction19(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp <= #")"
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #"0"
                  then yyQ62(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"."
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #":"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyQ63(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp = #"`"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"`"
              then if inp = #"]"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"]"
                  then if inp = #"A"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"A"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp = #"\\"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"^"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = yyAction6(strm, yyNO_MATCH)
fun yyQ72 (strm, lastMatch : yymatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ71 (strm, lastMatch : yymatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #"*"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"*"
                  then if inp = #"#"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                          else yyAction19(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp = #")"
                      then yyQ71(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"."
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"."
                  then if inp = #","
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #"0"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"0"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #":"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"`"
              then if inp = #"]"
                  then yyQ72(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"]"
                  then if inp = #"A"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"A"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp = #"\\"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"^"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = yyAction3(strm, yyNO_MATCH)
fun yyQ54 (strm, lastMatch : yymatch) = yyAction8(strm, yyNO_MATCH)
fun yyQ74 (strm, lastMatch : yymatch) = yyAction9(strm, yyNO_MATCH)
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ74(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ73(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = yyAction11(strm, yyNO_MATCH)
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ75(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ64(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp = #"E"
              then yyQ65(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"E"
              then if inp <= #"9"
                  then yyQ75(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ75(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ64(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp = #"E"
              then yyQ65(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"E"
              then if inp <= #"9"
                  then yyQ75(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ78(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ78(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction24(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ78(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ78(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ78(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ78(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ78(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ78(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ78(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ78(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ78(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ78(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ81(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ81(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction25(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ81(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ81(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ81(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ81(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ81(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ81(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ81(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ81(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ81(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ81(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ79(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp < #"0"
              then yyAction22(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ79(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ79(strm', lastMatch)
            else if inp = #"x"
              then yyQ80(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ65(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"/"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ64(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                      else yyAction21(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ75(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp = #"x"
              then yyQ77(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"x"
              then if inp = #"w"
                  then yyQ76(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = yyAction17(strm, yyNO_MATCH)
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ83(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ82(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = yyAction4(strm, yyNO_MATCH)
fun yyQ85 (strm, lastMatch : yymatch) = yyAction34(strm, yyNO_MATCH)
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ84(strm', lastMatch)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ85(strm', lastMatch)
                  else yyQ84(strm', lastMatch)
            else if inp = #"\r"
              then yyQ86(strm', lastMatch)
              else yyQ84(strm', lastMatch)
      (* end case *))
and yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ84(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ85(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyQ84(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp = #"\r"
              then yyQ86(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyQ84(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #"*"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"*"
                  then if inp = #"#"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                          else yyAction19(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp = #")"
                      then yyQ84(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"."
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"."
                  then if inp = #","
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #"0"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"0"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #":"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"`"
              then if inp = #"]"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"]"
                  then if inp = #"A"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"A"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp = #"\\"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"^"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ88 (strm, lastMatch : yymatch) = yyAction14(strm, yyNO_MATCH)
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ94(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ94(strm', lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yyQ94(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"\r"
                  then yystuck(lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #"!"
              then yyQ94(strm', lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yystuck(lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #"("
              then yyQ95(strm', lastMatch)
              else yyQ94(strm', lastMatch)
      (* end case *))
and yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #" "
              then yystuck(lastMatch)
            else if inp < #" "
              then if inp = #"\v"
                  then yyQ94(strm', lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yyQ94(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"\r"
                  then yystuck(lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #")"
              then yyQ94(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ95(strm', lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #"*"
              then yyQ96(strm', lastMatch)
              else yyQ94(strm', lastMatch)
      (* end case *))
and yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #" "
              then yystuck(lastMatch)
            else if inp < #" "
              then if inp = #"\v"
                  then yyQ94(strm', lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yyQ94(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"\r"
                  then yystuck(lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #")"
              then yyQ94(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ95(strm', lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #"@"
              then yyQ97(strm', lastMatch)
              else yyQ94(strm', lastMatch)
      (* end case *))
and yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #" "
              then yystuck(lastMatch)
            else if inp < #" "
              then if inp = #"\v"
                  then yyQ94(strm', lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yyQ94(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"\r"
                  then yystuck(lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #")"
              then yyQ94(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ95(strm', lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #"W"
              then yyQ98(strm', lastMatch)
              else yyQ94(strm', lastMatch)
      (* end case *))
and yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #" "
              then yystuck(lastMatch)
            else if inp < #" "
              then if inp = #"\v"
                  then yyQ94(strm', lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yyQ94(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"\r"
                  then yystuck(lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #")"
              then yyQ94(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ95(strm', lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #"K"
              then yyQ99(strm', lastMatch)
              else yyQ94(strm', lastMatch)
      (* end case *))
and yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #" "
              then yystuck(lastMatch)
            else if inp < #" "
              then if inp = #"\v"
                  then yyQ94(strm', lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yyQ94(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"\r"
                  then yystuck(lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #")"
              then yyQ94(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ95(strm', lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #"*"
              then yyQ100(strm', lastMatch)
              else yyQ94(strm', lastMatch)
      (* end case *))
and yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #" "
              then yystuck(lastMatch)
            else if inp < #" "
              then if inp = #"\v"
                  then yyQ94(strm', lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yyQ94(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"\r"
                  then yystuck(lastMatch)
                  else yyQ94(strm', lastMatch)
            else if inp = #")"
              then yyQ101(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ95(strm', lastMatch)
                  else yyQ94(strm', lastMatch)
              else yyQ94(strm', lastMatch)
      (* end case *))
and yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ94(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ94(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yyQ94(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ94(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = #"!"
              then yyQ94(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"!"
              then if inp = #" "
                  then yyAction28(strm, yyNO_MATCH)
                  else yyQ94(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp = #"("
              then yyQ95(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyQ94(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ94(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ93(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"W"
              then yyQ92(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ106(strm', lastMatch)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ107(strm', lastMatch)
                  else yyQ106(strm', lastMatch)
            else if inp = #"\r"
              then yyQ108(strm', lastMatch)
              else yyQ106(strm', lastMatch)
      (* end case *))
and yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ106(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ107(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyQ106(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp = #"\r"
              then yyQ108(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyQ106(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"T"
              then yyQ106(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"R"
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ104(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"S"
              then yyQ103(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"N"
              then yyQ102(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"J"
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"J"
              then if inp = #"I"
                  then yyQ90(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"K"
              then yyQ91(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"@"
              then yyQ89(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"+"
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"+"
              then if inp = #"*"
                  then yyQ87(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ88(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                      else yyAction18(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                      else yyAction18(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ109(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = yyAction40(strm, yyNO_MATCH)
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #","
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #","
                  then if inp = #"#"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                        else if inp = #"\""
                          then yyQ110(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                          else yyAction19(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp <= #")"
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"/"
                  then if inp = #"-"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"`"
              then if inp = #"]"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"]"
                  then if inp = #"A"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"A"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp = #"\\"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"^"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #","
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #","
                  then if inp = #"#"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp < #"#"
                      then if inp = #"!"
                          then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                          else yyAction19(strm, yyNO_MATCH)
                    else if inp = #"'"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"'"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp <= #")"
                      then yyAction19(strm, yyNO_MATCH)
                      else yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #"/"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"/"
                  then if inp = #"-"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"`"
              then if inp = #"]"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"]"
                  then if inp = #"A"
                      then yyAction19(strm, yyNO_MATCH)
                    else if inp < #"A"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                    else if inp = #"\\"
                      then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"^"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"}"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"}"
              then if inp = #"|"
                  then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"~"
              then yyQ61(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ37 (strm, lastMatch : yymatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ37(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = yyAction0(strm, yyNO_MATCH)
fun yyQ35 (strm, lastMatch : yymatch) = yyAction53(strm, yyNO_MATCH)
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ49(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"$"
                  then yyQ40(strm', lastMatch)
                else if inp < #"$"
                  then if inp = #"\^N"
                      then yyQ35(strm', lastMatch)
                    else if inp < #"\^N"
                      then if inp = #"\n"
                          then yyQ37(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ36(strm', lastMatch)
                              else yyQ35(strm', lastMatch)
                        else if inp = #"\r"
                          then yyQ38(strm', lastMatch)
                          else yyQ35(strm', lastMatch)
                    else if inp = #"!"
                      then yyQ40(strm', lastMatch)
                    else if inp < #"!"
                      then if inp = #" "
                          then yyQ39(strm', lastMatch)
                          else yyQ35(strm', lastMatch)
                    else if inp = #"\""
                      then yyQ41(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = #"+"
                  then yyQ40(strm', lastMatch)
                else if inp < #"+"
                  then if inp = #"("
                      then yyQ44(strm', lastMatch)
                    else if inp < #"("
                      then if inp = #"'"
                          then yyQ43(strm', lastMatch)
                          else yyQ40(strm', lastMatch)
                    else if inp = #")"
                      then yyQ45(strm', lastMatch)
                      else yyQ46(strm', lastMatch)
                else if inp = #"."
                  then yyQ48(strm', lastMatch)
                else if inp < #"."
                  then if inp = #","
                      then yyQ47(strm', lastMatch)
                      else yyQ40(strm', lastMatch)
                  else yyQ40(strm', lastMatch)
            else if inp = #"_"
              then yyQ55(strm', lastMatch)
            else if inp < #"_"
              then if inp = #"A"
                  then yyQ52(strm', lastMatch)
                else if inp < #"A"
                  then if inp = #";"
                      then yyQ51(strm', lastMatch)
                    else if inp < #";"
                      then if inp = #":"
                          then yyQ40(strm', lastMatch)
                          else yyQ50(strm', lastMatch)
                      else yyQ40(strm', lastMatch)
                else if inp = #"\\"
                  then yyQ40(strm', lastMatch)
                else if inp < #"\\"
                  then if inp = #"["
                      then yyQ53(strm', lastMatch)
                      else yyQ52(strm', lastMatch)
                else if inp = #"]"
                  then yyQ54(strm', lastMatch)
                  else yyQ40(strm', lastMatch)
            else if inp = #"{"
              then yyQ57(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"h"
                  then yyQ56(strm', lastMatch)
                else if inp < #"h"
                  then if inp = #"`"
                      then yyQ40(strm', lastMatch)
                      else yyQ52(strm', lastMatch)
                  else yyQ52(strm', lastMatch)
            else if inp = #"~"
              then yyQ60(strm', lastMatch)
            else if inp < #"~"
              then if inp = #"|"
                  then yyQ58(strm', lastMatch)
                  else yyQ59(strm', lastMatch)
              else yyQ35(strm', lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = yyAction31(strm, yyNO_MATCH)
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ32(strm', lastMatch)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ33(strm', lastMatch)
                  else yyQ32(strm', lastMatch)
            else if inp = #"\r"
              then yyQ34(strm', lastMatch)
              else yyQ32(strm', lastMatch)
      (* end case *))
and yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ32(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ33(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyQ32(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp = #"\r"
              then yyQ34(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyQ32(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ32(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = yyAction0(strm, yyNO_MATCH)
fun yyQ29 (strm, lastMatch : yymatch) = yyAction32(strm, yyNO_MATCH)
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ29(strm', lastMatch)
            else if inp < #"\v"
              then if inp = #"\t"
                  then yyQ30(strm', lastMatch)
                else if inp = #"\n"
                  then if yyInput.eof(!(yystrm))
                      then UserDeclarations.eof(yyarg)
                      else yystuck(lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"*"
              then yyQ31(strm', lastMatch)
              else yyQ29(strm', lastMatch)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"."
              then if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction30(strm, yyNO_MATCH)
            else if inp < #"."
              then if inp = #"-"
                  then yyQ3(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction30(strm, yyNO_MATCH)
            else if inp = #"A"
              then yyQ3(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"A"
              then if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction30(strm, yyNO_MATCH)
            else if inp <= #"Z"
              then yyQ3(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = yyAction46(strm, yyNO_MATCH)
fun yyQ26 (strm, lastMatch : yymatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ26(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyQ27(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyQ26(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
                  else yyAction47(strm, yyNO_MATCH)
            else if inp = #"\""
              then yyQ28(strm', yyMATCH(strm, yyAction47, yyNO_MATCH))
              else yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = yyAction42(strm, yyNO_MATCH)
fun yyQ22 (strm, lastMatch : yymatch) = yyAction43(strm, yyNO_MATCH)
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ22(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = yyAction45(strm, yyNO_MATCH)
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                      else yyAction48(strm, yyNO_MATCH)
                else if inp = #"\r"
                  then yyAction48(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp = #"#"
              then yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyAction48(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp = #"\\"
              then yyAction48(strm, yyNO_MATCH)
              else yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp < #"\^N"
              then if inp = #"\n"
                  then yyQ22(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ21(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                      else yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                else if inp = #"\r"
                  then yyQ23(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                  else yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp = #"#"
              then yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ24(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                  else yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ25(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
              else yyQ20(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = yyAction50(strm, yyNO_MATCH)
fun yyQ18 (strm, lastMatch : yymatch) = yyAction1(strm, yyNO_MATCH)
fun yyQ16 (strm, lastMatch : yymatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ16(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = yyAction0(strm, yyNO_MATCH)
fun yyQ14 (strm, lastMatch : yymatch) = yyAction51(strm, yyNO_MATCH)
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ14(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\n"
                  then yyQ16(strm', lastMatch)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ15(strm', lastMatch)
                      else yyQ14(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ17(strm', lastMatch)
                  else yyQ14(strm', lastMatch)
            else if inp = #"!"
              then yyQ14(strm', lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ18(strm', lastMatch)
                  else yyQ14(strm', lastMatch)
            else if inp = #"\\"
              then yyQ19(strm', lastMatch)
              else yyQ14(strm', lastMatch)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ12(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = yyAction35(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ13(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ8(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = yyAction38(strm, yyNO_MATCH)
fun yyQ6 (strm, lastMatch : yymatch) = yyAction39(strm, yyNO_MATCH)
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ6(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\n"
                  then yyQ8(strm', lastMatch)
                else if inp < #"\n"
                  then if inp = #"\t"
                      then yyQ7(strm', lastMatch)
                      else yyQ6(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ9(strm', lastMatch)
                  else yyQ6(strm', lastMatch)
            else if inp = #")"
              then yyQ6(strm', lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ10(strm', lastMatch)
                  else yyQ6(strm', lastMatch)
            else if inp = #"*"
              then yyQ11(strm', lastMatch)
              else yyQ6(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of C => yyQ0(!(yystrm), yyNO_MATCH)
    | F => yyQ1(!(yystrm), yyNO_MATCH)
    | S => yyQ2(!(yystrm), yyNO_MATCH)
    | C0 => yyQ3(!(yystrm), yyNO_MATCH)
    | C1 => yyQ4(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ5(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
