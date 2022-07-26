(* text-output.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Output extracted code as text.  This mode is to support splitting up source files to be
 * included in ASCIIDOC documentation.
 *)

structure TextDevice : OUTPUT_DEVICE =
  struct

    structure R = OutputReps
    structure T = Tokens

    val suffix = "txt"

    fun revmap _ ([], l) = l
      | revmap f (x::r, l) = revmap f (r, (f x)::l)

  (* this adjusts a block of text to the left by the minimum indent amount.
   * This also reverses the list of lines.
   * NOTE: this ignores RAW text (which is just used for TeX comments).
   *)
    fun outdent lns = let
	  fun extent (R.LN[], minI) = minI
	    | extent (R.LN l, minI) = let
		val {indent, ...} = T.extent l
		in
		  Int.min(minI, indent)
		end
	    | extent (txt, minI) = minI
	  val minIndent = List.foldl extent 10000 lns
	  fun adjust (R.LN({space, kind, text}::r)) =
		R.LN({space=space-minIndent, kind=kind, text=text}::r)
	    | adjust txt = txt
	  in
	    revmap adjust (lns, [])
	  end

    fun output {outStrm, codeEnv, text} = let
	  fun pr s = TextIO.StreamIO.output(outStrm, s)
	  fun pr1 c = TextIO.StreamIO.output1(outStrm, c)
	  fun prSpace 0 = ()
	    | prSpace i = (pr1 #" "; prSpace(i-1))
	  fun prText (_, text) = pr text
	  fun putToken {space, kind, text} = (prSpace space; prText (kind, text))
	  fun putHdr (R.COMMENT s :: r) = putHdr r (* do not include tool-generated comments *)
	    | putHdr l = List.app putText l
	  and putText elem = (case elem
		 of (R.RAW s) => pr s
		  | (R.COMMENT s) => ()  (* do not include tool-generated comments *)
		  | (R.LN[]) => pr "\n"
		  | (R.LN toks) => (
			List.app putToken toks; pr "\n")
		  | R.HLIGHT_ON => ()
		  | R.HLIGHT_OFF => ()
		(* end case *))
	  in
	    putHdr (outdent text)
	  end

  end

structure TextOutput = OutputFn (TextDevice)
