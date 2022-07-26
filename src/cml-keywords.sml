(* cml-keywords.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This structure implements a hash table for mapping identifiers to
 * unique names (and parser tokens).
 *)

structure CMLKeywords : sig

    val mkToken : {space : int, text : string}
	  -> {space : int, kind : Tokens.token_kind, text : string}

  end = struct

    structure T = Tokens

  (* the keyword hash table *)
    exception Keyword
    val keywords : T.token_kind AtomTable.hash_table = AtomTable.mkTable(64, Keyword)

  (* insert the reserved words into the keyword hash table *)
    val _ = let
	  val insert = AtomTable.insert keywords
	  fun ins (s, item) = insert (Atom.atom s, item)
	  in
	    app ins [
	      (* SML reserved words *)
		("*", T.Symbol),
		("|", T.Symbol),
		(":", T.Symbol),
		(":>", T.Symbol),
		("=", T.Symbol),
		("#", T.Symbol),
		("->", T.Symbol),
		("=>", T.Symbol),
		("abstype", T.Keyword),
		("and", T.Keyword),
		("andalso", T.Keyword),
		("as", T.Keyword),
		("case", T.Keyword),
		("datatype", T.Keyword),
		("do", T.Keyword),
		("else", T.Keyword),
		("end", T.Keyword),
		("eqtype", T.Keyword),
		("exception", T.Keyword),
		("fn", T.Keyword),
		("fun", T.Keyword),
		("functor", T.Keyword),
		("funsig", T.Keyword),
		("handle", T.Keyword),
		("if", T.Keyword),
		("in", T.Keyword),
		("include", T.Keyword),
		("infix", T.Keyword),
		("infixr", T.Keyword),
		("let", T.Keyword),
		("local", T.Keyword),
		("nonfix", T.Keyword),
		("of", T.Keyword),
		("op", T.Keyword),
		("open", T.Keyword),
		("orelse", T.Keyword),
		("raise", T.Keyword),
		("rec", T.Keyword),
		("sharing", T.Keyword),
		("sig", T.Keyword),
		("signature", T.Keyword),
		("struct", T.Keyword),
		("structure", T.Keyword),
		("then", T.Keyword),
		("type", T.Keyword),
		("val", T.Keyword),
		("where", T.Keyword),
		("while", T.Keyword),
		("with", T.Keyword),
		("withtype", T.Keyword),
	      (* extra CML identifiers *)
		("chan", T.SpecialId),
		("channel", T.SpecialId),
		("choose", T.SpecialId),
		("CML", T.SpecialId),
		("guard", T.SpecialId),
		("recv", T.SpecialId),
		("recvEvt", T.SpecialId),
		("send", T.SpecialId),
		("sendEvt", T.SpecialId),
		("select", T.SpecialId),
		("spawn", T.SpecialId),
		("sync", T.SpecialId),
		("withNack", T.SpecialId),
		("wrap", T.SpecialId)
	      ]
	  end

    val peek = AtomTable.find keywords

    fun mkToken {space, text} = let
	  val name = Atom.atom text
	  val kind = (case (peek name) of (SOME k) => k | _ => T.Ident)
	  in
	    {space = space, kind = kind, text = Atom.toString name}
	  end

  end (* SMLKeywords *)

