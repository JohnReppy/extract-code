(* moby-keywords.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

structure MobyKeywords : sig

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
		("abstract",	T.Keyword),
		("and",		T.Keyword),
		("case",	T.Keyword),
		("class",	T.Keyword),
		("const",	T.Keyword),
		("datatype",	T.Keyword),
		("deconst",	T.Keyword),
		("do",		T.Keyword),
		("else",	T.Keyword),
		("enumtype",	T.Keyword),
		("event",	T.Keyword),
		("except",	T.Keyword),
		("extends",	T.Keyword),
(*		("False",	T.Keyword),*)
		("field",	T.Keyword),
		("final",	T.Keyword),
		("finally",	T.Keyword),
		("fn",		T.Keyword),
		("for",		T.Keyword),
		("fun",		T.Keyword),
		("if",		T.Keyword),
		("implements",	T.Keyword),
		("include",	T.Keyword),
		("inherits",	T.Keyword),
		("initially",	T.Keyword),
		("is",		T.Keyword),
		("isnot",	T.Keyword),
		("ivar",	T.Keyword),
		("let",		T.Keyword),
		("local",	T.Keyword),
		("maker",	T.Keyword),
		("method",	T.Keyword),
		("module",	T.Keyword),
		("mvar",	T.Keyword),
		("nack_event",	T.Keyword),
		("new",		T.Keyword),
		("objtype",	T.Keyword),
		("of",		T.Keyword),
		("override",	T.Keyword),
		("public",	T.Keyword),
		("raise",	T.Keyword),
		("rdy_event",	T.Keyword),
		("self",	T.Keyword),
		("signature",	T.Keyword),
		("spawn",	T.Keyword),
		("super",	T.Keyword),
		("sync",	T.Keyword),
		("tagtype",	T.Keyword),
		("then",	T.Keyword),
(*		("True",	T.Keyword),*)
		("try",		T.Keyword),
		("type",	T.Keyword),
		("typeof",	T.Keyword),
		("val",		T.Keyword),
		("var",		T.Keyword),
		("when",	T.Keyword),
		("with",	T.Keyword)
	      ]
	  end

    val peek = AtomTable.find keywords

    fun mkToken {space, text} = let
	  val name = Atom.atom text
	  val kind = (case (peek name) of (SOME k) => k | _ => T.Ident)
	  in

	    {space = space, kind = kind, text = Atom.toString name}
	  end

  end (* MobyKeywords *)
