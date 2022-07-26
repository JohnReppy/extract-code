(* net-message.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This implements the conversion between wire and ML representations
 * of network messages.  There are six different types of messages,
 * divided into two classes:
 *
 *   1) Messages from a tuple-server proxy to a remote server:
 *	- InReq
 *	- RdReq
 *	- Accept
 *	- Cancel
 *	- OutTuple
 *
 *   2) Messages from a remote server to a tuple-server proxy:
 *	- InReply
 *)

(*@FILE net-message-sig.tex recv-message.tex *)
structure NetMessage : sig

(*@BEGIN net-message-sig.tex *)
    datatype message
      = OutTuple of Tuple.tuple
      | InReq of {
	  transId : int,
	  pat : Tuple.template
	}
      | RdReq of {
	  transId : int,
	  pat : Tuple.template
	}
      | Accept of { transId : int }
      | Cancel of { transId : int }
      | InReply of {
	  transId : int,
	  vals : Tuple.val_atom list
	}

    type 'a sock = ('a, Socket.active Socket.stream) Socket.sock

    val recvMessage : 'a sock -> message
    val sendMessage : ('a sock * message) -> unit
(*@END net-message-sig.tex *)

  end = struct

    fun error msg = raise Fail msg

    datatype message
      = OutTuple of Tuple.tuple
      | InReq of {
	  transId : int,
	  pat : Tuple.template
	}
      | RdReq of {
	  transId : int,
	  pat : Tuple.template
	}
      | Accept of { transId : int }
      | Cancel of { transId : int }
      | InReply of {
	  transId : int,
	  vals : Tuple.val_atom list
	}

    type 'a sock = ('a, Socket.active Socket.stream) Socket.sock

(*@BEGIN recv-message.tex *)
    structure DR = DataRep

    fun recvMessage sock = let
	  val hdr = SockUtil.recvVec (sock, 4)
	  val kind = Pack16Big.subVec(hdr, 0)
	  val len = LargeWord.toInt(Pack16Big.subVec(hdr, 1))
	  val data = SockUtil.recvVec (sock, len)
	  fun getId () = LargeWord.toInt(Pack32Big.subVec(data, 0))
	  fun getTuple () = #1 (DR.decodeTuple (data, 0))
	  fun getPat () = #1 (DR.decodeTemplate (data, 4))
	  fun getVals () = #1 (DR.decodeValues (data, 4))
	  in
	    case kind
	     of 0w0 => OutTuple(getTuple ())
	      | 0w1 => InReq{transId=getId(), pat=getPat()}
	      | 0w2 => RdReq{transId=getId(), pat=getPat()}
	      | 0w3 => Accept{transId=getId()}
	      | 0w4 => Cancel{transId=getId()}
	      | 0w5 => InReply{transId=getId(), vals=getVals()}
	      | _ => error "recvMessage: bogus message kind"
	    (* end case *)
	  end
(*@END recv-message.tex *)

    fun sendMessage (sock, msg) = let
	  val (kind, sz) = (case msg
		 of (OutTuple data) => (0w0, DR.tupleSz data)
		  | (InReq{pat, ...}) => (0w1, 4 + DR.templateSz pat)
		  | (RdReq{pat, ...}) => (0w2, 4 + DR.templateSz pat)
		  | (Accept _) => (0w3, 4)
		  | (Cancel _) => (0w4, 4)
		  | (InReply{vals, ...}) => (0w5, 4 + DR.valuesSz vals)
		(* end case *))
	  val msg' = Word8Array.array(4+sz, 0w0)
	  fun putId id = Pack32Big.update(msg', 1, LargeWord.fromInt id)
	  in
	    Pack16Big.update(msg', 0, kind);
	    Pack16Big.update(msg', 1, LargeWord.fromInt sz);
	    case msg
	     of (OutTuple data) =>
		  ignore (DR.encodeTuple (data, msg', 4))
	      | (InReq{transId, pat}) => (
		  putId transId;
		  ignore (DR.encodeTemplate (pat, msg', 8)))
	      | (RdReq{transId, pat}) => (
		  putId transId;
		  ignore (DR.encodeTemplate (pat, msg', 8)))
	      | (Accept{transId}) => putId transId
	      | (Cancel{transId}) => putId transId
	      | (InReply{transId, vals}) => (
		  putId transId;
		  ignore (DR.encodeValues (vals, msg', 8)))
	    (* end case *);
	    SockUtil.sendArr (sock, msg')
	  end

  end (* NetMessage *)

