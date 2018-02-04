exception ParseError
exception EvalError

datatype term = I | K | S | A of term * term | M of term

datatype 'a result = NoValue
                   | NoParsed
                   | Parsed of 'a * {exp : term result,
                                     expSuffix : (term -> term) result,
                                     exp' : term result,
                                     char : char result} ref

(*パーサ*)

fun pExp ta =
    case #exp' (!ta) of
        Parsed (t, ta') =>
        (case #expSuffix (!ta') of
             Parsed (suf, ta'') => Parsed (suf t, ta'')
           | _ => NoParsed)
      | _ => NoParsed

fun pExpSuf ta =
    case #exp' (!ta) of
        Parsed (t, ta') =>
        (case #expSuffix (!ta') of
             Parsed (suf, ta'') => Parsed (fn x => suf (A (x, t)), ta'')
           | _ => Parsed (fn x => x, ta))
      | _ => Parsed (fn x => x, ta)

fun pExp' ta =
    case #char (!ta) of
        Parsed (#"I", ta') => Parsed (I, ta')
      | Parsed (#"K", ta') => Parsed (K, ta')
      | Parsed (#"S", ta') => Parsed (S, ta')
      | Parsed (#"(", ta') =>
        (case #exp (!ta') of
             Parsed (t, ta'') =>
             (case #char (!ta'') of
                  Parsed (#")" , ta''') => Parsed (t , ta''')
                | _ => NoParsed)
           | _ => NoParsed)
      | _ => NoParsed


fun init [] =
    ref {exp  = NoValue, expSuffix = NoValue,
         exp' = NoValue, char = NoValue}
  | init (h :: t) =
    if Char.isSpace h
    then init t
    else ref {exp  = NoValue, expSuffix = NoValue,
              exp' = NoValue, char = Parsed (h, (init t))}

fun parse cl =
    let
      fun isWS cl = List.all (Char.isSpace) cl
      fun Exp t = t := (!t # {exp = pExp t})
      fun ExpSuffix t = t := (!t # {expSuffix = pExpSuf t})
      fun Exp' t =  t := (!t # {exp'  = pExp' t})
      fun exec t =
          case #char (!t) of
              NoValue => ()
            | NoParsed => ()
            | Parsed (c, d) => (exec d;  Exp' d;
                                ExpSuffix d; Exp d)
      fun extResult ta =
          case #exp (!ta) of
              Parsed (t, ta') => t
            | _ => raise ParseError

      val table = if isWS cl then init [#"I"] else init cl
    in
      exec table;
      Exp' table;
      ExpSuffix table;
      Exp table;
      extResult table
    end

(*出力*)

fun toString t =
    let
      fun inTerm t =
          case t of
              I => "I"
            | K => "K"
            | S => "S"
            | A (t1, t2) => "(" ^ inTerm t1 ^
                            " " ^ inTerm t2 ^ ")"
            | M t => raise EvalError
    in
      inTerm t
    end

fun isMemo t =
          case t of
              A (t1, t2) => isMemo t1 orelse isMemo t2
            | M _ => true
            | _ => false

fun memoCheck l t =
    if List.all isMemo l then NONE else SOME t

fun writeEval1 t =
      case t of
          A (I, t) => memoCheck [t] (M t)
        | A (A (K, t1), t2) => memoCheck [t1,t2] (M t1)
        | A (A (A (S, t1), t2), t3) =>
          memoCheck [t1,t2,t3] (A (A (M t1, M t3), A (M t2, M t3)))
        | A (t1, t2) =>
          (case writeEval1 t1 of
               SOME t1' => SOME (A (t1', t2))
             | NONE =>
               case writeEval1 t2 of
                   SOME t2' => SOME (A (t1, t2'))
                 | NONE => NONE)
        | _ => NONE

fun writeEval t =
    case writeEval1 t of
        NONE => t
      | SOME t' => writeEval t'


fun eval1 t =
    case t of
        A (I, t) => memoCheck [t] t
      | A (A (K, t1), t2) => memoCheck [t1] t1
      | A (A (A (S, t1), t2), t3) =>
        memoCheck [t1,t2,t3]  (A (A (t1, t3), A (t2, t3)))
      | A (t1, t2) =>
        (case eval1 t1 of
             SOME t1' => SOME (A (t1', t2))
           | NONE =>
             case eval1 t2 of
                 SOME t2' => SOME (A (t1, t2'))
               | NONE => NONE)
      | _ => NONE

fun eval1' t =
    case eval1 t of
        NONE => t
      | SOME t' => t'

fun read1 t =
    case t of
        M t => t
      | A (t1, t2) => if isMemo t1
                      then A (read1 t1, t2)
                      else A (t1, read1 t2)
      | _ => t




fun isSame I I = true
  | isSame K K = true
  | isSame S S = true
  | isSame (M t1) (M t2) = isSame t1 t2
  | isSame (A (t1, t2)) (A (t1', t2')) = isSame t1 t1' andalso isSame t2 t2'
  | isSame _ _ = false

fun eval e =
    let
      fun aux w =
          let
            val r = read1 w
            val e = eval1' r
          in
            if isSame r e
            then if isSame (read1 e) e then e else aux e
            else aux (writeEval e)
          end
    in
      aux (writeEval e)
    end

(*トップレベル*)

fun main () =
    case (print ">> " ; TextIO.inputLine TextIO.stdIn) of
        NONE => ()
      | SOME src =>
        let
          val time1 = Time.toReal (Time.now ())
          val parseResult = parse (String.explode src)
          val time2 = Time.toReal (Time.now ())
          val evalResult = eval parseResult
          val time3 = Time.toReal (Time.now ())
        in
          print ("\n" ^ toString evalResult ^ "\n");
          print (Real.toString (time2 - time1) ^ " : parsing time\n");
          print (Real.toString (time3 - time2) ^ " : evaluation time\n");
          main ()
        end


fun errorHandle () =
    let
      val _ = main () handle ParseError => print "Error : Failed to parse\n"
                           | EvalError => print "Error : Remained Memo\n"
    in
      errorHandle ()
    end

val _ = errorHandle () : unit
