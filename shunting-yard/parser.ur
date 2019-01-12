signature TOKEN = sig
    type token
    val eq_token : eq token
    val show_token : show token

    val number : float -> token
    val isNumber : token -> bool
    val getNumber : token -> float

    val leftParen : token
    val rightParen : token

    val mul : token
    val pow : token
    val div : token
    val add : token
    val sub : token

    val isOperator : token -> bool

    val lex : string -> list token
end

structure Token : TOKEN = struct
    datatype token = LeftParen | RightParen | Mul | Pow | Div | Add | Sub | Num of float

    val eq_token : eq token =
        mkEq (fn x y =>
                 case (x, y) of
                     (LeftParen, LeftParen) => True
                   | (RightParen, RightParen) => True
                   | (Mul, Mul) => True
                   | (Pow, Pow) => True
                   | (Div, Div) => True
                   | (Add, Add) => True
                   | (Sub, Sub) => True
                   | (Num x, Num y) => (x = y)
                   | (_, _) => False)
    val show_token : show token =
        mkShow (fn x =>
                   case x of
                       LeftParen => "("
                     | RightParen => ")"
                     | Mul => "*"
                     | Pow => "^"
                     | Div => "/"
                     | Add => "+"
                     | Sub => "-"
                     | Num x => show x)

    fun number n = Num n
    fun isNumber t = case t of Num _ => True | _ => False
    fun getNumber t =
        case t of Num x => x | _ => error <xml>tried to getNumber on non-number</xml>

    val leftParen = LeftParen
    val rightParen = RightParen

    val mul = Mul
    val pow = Pow
    val div = Div
    val add = Add
    val sub = Sub
    fun isOperator t =
        case t of Mul => True
                | Pow => True
                | Div => True
                | Add => True
                | Sub => True
                | _ => False

    fun readNumber (s : string) : option float * string =
        let
            val len = strlen s

            fun aux i =
                if i >= len then i
                else
                    let
                        val ch = strsub s i
                    in
                        if isdigit ch || ch = #"-" || ch = #"." || ch = #"E" || ch = #"e" then aux (i+1)
                        else i
                    end

            val last = aux 0
        in
            if last <= 0 then (None, s)
            else
                let
                    val n = substring s 0 last
                    val s' = substring s last (len - last)
                    val r = readError n
                in
                    (Some r, s')
                end
        end

    fun lexstep s acc =
        let
            val len = strlen s
        in
            if len <= 0 then acc
            else
                let
                    val ch = strsub s 0
                    val s1 = strsuffix s 1
                in
                    if ch = #" " then lexstep s1 acc
                    else if ch = #"(" then lexstep s1 (LeftParen :: acc)
                    else if ch = #")" then lexstep s1 (RightParen :: acc)
                    else if ch = #"*" then lexstep s1 (Mul :: acc)
                    else if ch = #"^" then lexstep s1 (Pow :: acc)
                    else if ch = #"/" then lexstep s1 (Div :: acc)
                    else if ch = #"+" then lexstep s1 (Add :: acc)
                    else if ch = #"-" then lexstep s1 (Sub :: acc)
                    else
                        (case readNumber s of
                             (Some n, s1) => lexstep s1 (Num n :: acc)
                           | (None, s1) => error <xml>bad input</xml>)

                end
        end

    fun lex s =
        List.rev (lexstep s [])
end

signature OPERATOR = sig
  type token
  datatype associativity = Left | Right
  type operator = { Symbol : token, Assoc : associativity, Precedence : int }

  val find : list operator -> token -> option operator
  val cmp : operator -> operator -> bool
end

functor MakeOperator(T : TOKEN) : OPERATOR where type token = T.token = struct
  datatype associativity = Left | Right
  type operator = { Symbol : T.token, Assoc : associativity, Precedence : int }
  type token = T.token

  fun find ops c =
      List.find (fn s => (s.Symbol = c)) ops

  fun cmp (o1 : operator) (o2 : operator) =
      let
          val p1 = o1.Precedence
          val a1 = o1.Assoc
          val p2 = o2.Precedence
      in
          case a1 of
              Left => p1 <= p2
            | Right => p1 < p2
      end
end

signature SHUNTING_YARD = sig
    type tree

    val parse : string -> tree
end

functor MakeShuntingYard(M : sig
                          structure T : TOKEN
                          structure O : OPERATOR where type token = T.token

                          val ops : list O.operator

                          type tree
                          val leaf : float -> tree
                          val node : T.token -> tree -> tree -> tree
                     end) : SHUNTING_YARD where type tree = M.tree = struct
    open M
    type operator = O.operator
    type token = T.token

    fun reduce (t : token) (xs : list tree) : list tree =
        case xs of
            b :: a :: rest => node t a b :: rest
          | _ => error <xml>not enough arguments to operator {[t]}</xml>

    fun popUntilLeftParen (os : list token) (trees : list tree)
        : (list token * list tree) =
        case (os, trees) of
            ([], _) => error <xml>unopened parenthesis</xml>
          | (opr :: os, trees) =>
            if opr = T.leftParen then (os, trees)
            else
                let
                    val trees = reduce opr trees
                in
                    popUntilLeftParen os trees
                end

    fun shift (o1 : operator) (os : list token) (trees : list tree)
        : (list token * list tree) =
        case (os, trees) of
            ([], trees) => ([], trees)
          | (o2 :: os, trees) =>
            (case O.find ops o2 of
                 Some o2 =>
                 if O.cmp o1 o2 then
                     let
                         val trees = reduce o2.Symbol trees
                     in
                         shift o1 os trees
                     end
                 else (o2.Symbol :: os, trees)
               | None => (o2 :: os, trees))

    fun aux (toks : list token) (stk : list token) (trees : list tree) : tree =
        case (toks, stk, trees) of
            ([], [], result :: []) => result
          | ([], opr :: os, trees) =>
            if opr = T.leftParen || opr = T.rightParen then error <xml>unclosed parenthesis</xml>
            else
                let
                    val trees = reduce opr trees
                in
                    aux [] os trees
                end
          | (t :: ts, os, trees) =>
            if t = T.leftParen then
                aux ts (t :: os) trees
            else if t = T.rightParen then
                let
                    val (os, trees) = popUntilLeftParen os trees
                in
                    aux ts os trees
                end
            else
                (case O.find M.ops (t : T.token) of
                     Some o1 => let
                         val (os, trees) = shift o1 os trees
                     in
                         aux ts (o1.Symbol :: os) trees
                     end
                   | None =>
                     let
                         val n = T.getNumber t
                         val trees = leaf n :: trees
                     in
                         aux ts os trees
                     end)
          | (_, _, _) => error <xml>bad input</xml>

    fun parse (s : string) : tree = let
        val tokens = T.lex s
    in
        aux tokens [] []
    end
end
