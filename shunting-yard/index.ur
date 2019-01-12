structure T = Parser.Token
structure MT = Parser.MakeShuntingYard(struct
                                       structure T = T
                                       structure O = Parser.MakeOperator(T)
                                       val ops =
                                           { Symbol = T.pow, Precedence = 4, Assoc = O.Right } ::
                                           { Symbol = T.mul, Precedence = 3, Assoc = O.Left } ::
                                           { Symbol = T.div, Precedence = 3, Assoc = O.Left } ::
                                           { Symbol = T.add, Precedence = 2, Assoc = O.Left } ::
                                           { Symbol = T.sub, Precedence = 2, Assoc = O.Left } :: []

                                       type tree = float
                                       fun leaf x = x
                                       fun node o l r =
                                           if o = T.add then l + r
                                           else if o = T.sub then l - r
                                           else if o = T.pow then pow l r
                                           else if o = T.mul then l * r
                                           else if o = T.div then l / r
                                           else error <xml>Unhandled operator</xml>
                                       end)

fun eval t =
    return (MT.parse t)

fun main () =
txt <- source "";
res <- source "";
return <xml>
  <body>
    <h1>Shunting yard-based calculator example</h1>

    <p>Please enter some simple arithmetic expressions
      involving floating-point numbers and operators <strong>+</strong>, <strong>-</strong>, <strong>*</strong>, <strong>/</strong>,
      <strong>^</strong> such as the ones below and hit "Evaluate"</p>
    <p>Evaluation can also be carried out on the server.</p>

    <ul>
      <li>2 + 2 * 2</li>
      <li>3^4</li>
      <li>(2 + 2) * 4</li>
    </ul>

    <ctextbox source={txt}/>
    <button onclick={fn _ => t <- get txt; e <- return (MT.parse t); set res (show e)}>Evaluate</button>
    <button onclick={fn _ => t <- get txt; e <- rpc (eval t); set res (show e)}>Evaluate on server</button>
    <dyn signal={r <- signal res; return <xml>{[r]}</xml>}/>

  </body>
</xml>
