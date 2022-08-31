use peg::parser;

#[derive(Debug, Clone)]
pub enum Token {
    Function(String, Vec<String>, Vec<Token>),
    Identifier(String),

    Integer(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Assign(bool, String, Box<Token>),
    Array(Vec<Token>),

    Index(String, Vec<Token>),
    ArrayIndexAssign(String, Vec<Token>, Box<Token>),

    Eq(Box<Token>, Box<Token>),
    Ne(Box<Token>, Box<Token>),
    Lt(Box<Token>, Box<Token>),
    Le(Box<Token>, Box<Token>),
    Gt(Box<Token>, Box<Token>),
    Ge(Box<Token>, Box<Token>),
    Add(Box<Token>, Box<Token>),
    Sub(Box<Token>, Box<Token>),
    Mul(Box<Token>, Box<Token>),
    Div(Box<Token>, Box<Token>),
    IfElse(Box<Token>, Vec<Token>, Option<Vec<Token>>),
    WhileLoop(Box<Token>, Vec<Token>),
    Call(String, Vec<Token>),
    Return(Box<Token>),
    GlobalDataAddr(String)
}

parser!(pub grammar parser() for str {

    pub rule script() -> Vec<Token>
        = WHITESPACE() f:(statements()) WHITESPACE() { f }

    rule statements() -> Vec<Token>
        = s:(statement()*) { s }

    rule statement() -> Token
        = WHITESPACE() e:(
            var() /
            assignment() / array_assigment() /
            call() /
            function() /
            if_else() /
            while_loop() /
            rtn()
        ) WHITESPACE() { e }

    rule function() -> Token
        = "function" _ name:identifier() _ "(" params:((_ i:identifier() _ {i}) ** ",") ")" _
        "{" _ stmts:statements() _ "}" WHITESPACE()
        { Token::Function(name, params, stmts) }

    rule call() -> Token
        = i:identifier() "(" args:((_ e:expression() _ {e}) ** ",") ")" { Token::Call(i, args) }

    rule var() -> Token
        = "var" _ i:identifier() _ "=" _ e:expression() {  Token::Assign(true, i, Box::new(e)) }

    rule assignment() -> Token
        = i:identifier() _ "=" _ e:expression() {  Token::Assign(false, i, Box::new(e)) }

    rule array_assigment() -> Token
        = i:identifier() indexes:("[" e:expression() "]" { e })+  _ "=" _ e:expression()  { Token::ArrayIndexAssign(i, indexes, Box::new(e)) }

    rule if_else() -> Token
        = "if" _ e:expression() WHITESPACE() "{" WHITESPACE() then_body:statements() WHITESPACE() "}" WHITESPACE()
        else_body:("else" _ "{" WHITESPACE() s:statements() WHITESPACE() "}" { s })?
        { Token::IfElse(Box::new(e), then_body, else_body) }

    rule while_loop() -> Token
        = "while" _ e:expression() WHITESPACE() "{" WHITESPACE()
            s:statements() WHITESPACE() "}" { Token::WhileLoop(Box::new(e), s) }

    rule rtn() -> Token
        = "return" _ e:expression() { Token::Return(Box::new(e)) }

    rule expression() -> Token = precedence!{
        a:@ _ "==" _ b:(@) { Token::Eq(Box::new(a), Box::new(b)) }
        a:@ _ "!=" _ b:(@) { Token::Ne(Box::new(a), Box::new(b)) }
        a:@ _ "<"  _ b:(@) { Token::Lt(Box::new(a), Box::new(b)) }
        a:@ _ "<=" _ b:(@) { Token::Le(Box::new(a), Box::new(b)) }
        a:@ _ ">"  _ b:(@) { Token::Gt(Box::new(a), Box::new(b)) }
        a:@ _ ">=" _ b:(@) { Token::Ge(Box::new(a), Box::new(b)) }
        --
        a:@ _ "+" _ b:(@) { Token::Add(Box::new(a), Box::new(b)) }
        a:@ _ "-" _ b:(@) { Token::Sub(Box::new(a), Box::new(b)) }
        --
        a:@ _ "*" _ b:(@) { Token::Mul(Box::new(a), Box::new(b)) }
        a:@ _ "/" _ b:(@) { Token::Div(Box::new(a), Box::new(b)) }
        --
        l:literal() { l }
        i:identifier() "(" args:((_ e:expression() _ {e}) ** ",") ")" { Token::Call(i, args) }
        a:array_index() { a }
        i:identifier() { Token::Identifier(i) }
    }

    rule func_call() -> Token
        = quiet!{i:identifier() "(" args:((_ e:expression() _ {e}) ** ",") ")" { Token::Call(i, args) } }

    rule identifier() -> String
        = quiet!{ n:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { n.to_owned() } }
        / expected!("identifier")

    rule array_index() -> Token
        = i:identifier() indexes:("[" e:expression() "]" { e })+ { Token::Index(i, indexes) }

    rule string() -> String
        = quiet!{ n:$([^'"']*) { n.to_owned() } }
        / expected!("string")

    rule literal() -> Token
        = n:$(['0'..='9']+ "." ['0'..='9']+) { Token::Float(n.parse().unwrap()) }
        / n:$(['0'..='9']+) { Token::Integer(n.parse().unwrap()) }
        / "true" { Token::Bool(true) }
        / "false" { Token::Bool(false) }
        / "&" i:identifier() { Token::GlobalDataAddr(i) }
        / "\"" s:string() "\"" { Token::String(s) }
        / "[" WHITESPACE() elements:((_ e:expression() _ {e}) ** ",") WHITESPACE() "]" { Token::Array(elements) }

    rule _() =  quiet!{[' ' | '\t']*}
    rule NEWLINE() = quiet!{ ['\n'|'\r'] }
    rule NEWLINES() = quiet!{ ['\n'|'\r']* }
    rule WHITESPACE() = quiet!{ [' '|'\t'|'\n'|'\r']* }

});