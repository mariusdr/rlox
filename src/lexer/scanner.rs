use super::cursor::Cursor;
use super::token::{Token, TokenType};

/// The scanner iterates over the source code string and emits tokens.
/// Its cursor contains the character level state. The next token is always 
/// emitted on a scan_token() call.
/// The scanner also keeps track of the current line in the source code and 
/// inserts that information to the tokens.
#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    cur: Cursor<'a>,
    line: u32,
}

#[inline(always)]
fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\r' || c == '\t' || c == '\n'
}

#[inline(always)]
fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

#[inline(always)]
fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

#[inline(always)]
fn is_alphanum(c: char) -> bool {
    is_alpha(c) || is_digit(c)
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str) -> Scanner {
        Scanner {
            cur: Cursor::new(src),
            line: 1,
        }
    }

    /// Emits the next token from the source code. When the scanner has reached 
    /// the end of the file an Eof Token is returned on each further call.
    pub fn scan_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        self.skip_comment_line();

        // check if entire source code was consumed
        if self.cur.at_eof() {
            return self.emit_simple_tok(TokenType::Eof);
        }
        
        if is_alpha(self.cur.peek()) {
            return self.emit_identifier_or_keyword();
        }
        if is_digit(self.cur.peek()) {
            return self.emit_number();
        }
        
        if self.cur.compare('(') {
            return self.emit_simple_tok(TokenType::LeftParen);
        }
        if self.cur.compare(')') {
            return self.emit_simple_tok(TokenType::RightParen);
        }
        if self.cur.compare('{') {
            return self.emit_simple_tok(TokenType::LeftBrace);
        }
        if self.cur.compare('}') {
            return self.emit_simple_tok(TokenType::RightBrace);
        }
        if self.cur.compare(',') {
            return self.emit_simple_tok(TokenType::Comma);
        }
        if self.cur.compare('.') {
            return self.emit_simple_tok(TokenType::Dot);
        }
        if self.cur.compare('-') {
            return self.emit_simple_tok(TokenType::Minus);
        }
        if self.cur.compare('+') {
            return self.emit_simple_tok(TokenType::Plus);
        }
        if self.cur.compare(';') {
            return self.emit_simple_tok(TokenType::Semicolon);
        }
        if self.cur.compare('/') {
            return self.emit_simple_tok(TokenType::Slash);
        }
        if self.cur.compare('*') {
            return self.emit_simple_tok(TokenType::Star);
        }

        if self.cur.compare('"') {
            return self.emit_string();
        }

        if self.cur.compare('!') {
            if self.cur.compare_ahead(1, '=') {
                self.cur.skip();
                return self.emit_simple_tok(TokenType::BangEqual);
            } else {
                return self.emit_simple_tok(TokenType::Bang);
            }
        }
        
        if self.cur.compare('=') {
            if self.cur.compare_ahead(1, '=') {
                self.cur.skip();
                return self.emit_simple_tok(TokenType::EqualEqual);
            } else {
                return self.emit_simple_tok(TokenType::Equal);
            }
        }
        
        if self.cur.compare('>') {
            if self.cur.compare_ahead(1, '=') {
                self.cur.skip();
                return self.emit_simple_tok(TokenType::GreaterEqual);
            } else {
                return self.emit_simple_tok(TokenType::Greater);
            }
        }
        
        if self.cur.compare('<') {
            if self.cur.compare_ahead(1, '=') {
                self.cur.skip();
                return self.emit_simple_tok(TokenType::LessEqual);
            } else {
                return self.emit_simple_tok(TokenType::Less);
            }
        }

        self.emit_error("Unexpected character")
    }

    /// Skips whitespace tokens until a non-whitespace char is reached.
    /// On linebreaks the current source code line is incremented.
    fn skip_whitespace(&mut self) {
        while is_whitespace(self.cur.peek()) {
            if self.cur.peek() == '\n' {
                self.line += 1;
            }
            self.cur.skip();
        }
    }
    
    /// Checks if the cursor is currently on a comment line, if so the entire line 
    /// is skipped. The current source code line is then incremented.
    fn skip_comment_line(&mut self) {
        while self.cur.compare_word("//") {
            self.cur.skip_until(|c| c == '\n');
            self.cur.skip(); // to skip the '\n' char
            self.line += 1;
            // in a new line now -> look for whitespace again
            self.skip_whitespace(); 
        }
    }

    /// Emits Token without lexemes.
    fn emit_simple_tok(&mut self, ttype: TokenType) -> Option<Token> {
        self.cur.skip();
        Some(Token::new(ttype, None, self.line))
    }

    /// Emits an error token.
    fn emit_error(&mut self, err_txt: &str) -> Option<Token> {
        Some(Token::new(TokenType::Error, Some(String::from(err_txt)), self.line))
    }

    /// Used to emit keyword Tokens as they require the scanner to skip multiple
    /// characters. The length of the keywords needs to be passed in skiplen.
    fn emit_keyword(&mut self, skiplen: usize, ttype: TokenType) -> Option<Token> {
        self.cur.skip_n(skiplen);
        Some(Token::new(ttype, None, self.line))
    }

    /// Emits a Identifier Token, consumes the cursor for the lexeme.
    fn emit_identifier(&mut self) -> Option<Token> {
        let cs = self.cur.consume_while(is_alphanum)?;
        let typ = TokenType::Identifier;
        let lexeme = cs.iter().collect::<String>();
        Some(Token::new(typ, Some(lexeme), self.line))
    }

    /// Text literals in the source can either be identifiers or keywords,
    /// as keywords have precedence before identifiers they are checked first.
    /// If the literal at the cursor does not match any Lox keyword an identifier 
    /// is returned.
    fn emit_identifier_or_keyword(&mut self) -> Option<Token> {
        if self.cur.compare_word("and") {
            return self.emit_keyword(3, TokenType::And);
        }
        if self.cur.compare_word("class") {
            return self.emit_keyword(5, TokenType::Class);
        }
        if self.cur.compare_word("else") {
            return self.emit_keyword(4, TokenType::Else);
        }
        if self.cur.compare_word("false") {
            return self.emit_keyword(5, TokenType::False);
        }
        if self.cur.compare_word("for") {
            return self.emit_keyword(3, TokenType::For);
        }
        if self.cur.compare_word("fun") {
            return self.emit_keyword(3, TokenType::Fun);
        }
        if self.cur.compare_word("if") {
            return self.emit_keyword(2, TokenType::If);
        }
        if self.cur.compare_word("nil") {
            return self.emit_keyword(3, TokenType::Nil);
        }
        if self.cur.compare_word("or") {
            return self.emit_keyword(2, TokenType::Or);
        }
        if self.cur.compare_word("print") {
            return self.emit_keyword(5, TokenType::Print);
        }
        if self.cur.compare_word("return") {
            return self.emit_keyword(6, TokenType::Return);
        }
        if self.cur.compare_word("super") {
            return self.emit_keyword(5, TokenType::Super);
        }
        if self.cur.compare_word("this") {
            return self.emit_keyword(4, TokenType::This);
        }
        if self.cur.compare_word("true") {
            return self.emit_keyword(4, TokenType::True);
        }
        if self.cur.compare_word("var") {
            return self.emit_keyword(3, TokenType::Var);
        }
        if self.cur.compare_word("while") {
            return self.emit_keyword(5, TokenType::While);
        }
        self.emit_identifier()
    }
    
    /// Emits a Number Token, consumes the literal from the cursor.
    fn emit_number(&mut self) -> Option<Token> {
        let mut digits = self.cur.consume_while(is_digit)?;
        
        // numbers can also have a fractional part.
        if self.cur.compare('.') && is_digit(self.cur.peek_ahead(1)) {
            digits.push(self.cur.consume()?);
            let mut decimals = self.cur.consume_while(is_digit)?;
            digits.append(&mut decimals);
        }
        Some(Token::new(TokenType::Number, Some(digits.iter().collect()), self.line))
    }

    /// Emits a String Token, consumes the literal from the cursor.
    fn emit_string(&mut self) -> Option<Token> {
        self.cur.skip(); // skip "
        if let Some(cs) = self.cur.consume_until(|c| c == '"') {
            self.cur.skip(); // skip closing "
            return Some(Token::new(TokenType::String, Some(cs.iter().collect()), self.line));
        }
        // there was no closing " -> return error token
        self.emit_error("Unterminated string")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip_whitespace() {
        let src = String::from("          ");
        let mut scn = Scanner::new(src.as_ref());
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Eof);
    }

    #[test]
    fn test_skip_comment() {
        let src = String::from("// this is a comment\n ( ) ");
        let mut scn = Scanner::new(src.as_ref());
        let fst = scn.scan_token().unwrap();
        assert_eq!(fst.get_type(), TokenType::LeftParen);
        let snd = scn.scan_token().unwrap();
        assert_eq!(snd.get_type(), TokenType::RightParen);
    }

    
    #[test]
    fn test_simple_toks() {
        let src = String::from("(){},.-+;/*!");
        let mut scn = Scanner::new(src.as_ref());

        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::LeftParen);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::RightParen);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::LeftBrace);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::RightBrace);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Comma);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Dot);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Minus);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Plus);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Semicolon);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Slash);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Star);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Bang);
    }

    #[test]
    fn test_scan_double_toks() {
        let src = String::from("> >= < <= = == ! !=");
        let mut scn = Scanner::new(src.as_ref());

        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Greater);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::GreaterEqual);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Less);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::LessEqual);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Equal);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::EqualEqual);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Bang);
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::BangEqual);
    }

    #[test]
    fn test_numbers() {
        let src = String::from("  1234232");
        let mut scn = Scanner::new(src.as_ref());
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_lexeme(), "1234232");

        let src = String::from("  123.4232");
        let mut scn = Scanner::new(src.as_ref());
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_lexeme(), "123.4232");
    }

    #[test]
    fn test_string() {
        let src = String::from("   \"foobar\"  ");
        let mut scn = Scanner::new(src.as_ref());
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_lexeme(), "foobar");    
    }

    #[test]
    fn test_error_unterminated_string() {
        let src = String::from("   \"foobar 1323 \n 12 ");
        let mut scn = Scanner::new(src.as_ref());
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Error);
        assert_eq!(tok.get_lexeme(), "Unterminated string");    
    }

    #[test]
    fn test_identifier() {
        let src = String::from("  aVar = 1234;");
        let mut scn = Scanner::new(src.as_ref());
        let tok = scn.scan_token().unwrap();
        assert_eq!(tok.get_type(), TokenType::Identifier);
        assert_eq!(tok.get_lexeme(), "aVar");
    }

    #[test]
    fn test_keywords() {
        let src = String::from("and class else false for fun if nil or print return super this true var while");
        let mut toks: Vec<Token> = Vec::new();
        let mut scn = Scanner::new(src.as_ref());
        while let Some(tok) = scn.scan_token() {
            toks.push(tok.clone());
            if tok.get_type() == TokenType::Eof {
                break;
            }
        }

        assert_eq!(toks[0].get_type(), TokenType::And);
        assert_eq!(toks[1].get_type(), TokenType::Class);
        assert_eq!(toks[2].get_type(), TokenType::Else);
        assert_eq!(toks[3].get_type(), TokenType::False);
        assert_eq!(toks[4].get_type(), TokenType::For);
        assert_eq!(toks[5].get_type(), TokenType::Fun);
        assert_eq!(toks[6].get_type(), TokenType::If);
        assert_eq!(toks[7].get_type(), TokenType::Nil);
        assert_eq!(toks[8].get_type(), TokenType::Or);
        assert_eq!(toks[9].get_type(), TokenType::Print);
        assert_eq!(toks[10].get_type(), TokenType::Return);
        assert_eq!(toks[11].get_type(), TokenType::Super);
        assert_eq!(toks[12].get_type(), TokenType::This);
        assert_eq!(toks[13].get_type(), TokenType::True);
        assert_eq!(toks[14].get_type(), TokenType::Var);
        assert_eq!(toks[15].get_type(), TokenType::While);
        assert_eq!(toks[16].get_type(), TokenType::Eof);
    }
}