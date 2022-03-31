use std::fmt;

/// Types of Tokens of the Lox programming language.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    /// Single character tokens
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    /// Multi character tokens
    Bang, BangEqual, Equal, EqualEqual,
    Greater, GreaterEqual, Less, LessEqual,

    /// Literals
    Identifier, String, Number,

    /// Keywords
    And, Class, Else, False, For, Fun, If,
    Nil, Or, Print, Return, Super, This,
    True, Var, While,

    /// Control tokens
    Error, Eof,
}

/// A lexer Token encapsulates a given type and if necessary a lexeme.
/// Each token also saves the line number of the source code in which it appeared.
#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    lexeme: Option<String>,
    line: u32,
}

impl Token {
    pub fn new(ttype: TokenType, lexeme: Option<String>, line: u32) -> Self {
        Token {
            token_type: ttype,
            lexeme: lexeme,
            line: line,
        }
    }

    /// Return the TokenType of this token.
    pub fn get_type(&self) -> TokenType {
        self.token_type
    }

    /// True if this token has a lexeme, false otherwise.
    pub fn has_lexeme(&self) -> bool {
        self.lexeme.is_some()
    }

    /// Return lexeme if this token has one, otherwise empty string.
    pub fn get_lexeme(&self) -> String {
        if let Some(lexstr) = &self.lexeme {
            return lexstr.clone();
        }
        String::from("")
    }
}

impl Clone for Token {
    fn clone(&self) -> Self {
        if let Some(lex) = &self.lexeme {
            return Token::new(self.token_type, Some(lex.clone()), self.line);
        }
        Token::new(self.token_type, None, self.line)
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.has_lexeme() {
            return write!(f, "Token {{ {} \'{}\' }}", self.get_type(), self.get_lexeme());
        }
        write!(f, "Token {{ {} }}", self.get_type())
    }
}

#[cfg(test)] 
mod tests {
    use super::*;

    #[test]
    fn test_init_with_lexstr() {
        let lxs = String::from("deadbeef");
        let tok = Token::new(TokenType::Identifier, Some(lxs), 1);
        assert_ne!(tok.lexeme, None);
        assert_eq!(tok.lexeme, Some(String::from("deadbeef")));
    }
    
    #[test]
    fn test_init_without_lexstr() {
        let tok = Token::new(TokenType::Plus, None, 1);
        assert_eq!(tok.lexeme, None);
    }
}
