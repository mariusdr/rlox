#![allow(dead_code)]
use std::str::Chars;

/// Cursor encapsulates all the character level operations on the source code.
/// The state of the cursor is always the next char that is to be considered by the lexer. 
#[derive(Debug, Clone)]
pub struct Cursor<'a> {
    it: Chars<'a>,
}

pub const EOF_CHAR: char = '\0';

impl<'a> Cursor<'a> {
    pub fn new(src: &'a str) -> Cursor<'a> {
        Cursor {
            it: src.chars(),
        }
    }

    /// Return true if end of file is reached.
    pub fn at_eof(&self) -> bool {
        self.it.as_str().is_empty() || self.peek() == EOF_CHAR
    }

    /// Peek at the char on the current cursor position
    /// without consuming it. 
    /// EOF is returned if there is no char.
    pub fn peek(&self) -> char {
        self.it.clone().next().unwrap_or(EOF_CHAR)
    }

    /// Peek n chars ahead without consuming the cursor.
    pub fn peek_ahead(&self, n: usize) -> char {
        let mut itc = self.it.clone();
        for _ in 0..n {
            itc.next();
        }
        itc.next().unwrap_or(EOF_CHAR)
    }

    /// Peek at the cursor and compare with the char c.
    pub fn compare(&self, c: char) -> bool {
        self.peek() == c
    }

    /// Peek ahead and compare with char c.
    pub fn compare_ahead(&self, n: usize, c: char) -> bool {
        self.peek_ahead(n) == c
    }

    /// Peek ahead and compare chars with the given string literal.
    pub fn compare_word(&self, lit: &str) -> bool {
        for (i, c) in lit.chars().enumerate() {
            if !self.compare_ahead(i, c) {
                return false;
            }
        }
        true
    }

    /// Returns the char at the current cursor position 
    /// if one exists. Advances the cursor by one.
    pub fn consume(&mut self) -> Option<char> {
        if self.at_eof() {
            return None;
        }
        let c = self.it.next()?;
        Some(c)
    }

    /// Consumes the cursor if predicate pred evaluates to true.
    pub fn consume_if(&mut self, pred: impl Fn(char) -> bool) -> Option<char> {
        if pred(self.peek()) {
            return self.consume();
        }
        None
    }

    /// Consumes the cursor while the predicate evaluates to true.
    pub fn consume_while(&mut self, pred: impl Fn(char) -> bool) -> Option<Vec<char>> {
        let mut buf: Vec<char> = Vec::new();
        while pred(self.peek()) {
            if let Some(c) = self.consume() {
                buf.push(c);
            } else {
                return None;
            }
        } 
        Some(buf)
    }

    /// Consumes the cursor until the predicate pred evaluates to true.
    pub fn consume_until(&mut self, pred: impl Fn(char) -> bool) -> Option<Vec<char>> {
        self.consume_while(|c| !pred(c))
    }

    /// Consumes the next n chars.
    pub fn consume_n(&mut self, n: usize) -> Option<Vec<char>> {
        let mut buf: Vec<char> = Vec::new();
        for _ in 0..n {
            if let Some(c) = self.consume() {
                buf.push(c);
            } else {
                return None;
            }
        }
        Some(buf)
    }

    /// Advances the cursor without returning anything.
    pub fn skip(&mut self) {
        if !self.at_eof() {
            self.it.next();
        }
    }

    /// Advances the cursor without returning anything if the 
    /// predicate pred evaluates to true for the current char.
    pub fn skip_if(&mut self, pred: impl Fn(char) -> bool) {
        if pred(self.peek()) {
            self.skip();
        }
    }

    /// Advances the cursor without returning anything while the 
    /// predicate pred evaluates to true for the current char.
    pub fn skip_while(&mut self, pred: impl Fn(char) -> bool) {
        while pred(self.peek()) {
            self.skip();
        }
    }

    /// Advances the cursor without returning anything until the 
    /// predicate pred evaluates to true for the current char.
    pub fn skip_until(&mut self, pred: impl Fn(char) -> bool) {
        self.skip_while(|c| !pred(c));
    }

    /// Skips the next n chars of the cursor.
    pub fn skip_n(&mut self, n: usize) {
        for _ in 0..n {
            self.skip();
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_peek() {
        let src = String::from("abcd ");
        let cursor = Cursor::new(src.as_ref());
        assert_eq!(cursor.peek(), 'a');
        
        let src = String::from("");
        let cursor = Cursor::new(src.as_ref());
        assert_eq!(cursor.peek(), EOF_CHAR);
    }

    #[test]
    fn test_peek_ahead() {
        let src = String::from("abcd ");
        let cursor = Cursor::new(src.as_ref());
        assert_eq!(cursor.peek_ahead(1), 'b');
        assert_eq!(cursor.peek_ahead(2), 'c');
        assert_eq!(cursor.peek_ahead(3), 'd');
        assert_eq!(cursor.peek_ahead(4), ' ');
        assert_eq!(cursor.peek_ahead(5), EOF_CHAR);
        assert_eq!(cursor.peek_ahead(6), EOF_CHAR);
    }

    #[test]
    fn test_skip_whitespace() {
        let src = String::from("      abcd ");
        let mut cursor = Cursor::new(src.as_ref());
        assert_ne!(cursor.peek(), 'a');
        
        cursor.skip_while(|c| c == ' ');
        assert_eq!(cursor.peek(), 'a');
    }


    #[test]
    fn test_consume() {
        let src = String::from("abcd ");
        let mut cursor = Cursor::new(src.as_ref());
        assert_ne!(cursor.at_eof(), true);
        assert_eq!(cursor.consume(), Some('a'));
        assert_eq!(cursor.consume(), Some('b'));
        assert_eq!(cursor.consume(), Some('c'));
        assert_eq!(cursor.consume(), Some('d'));
        assert_eq!(cursor.consume(), Some(' '));
        assert_eq!(cursor.consume(), None);
        assert_eq!(cursor.at_eof(), true);
    }
    
    #[test]
    fn test_consume_string_literal() {
        let src = String::from("\"this is a string, foo, bar\" baz");
        let mut cursor = Cursor::new(src.as_ref());
        cursor.skip_if(|c| c == '\"');
        let xs = cursor.consume_until(|c| c == '\"').unwrap().iter().collect::<String>();
        assert_eq!(xs, "this is a string, foo, bar");    
    }

    #[test]    
    fn test_compare_word() {
        let src = String::from("foobar bar baz");
        let mut cursor = Cursor::new(src.as_ref());
        assert_eq!(true, cursor.compare_word("foo"));
        assert_eq!(false, cursor.compare_word("foO"));
        cursor.skip_n(7);
        assert_eq!(true, cursor.compare_word("bar"));
        assert_eq!(false, cursor.compare_word("bXr"));
        cursor.skip_n(4);
        assert_eq!(true, cursor.compare_word("baz"));
        assert_eq!(false, cursor.compare_word("bazz"));
    }

}
