/// Simple macros for generating a token type
/// with [`Display`] trait implementation.
///
/// Generates: [`Token`] and [`TokenType`].
///
/// Syntax:
/// ```
///  str_repr = (char | str)
///  tokens!{ str_repr => KindName }
/// ````
///
/// Example:
/// ```
///  tokens!{
///      '(' => LParen,
///      ')' => RParen,
///  }
/// ```
/// 
/// Generates:
/// ```
///  pub(crate) struct Token {
///      pub kind: TokenType,
///      pub slice: ::std::ops::Range<usize>
///  }
///  
///  impl Display for Token {
///      fm fmt(&self, f: Formatter) -> Result {
///          write!(f, "{}", self.kind)
///      }
///  }
///  pub(crate) struct TokenType {
///      LParen,
///      Rparen,
///  }
///
///  impl Display for TokenType {
///      fm fmt(&self, f: Formatter) -> Result {
///          match &self {
///              Self::Lparen => write!(f, "{}", '('),
///              Self::Rparen => write!(f, "{}", ')'),
///          }
///      }
///  }
/// ```
#[macro_export]
macro_rules! generate_tokens {
    ( $( $repr:expr => $token:ident ),* ) => {
        #[derive(Debug, Clone)]
        #[allow(dead_code)]
        pub(crate) struct Token {
            /// Token kind
            pub kind: TokenKind,
            /// slice range
            pub slice: ::std::ops::Range<usize>
        }

        impl ::std::fmt::Display for Token {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(f, "{}", self.kind)
            }
        }
    
        #[derive(Debug, Clone, PartialEq, Eq)]
        #[allow(dead_code)]
        pub(crate) enum TokenKind {
            EndOfFile,
            Whitespace,
            Unknown,
            $( $token ),*
        }

        impl ::std::fmt::Display for TokenKind {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match &self {
                    Self::EndOfFile => write!(f, "EndOfFile"),
                    Self::Whitespace => write!(f, "Whitespace"),
                    Self::Unknown => write!(f, "Unknown"),
                    $ ( Self::$token => write!(f, "{}", $repr) ),*
                }
            }
        }
    };
}

/// Simple macros for generating a token rules
///
/// Generates: [`Rule`], static ref [`Vec<Rule>`] and Lexer impl.
///
/// Syntax:
/// ```
///  str_repr = (char | str)
///  rule = fn(&str) -> Option<usize>   // returns a len of token str representation
///  token_rules!{ KindName(str_repr) => rule }
/// ```
///
/// Example:
/// ```
///  token_rules!{
///      LParen('(') => |input| /* rule */,
///      RParen(')') => |input| /* rule */
///  }
/// ```
/// 
/// Generates:
/// ```
///  tokens!{
///      '(' => LParen,
///      ')' => RParen,
///  }
///
///  #[derive(Debug, Clone)]
///  pub(crate) struct Rule {
///      pub kind: TokenKind,
///      pub matches: fn(&str) -> Option<usize>,
///  }
/// 
///  lazy_static::lazy_static!{
///      pub(crate) static ref RULES: Vec<Rule> = [
///          Rule { kind: TokenKind::LParen, matches: /* rule */ },
///          Rule { kind: TokenKind::RParen, matches: /* rule */ },
///      ].into_iter().collect();
///  }
///
///  impl<'i> crate::lexer::Lexer<'i> {
///      pub fn tokenize(&mut self) -> Vec<Token>;
///  }
/// ```
#[macro_export]
macro_rules! token_rules {
    ( $( $token:ident($repr:expr) => $rule:expr ),* ) => {
        generate_tokens!{
            $( $repr => $token ),*
        }
        
        #[derive(Debug, Clone)]
        #[allow(dead_code)]
        pub(crate) struct Rule {
            pub kind: TokenKind,
            pub matches: fn(&str) -> Option<usize>,
        }

        lazy_static::lazy_static!{
            pub(crate) static ref RULES: Vec<Rule> = [
                $( Rule { kind: TokenKind::$token, matches: $rule } ),*
            ].into_iter().collect();
        }

        generate_tokenize!{}
    };
}

/// Geneartes [`Lexer`].
#[macro_export]
macro_rules! generate_tokenize {
    () => {
        #[derive(Debug)]
        pub struct Lexer<'i> {
            pub input: &'i str,
            pub position: usize,
        }

        impl<'i> Lexer<'i> {
            pub fn new(input: &'i str) -> Self {
                Self {
                    input,
                    position: 0,
                }
            }
        }
        
        #[allow(dead_code)]
        impl<'i> Lexer<'i> {
            pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
                let mut tokens: Vec<Token> = Vec::new();

                while !self.input.is_empty() {
                    tokens.push(self.next_token()?);
                }
                    
                tokens.push(Token { kind: TokenKind::EndOfFile, slice: self.position..self.position });

                Ok(tokens)
            }

            fn next_token(&mut self) -> Result<Token, String> {
                if self.input.is_empty() {
                    return Err("Whoops something went wrong and input ended...".into());
                }

                if self.input.chars().next().unwrap().is_whitespace() {
                    return Ok(self.space_token());
                }

                Ok(self.known_token().unwrap_or_else(|| self.unknown_token()))
            }
            
            fn space_token(&mut self) -> Token {
                let len = self.input
                    .char_indices()
                    .take_while(|(_, ch)| ch.is_whitespace())
                    .last().unwrap().0 + 1;

                let start = self.position;
                self.input = &self.input[len..];
                self.position += len;

                Token { kind: TokenKind::Whitespace, slice: start..self.position }
            }

            fn unknown_token(&mut self) -> Token {
                let len = self.input
                    .char_indices()
                    .find(|(pos, _)|
                        self.peek_known_token(&self.input[*pos..]).is_some()
                    )
                    .map(|(pos, _)| pos)
                    .unwrap_or(self.input.len());

                let start = self.position;
                self.input = &self.input[len..];
                self.position += len;
            
                Token { kind: TokenKind::Unknown, slice: start..self.position }
            }

            fn known_kind(&self, input: &str) -> Option<(usize, TokenKind)> {
                RULES
                    .iter()
                    .rev()
                    .filter_map(|rule| Some(((rule.matches)(input)?, rule.kind.clone())))
                    .max_by_key(|&(len, _)| len)
            }
            
            fn known_token(&mut self) -> Option<Token> {
                self.known_kind(self.input).and_then(|(len, kind)| {
                    let start = self.position;
                    self.input = &self.input[len..];
                    self.position += len;

                    Some(Token { kind, slice: start..self.position })
                })
            }

            fn peek_known_token(&mut self, input: &str) -> Option<Token> {
                self.known_kind(input).and_then(|(len, kind)| {
                    Some(Token { kind, slice: self.position..self.position+len })
                })
            }
        }
    };
}
