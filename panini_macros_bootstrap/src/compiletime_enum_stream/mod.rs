mod grammar;

use grammar::compiletime_enum_stream_grammar;

pub struct EnumStreamParser;

impl EnumStreamParser {
    pub fn new() -> Self {
        Parser
    }

    pub fn parse_grammar_from_tts(&mut self, cx: &rs::ExtCtxt, tts: &[rs::TokenTree]) -> Stmts {
        let sess = cx.parse_sess();
        let mut trdr = rs::lexer::new_tt_reader(&sess.span_diagnostic, None, None, tts.to_vec());

        let mut spans = vec![];
        let mut tokens = vec![];
        let mut token_and_span = rs::transcribe::tt_next_token(&mut trdr);
        while token_and_span.tok != rs::Token::Eof {
            let t = mem::replace(&mut token_and_span, rs::transcribe::tt_next_token(&mut trdr));
            spans.push(t.sp);
            tokens.push(t.tok);
        }
        self.parse_grammar(cx, &tokens[..], &spans[..])
    }

    pub fn parse_enum_stream(
        &mut self,
        cx: &rs::ExtCtxt,
        tokens: &[rs::Token],
        spans: &[rs::Span])
        -> Stmts
    {
        let grammar = compiletime_enum_stream_grammar();
        let mut result = parser.parse(tokens.iter().zip(spans.iter().cloned())).unwrap();
        let stmts = result.next().unwrap().clone();
        assert!(result.next().is_none());
        stmts
    }
}
