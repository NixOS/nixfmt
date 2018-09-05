// 1. Remove trailing whitespace and compress multiple newlines to two newlines.
// 2. Find nodes that need to start a new line: All nodes with leading comments,
//    all list and set closing nodes, every first element in lists and sets.
// 3. Determine indentation level of every node.
// 4. Determine single line length of every node.
// 5. Determine which nodes fit on a single line.
// 6. Format trivia.
// 7. Indent multiline comments and strings.
// 8. Insert double newlines before and after nodes spanning multiple lines in
//    lists and sets.
//
// Trailing trivia is on the same line, any trivia on the next line is leading
// for the next token, except if it's the last non-eof token, in which case it's
// all trailing trivia anyway.
//
// Single line comments do not include their terminating newline. Having a
// single line comment not followed by a newline is a bug.
//
// TODO: Handle trailing trivia on last node

extern crate arenatree;
extern crate smol_str;
extern crate rnix;

use smol_str::SmolStr;
use arenatree::{Arena, NodeId};

use rnix::value::Value;
use rnix::parser::{ASTKind, ASTNode, AST, Data};
use rnix::tokenizer::{Token, TokenKind, Trivia, Meta, Span};

use std::cmp::{min, max};
use std::{env, fs, fmt};

pub struct FormatNode {
    pub children: Vec<FormatNode>,
    pub kind: ASTKind,
    pub span: Span,
    pub data: Data,
    pub indent_level: usize, // number of spaces
    pub single_line_length: usize, // number of characters
    pub line_span: usize, // number of newlines contained + 1
}

impl FormatNode {
    fn fmt_level(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        write!(f, "{}{:?}, {:?}, indent: {:?}, length: {:?}, span: {:?}\n",
               "  ".repeat(level), self.kind, self.span, self.indent_level,
               self.single_line_length, self.line_span)?;
        write!(f, "{}        data: {:?}\n", "  ".repeat(level), self.data)?;
        for child in &self.children {
            child.fmt_level(f, level + 1)?;
        }
        Ok(())
    }
}

impl fmt::Debug for FormatNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_level(f, 0)
    }
}

fn span_size(span: Span) -> usize {
    match span {
        Span { start, end: Some(end) } => (end - start) as usize,
        _ => 0,
    }
}

fn spacing_size(kind: ASTKind, children: &Vec<FormatNode>) -> usize {
    match kind {
        ASTKind::List | ASTKind::Set => children.len() - 1,
        ASTKind::Apply => 1,
        ASTKind::Assert | ASTKind::Operation => 2,
        _ => 0,
    }
}

fn leaf_size(data: &Data, span: Span) -> usize {
    match data {
        Data::Ident(_, _) | Data::Token(_, _) | Data::Value(_, _) =>
            span_size(span),
        _ => 0,
    }
}

fn convert(arena: &mut Arena<ASTNode>, id: NodeId, level: usize) -> FormatNode {
    let node = arena.take(id);
    let level = match node.kind {
        ASTKind::ListItem | ASTKind::SetEntry => level + 2,
        _ => level,
    };

    let child_ids: Vec<_> = node.children(arena).collect();
    let children: Vec<_> = child_ids.iter()
        .map(|child| convert(arena, *child, level))
        .collect();

    let single_line_length = leaf_size(&node.data, node.span)
        + spacing_size(node.kind, &children)
        + children.iter().map(|child| child.single_line_length).sum::<usize>();

    return FormatNode {
        children: children,
        kind: node.kind,
        span: node.span,
        data: node.data,
        indent_level: level,
        single_line_length: single_line_length,
        line_span: 0,
    }
}

fn node_meta(node: &FormatNode) -> Option<&Meta> {
    match &node.data {
        Data::Token(meta, _) => Some(meta),
        Data::Ident(meta, _) => Some(meta),
        Data::Value(meta, _) => Some(meta),
        _ => None,
    }
}

fn node_meta_mut(node: &mut FormatNode) -> Option<&mut Meta> {
    match &mut node.data {
        Data::Token(ref mut meta, _) => Some(meta),
        Data::Ident(ref mut meta, _) => Some(meta),
        Data::Value(ref mut meta, _) => Some(meta),
        _ => None,
    }
}

fn has_leading_comments(node: &FormatNode) -> bool {
    match node_meta(node) {
        Some(meta) => meta.leading.iter().any(Trivia::is_comment),
        None => false,
    }
}

fn format_comment(string: SmolStr, indent: usize) -> SmolStr {
    string
}

// Reformat all comments to start at the same indentation level
fn format_trivia(trivia: Vec<Trivia>, indent: usize) -> Vec<Trivia> {
    let mut result = vec![];
    let mut current_newlines = 0;

    for trivium in trivia {
        match trivium {
            Trivia::Comment { span, multiline, content } => {
                result.push(Trivia::Newlines(max(current_newlines, 1)));
                result.push(Trivia::Spaces(indent as u32));
                result.push(Trivia::Comment {
                    span: span,
                    multiline: multiline,
                    content: format_comment(content, indent),
                });
            }

            Trivia::Spaces(_) | Trivia::Tabs(_) => {}
            Trivia::Newlines(count) => {
                current_newlines = min(current_newlines + count, 2);
            }
        }
    }
    result.push(Trivia::Newlines(max(current_newlines, 1)));
    result.push(Trivia::Spaces(indent as u32));
    result
}

fn format_all_trivia(node: &mut FormatNode) {
    let indent_level = node.indent_level;
    if let Some(meta) = node_meta_mut(node) {
        let trivia = std::mem::replace(&mut meta.leading, vec![]);
        meta.leading = format_trivia(trivia, indent_level);
    }
    node.children.iter_mut().for_each(format_all_trivia);
}

fn format(node: FormatNode) -> FormatNode {
    node
}

fn print_structure(arena: &Arena<ASTNode>, id: NodeId, level: usize) {
    let node = &arena[id];

    println!("{}{:?} {:?}", "  ".repeat(level), node.kind, node.data);
    for child in node.children(arena) {
        print_structure(arena, child, level + 1)
    }
}

fn main() {
    let mut iter = env::args().skip(1).peekable();
    if iter.peek().is_none() {
        eprintln!("Usage: format <file>");
        return;
    }

    for file in iter {
        let content = match fs::read_to_string(file) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("error reading file: {}", err);
                return;
            }
        };

        let mut ast = match rnix::parse(&content) {
            Ok(ast) => ast,
            Err(err) => {
                eprintln!("parsing error: {:?}", err);
                return;
            }
        };

        let converted = convert(&mut ast.arena, ast.root, 0);
        eprintln!("{:?}", converted);
    }
}
