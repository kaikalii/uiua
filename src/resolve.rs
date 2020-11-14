use itertools::*;

use crate::{ast::*, codebase::*, span::*, types::*};

pub fn resolve_word(word: &Sp<UnresolvedWord>, defs: &Defs) -> SpResult<Word, ResolutionError> {
    let given_sig = if let Some(sig) = &word.sig {
        Some(resolve_sig(sig, defs, &sig.params)?)
    } else {
        None
    };
    let given_sig = given_sig.as_ref();
    let nodes = resolve_sequence(&word.nodes, defs, &word.name, given_sig.map(Sp::as_ref))?;
    let sig = seq_sig(&nodes, defs, given_sig, &word.name)?;
    Ok(Word {
        sig: sig.data,
        kind: WordKind::Uiua(nodes.into_iter().map(|n| n.data).collect()),
    })
}

pub fn resolve_sequence(
    nodes: &[Sp<UnresolvedNode>],
    defs: &Defs,
    name: &Sp<String>,
    _sig: Option<Sp<&Signature>>,
) -> SpResult<Vec<Sp<Node>>, ResolutionError> {
    let mut resolved_nodes = Vec::new();
    for (i, node) in nodes.iter().enumerate() {
        match &node.data {
            UnresolvedNode::Ident(ident) => {
                if ident.single_and_eq(&name.data) {
                    resolved_nodes.push(node.span.sp(Node::SelfIdent));
                } else if let Some((hash, _)) = defs.words.by_ident(ident) {
                    resolved_nodes.push(node.span.sp(Node::Ident(*hash)));
                } else if ident.single_and_eq("?") {
                    if i == 0 {
                        return Err(node.span.sp(ResolutionError::IfAtStart));
                    } else {
                        let sig = seq_sig(&resolved_nodes, defs, None, name)?;
                        resolved_nodes.push(resolve_magic(defs, '?', node.span, &sig)?);
                    }
                } else if ident.single_and_eq("!") {
                    if i == 0 {
                        return Err(node.span.sp(ResolutionError::CallAtStart));
                    } else {
                        let sig = seq_sig(&resolved_nodes, defs, None, name)?;
                        resolved_nodes.push(resolve_magic(defs, '!', node.span, &sig)?);
                    }
                } else {
                    return Err(node.span.sp(ResolutionError::UnknownWord(ident.clone())));
                }
            }
            UnresolvedNode::Literal(lit) => {
                resolved_nodes.push(node.span.sp(Node::Literal(lit.clone())))
            }
            UnresolvedNode::Quotation(sub_nodes) => resolved_nodes.push(node.span.sp(
                Node::Quotation(resolve_sequence(sub_nodes, defs, name, None)?),
            )),
        }
    }
    Ok(resolved_nodes)
}

fn resolve_magic(
    defs: &Defs,
    magic_char: char,
    span: Span,
    sig: &Sp<Signature>,
) -> SpResult<Sp<Node>, ResolutionError> {
    if let Some(last) = sig.after.last() {
        if let Type::Prim(Primitive::Quotation(sig)) = last {
            Ok(span.sp(Node::Ident(
                *defs
                    .words
                    .by_ident(&Ident::base(format!(
                        "{}{}--{}",
                        magic_char,
                        sig.before.len(),
                        sig.after.len()
                    )))
                    .unwrap()
                    .0,
            )))
        } else {
            Err(span.sp(ResolutionError::ExpectedQuotation {
                found: last.clone(),
            }))
        }
    } else {
        Err(span.sp(ResolutionError::ExpectedQuotation {
            found: Primitive::Unit.into(),
        }))
    }
}

pub fn resolve_sig(
    sig: &Sp<UnresolvedSignature>,
    defs: &Defs,
    params: &Sp<UnresolvedParams>,
) -> SpResult<Sp<Signature>, ResolutionError> {
    let mut resolved_before = Vec::new();
    let mut resolved_after = Vec::new();
    for (unresolved, resolved) in &mut [
        (&sig.before, &mut resolved_before),
        (&sig.after, &mut resolved_after),
    ] {
        for unresolved in *unresolved {
            resolved.push(resolve_type(unresolved, defs, params)?);
        }
    }
    Ok(sig.span.sp(Signature::explicit(
        params.iter().map(|param| param.data.clone()).collect(),
        resolved_before,
        resolved_after,
    )))
}

pub fn resolve_type(
    ty: &Sp<UnresolvedType>,
    defs: &Defs,
    params: &Sp<UnresolvedParams>,
) -> SpResult<Type, ResolutionError> {
    if let UnresolvedType::Ident(ident) = &**ty {
        if let Some(i) = params.iter().position(|param| ident.single_and_eq(&param)) {
            Ok(Type::Generic(Generic::new(ident.name.clone(), i as u8)))
        } else {
            resolve_concrete_type(ty, defs, params)
        }
    } else {
        resolve_concrete_type(ty, defs, params)
    }
}

fn resolve_concrete_type(
    ty: &Sp<UnresolvedType>,
    defs: &Defs,
    params: &Sp<UnresolvedParams>,
) -> SpResult<Type, ResolutionError> {
    match &ty.data {
        UnresolvedType::Prim(prim) => Ok(Type::Prim(resolve_prim(prim, defs, ty.span, params)?)),
        UnresolvedType::Ident(name) => {
            if let Some((_, ty)) = defs.types.by_ident(name) {
                Ok(ty.clone())
            } else {
                Err(ty.span.sp(ResolutionError::UnknownType(name.clone())))
            }
        }
    }
}

pub fn resolve_prim(
    prim: &UnresolvedPrimitive,
    defs: &Defs,
    span: Span,
    params: &Sp<UnresolvedParams>,
) -> SpResult<Primitive, ResolutionError> {
    Ok(match prim {
        Primitive::Unit => Primitive::Unit,
        Primitive::Bool => Primitive::Bool,
        Primitive::Nat => Primitive::Nat,
        Primitive::Int => Primitive::Int,
        Primitive::Float => Primitive::Float,
        Primitive::Char => Primitive::Char,
        Primitive::Text => Primitive::Text,
        Primitive::List(ty) => Primitive::List(Box::new(resolve_type(ty, defs, params)?)),
        Primitive::Quotation(sig) => {
            Primitive::Quotation(resolve_sig(&span.sp(sig.clone()), defs, params)?.data)
        }
    })
}

pub fn seq_sig(
    nodes: &[Sp<Node>],
    defs: &Defs,
    given: Option<&Sp<Signature>>,
    name: &Sp<String>,
) -> SpResult<Sp<Signature>, ResolutionError> {
    let mut iter = nodes.iter();
    let sig = if let Some(node) = iter.next() {
        let mut sig = node_sig(node, defs, given, name)?;
        for node in iter {
            let next_sig = node_sig(node, defs, given, name)?;
            sig = node.span.sp(sig.compose(&*next_sig).map_err(|e| {
                node.span.sp(match e {
                    TypeError::Mismatch { .. } => {
                        let ident = match &node.data {
                            Node::Ident(hash) => defs
                                .words
                                .ident_by_hash(hash)
                                .expect("unknown hash")
                                .clone(),
                            Node::SelfIdent => Ident::no_module(name.data.clone()),
                            Node::Literal(_) => {
                                panic!("composition with literal node has type mismatch")
                            }
                            Node::Quotation(_) => {
                                panic!("composition with quotation node has type mismatch")
                            }
                        };
                        ResolutionError::TypeMismatch {
                            ident,
                            expected: next_sig.data.clone(),
                            found: sig.data.clone(),
                        }
                    }
                })
            })?);
        }
        sig
    } else {
        Span::new(Loc::new(1, 1), Loc::new(1, 1)).sp(Signature::new(vec![], vec![]))
    };
    if let Some(given) = given {
        if !sig.is_equivalent_to(given) {
            return Err(given.span.sp(ResolutionError::SignatureMismatch {
                name: name.data.clone(),
                expected: given.data.clone(),
                found: sig.data,
            }));
        }
    }
    Ok(given.cloned().unwrap_or(sig))
}

pub fn node_sig(
    node: &Sp<Node>,
    defs: &Defs,
    self_sig: Option<&Sp<Signature>>,
    name: &Sp<String>,
) -> SpResult<Sp<Signature>, ResolutionError> {
    match &node.data {
        Node::Ident(hash) => {
            Ok(node
                .span
                .sp(defs.words.by_hash(hash).expect("unknown hash").sig.clone()))
        }
        Node::SelfIdent => self_sig.cloned().ok_or_else(|| {
            name.clone()
                .map(Ident::no_module)
                .map(ResolutionError::RecursiveNoSignature)
        }),
        Node::Quotation(nodes) => Ok(node.span.sp(Signature::new(
            vec![],
            vec![Primitive::Quotation(seq_sig(nodes, defs, None, name)?.data).into()],
        ))),
        Node::Literal(lit) => Ok(node
            .span
            .sp(Signature::new(vec![], vec![lit.as_primitive().into()]))),
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ResolutionError {
    #[error(
        "{ident} expects an input state ({}),\n\
        but the words before it have output state ({})",
        format_state(&expected.before),
        format_state(&found.after)
    )]
    TypeMismatch {
        ident: Ident,
        expected: Signature,
        found: Signature,
    },
    #[error("Unknown word \"{0}\"")]
    UnknownWord(Ident),
    #[error("Unknown type \"{0}\"")]
    UnknownType(Ident),
    #[error("Rescursive definition {0:?} must have an explicit type signature")]
    RecursiveNoSignature(Ident),
    #[error(
        "Signature mismatch \n\
        {name} is annotated with the signature {expected},\n\
        but its body resolves to {found}"
    )]
    SignatureMismatch {
        name: String,
        expected: Signature,
        found: Signature,
    },
    #[error("Unenumerated calls cannot be at the beginning of a definition")]
    CallAtStart,
    #[error("Unenumerated if's cannot be at the beginning of a definition")]
    IfAtStart,
    #[error("Expected quotation, found {found}")]
    ExpectedQuotation { found: Type },
}

fn format_state(types: &[Type]) -> String {
    types
        .iter()
        .map(ToString::to_string)
        .intersperse(" ".into())
        .collect()
}
