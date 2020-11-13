use itertools::*;

use crate::{ast::*, codebase::*, span::*, types::*};

pub fn resolve_word(def: &Sp<UnresolvedWord>, defs: &Defs) -> SpResult<Word, ResolutionError> {
    let given_sig = if let Some(sig) = &def.sig {
        Some(resolve_sig(sig, defs, &sig.bounds)?)
    } else {
        None
    };
    let given_sig = given_sig.as_ref();
    let nodes = resolve_sequence(&def.nodes, defs, &def.name, given_sig.map(Sp::as_ref))?;
    let sig = seq_sig(&nodes, defs, given_sig, &def.name)?;
    Ok(Word {
        sig: sig.data,
        kind: WordKind::Uiua(nodes.into_iter().map(|n| n.data).collect()),
    })
}

pub fn resolve_sequence(
    nodes: &[Sp<UnresolvedNode>],
    defs: &Defs,
    name: &Sp<Ident>,
    _sig: Option<Sp<&Signature>>,
) -> SpResult<Vec<Sp<Node>>, ResolutionError> {
    let mut resolved_nodes = Vec::new();
    for (i, node) in nodes.iter().enumerate() {
        match &node.data {
            UnresolvedNode::Ident(ident) => {
                if &name.data == ident {
                    resolved_nodes.push(node.span.sp(Node::SelfIdent));
                } else if let Some((hash, _)) = defs.words.by_ident(ident) {
                    resolved_nodes.push(node.span.sp(Node::Ident(*hash)));
                } else if ident.single_and_eq("?") {
                    if i == 0 {
                        return Err(node.span.sp(ResolutionError::IfAtStart));
                    } else {
                        let sig = seq_sig(&resolved_nodes, defs, None, name)?;
                        if let Some(last) = sig.after.last() {
                            if let Type::Prim(Primitive::Quotation(sig)) = last {
                                resolved_nodes.push(
                                    node.span.sp(Node::Ident(
                                        *defs
                                            .words
                                            .by_ident(&Ident::base(format!(
                                                "?{}--{}",
                                                sig.before.len(),
                                                sig.after.len()
                                            )))
                                            .unwrap()
                                            .0,
                                    )),
                                );
                            } else {
                                return Err(node.span.sp(ResolutionError::ExpectedQuotation {
                                    found: last.clone(),
                                }));
                            }
                        } else {
                            return Err(node.span.sp(ResolutionError::ExpectedQuotation {
                                found: Primitive::Unit.into(),
                            }));
                        }
                    }
                } else if ident.single_and_eq("!") {
                    if i == 0 {
                        return Err(node.span.sp(ResolutionError::CallAtStart));
                    } else {
                        let sig = seq_sig(&resolved_nodes, defs, None, name)?;
                        if let Some(last) = sig.after.last() {
                            if let Type::Prim(Primitive::Quotation(sig)) = last {
                                resolved_nodes.push(
                                    node.span.sp(Node::Ident(
                                        *defs
                                            .words
                                            .by_ident(&Ident::base(format!(
                                                "!{}--{}",
                                                sig.before.len(),
                                                sig.after.len()
                                            )))
                                            .unwrap()
                                            .0,
                                    )),
                                );
                            } else {
                                return Err(node.span.sp(ResolutionError::ExpectedQuotation {
                                    found: last.clone(),
                                }));
                            }
                        } else {
                            return Err(node.span.sp(ResolutionError::ExpectedQuotation {
                                found: Primitive::Unit.into(),
                            }));
                        }
                    }
                } else {
                    return Err(node.span.sp(ResolutionError::Unknown(ident.clone())));
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

pub fn resolve_sig(
    sig: &UnresolvedSignature,
    defs: &Defs,
    params: &Option<UnresolvedTypeParams>,
) -> SpResult<Sp<Signature>, ResolutionError> {
    let mut resolved_before = Vec::new();
    let mut resolved_after = Vec::new();
    for (unresolved, resolved) in &mut [
        (&sig.before, &mut resolved_before),
        (&sig.after, &mut resolved_after),
    ] {
        for unresolved in *unresolved {
            if let (Some(type_params), UnresolvedType::Ident(ident)) = (params, &**unresolved) {
                if let Some(i) = type_params.iter().position(|n| ident.single_and_eq(&**n)) {
                    resolved.push(Type::Generic(Generic::new(ident.name.clone(), i as u8)));
                } else {
                    resolved.push(resolve_type(unresolved, defs, params)?);
                }
            } else {
                resolved.push(resolve_type(unresolved, defs, params)?);
            }
        }
    }
    Ok(sig.span.sp(Signature::new(resolved_before, resolved_after)))
}

pub fn resolve_type(
    ty: &Sp<UnresolvedType>,
    defs: &Defs,
    params: &Option<UnresolvedTypeParams>,
) -> SpResult<Type, ResolutionError> {
    match &ty.data {
        UnresolvedType::Prim(prim) => Ok(Type::Prim(resolve_prim(prim, defs, ty.span, params)?)),
        UnresolvedType::Ident(name) => {
            if let Some((_, ty)) = defs.types.by_ident(name) {
                Ok(ty.clone())
            } else {
                Err(ty.span.sp(ResolutionError::Unknown(name.clone())))
            }
        }
    }
}

pub fn resolve_prim(
    prim: &UnresolvedPrimitive,
    defs: &Defs,
    span: Span,
    params: &Option<UnresolvedTypeParams>,
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
    ident: &Sp<Ident>,
) -> SpResult<Sp<Signature>, ResolutionError> {
    let mut iter = nodes.iter();
    let sig = if let Some(node) = iter.next() {
        let mut sig = node_sig(node, defs, given, ident)?;
        for node in iter {
            let next_sig = node_sig(node, defs, given, ident)?;
            sig = node.span.sp(sig.compose(&*next_sig).map_err(|e| {
                node.span.sp(match e {
                    TypeError::Mismatch { .. } => ResolutionError::TypeMismatch {
                        ident: ident.data.clone(),
                        expected: next_sig.data.clone(),
                        found: sig.data.clone(),
                    },
                })
            })?);
        }
        sig
    } else {
        Span::new(Loc::new(1, 1), Loc::new(1, 1)).sp(Signature::new(vec![], vec![]))
    };
    if let Some(given) = given {
        if given != &sig {
            return Err(given.span.sp(ResolutionError::SignatureMismatch {
                ident: ident.data.clone(),
                expected: given.data.clone(),
                found: sig.data,
            }));
        }
    }
    Ok(sig)
}

pub fn node_sig(
    node: &Sp<Node>,
    defs: &Defs,
    self_sig: Option<&Sp<Signature>>,
    ident: &Sp<Ident>,
) -> SpResult<Sp<Signature>, ResolutionError> {
    match &node.data {
        Node::Ident(hash) => {
            Ok(node
                .span
                .sp(defs.words.by_hash(hash).expect("unknown hash").sig.clone()))
        }
        Node::SelfIdent => self_sig
            .cloned()
            .ok_or_else(|| ident.clone().map(ResolutionError::RecursiveNoSignature)),
        Node::Quotation(nodes) => Ok(node.span.sp(Signature::new(
            vec![],
            vec![Primitive::Quotation(seq_sig(nodes, defs, None, ident)?.data).into()],
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
    Unknown(Ident),
    #[error("Rescursive definition {0:?} must have an explicit type signature")]
    RecursiveNoSignature(Ident),
    #[error(
        "Signature mismatch \n\
        {ident} is annoteted with the signature {expected},\n\
        but its body resolves to {found}"
    )]
    SignatureMismatch {
        ident: Ident,
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
