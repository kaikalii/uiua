use itertools::*;

use crate::{ast::*, builtin::*, codebase::*, span::*, types::*};

pub fn resolve_word(word: &Sp<UnresolvedWord>, defs: &mut Defs) -> SpResult<Word, ResolutionError> {
    let given_sig = if let Some(sig) = &word.sig {
        Some(resolve_sig(sig, defs, &word.params)?)
    } else {
        None
    };
    let given_sig = given_sig.as_ref();
    let (nodes, sig) = resolve_sequence(&word.nodes, defs, &word.name, given_sig.map(Sp::as_ref))?;
    Ok(Word {
        sig,
        kind: WordKind::Uiua(nodes.into_iter().map(|n| n.data).collect()),
    })
}

pub fn resolve_sequence(
    nodes: &[Sp<UnresolvedNode>],
    defs: &mut Defs,
    name: &Sp<String>,
    given_sig: Option<Sp<&Signature>>,
) -> SpResult<(Vec<Sp<Node>>, Signature), ResolutionError> {
    let mut resolved_nodes = Vec::new();
    let mut sig: Option<Sp<Signature>> = None;
    macro_rules! compose {
        ($next:expr, $name:expr) => {{
            let next = $next;
            sig = Some(if let Some(sig) = sig {
                next.span.sp(sig
                    .compose(&next.data)
                    .map_err(|e| next.span.sp(e.name($name)))?)
            } else {
                next
            });
        }};
    }
    for (i, node) in nodes.iter().enumerate() {
        match &node.data {
            UnresolvedNode::Ident(ident) => {
                if ident.single_and_eq(&name.data) {
                    // Self-identifier
                    let node_sig = given_sig.map(|sig| sig.cloned()).ok_or_else(|| {
                        name.clone()
                            .map(Ident::no_module)
                            .map(ResolutionError::RecursiveNoSignature)
                    })?;
                    compose!(node_sig, ident.to_string());
                    resolved_nodes.push(node.span.sp(Node::SelfIdent));
                } else {
                    // General word lookup
                    let sig_for_lookup = if let (0, Some(given)) = (i, given_sig) {
                        Some(given.imagine_input_sig())
                    } else {
                        sig.clone().map(|sig| sig.data)
                    };
                    let hash = if let Some(sig) = &sig_for_lookup {
                        defs.words
                            .by_ident_matching_sig(ident, &sig)
                            .next()
                            .map(|(hash, _)| *hash)
                    } else {
                        defs.words.by_ident(ident).next().map(|(hash, _)| *hash)
                    };
                    let matching_idents = defs.words.by_ident(ident).count();
                    let hash = if let Some(hash) = hash {
                        node.span.sp(hash)
                    } else if ident.single_and_eq("?") {
                        // Ifs
                        if let Some(sig) = &sig {
                            resolve_magic(defs, BuiltinWord::If, node.span, &sig)?
                        } else {
                            return Err(node.span.sp(ResolutionError::IfAtStart));
                        }
                    } else if ident.single_and_eq("!") {
                        // Calls
                        if let Some(sig) = &sig {
                            resolve_magic(defs, BuiltinWord::Call, node.span, &sig)?
                        } else {
                            return Err(node.span.sp(ResolutionError::CallAtStart));
                        }
                    } else if matching_idents == 0 {
                        return Err(node.span.sp(ResolutionError::UnknownWord(ident.clone())));
                    } else {
                        return if let Some(sig) = &sig_for_lookup {
                            Err(node.span.sp(ResolutionError::IncompatibleWord {
                                ident: ident.clone(),
                                input_sig: sig.clone(),
                            }))
                        } else {
                            Err(node.span.sp(ResolutionError::UnknownWord(ident.clone())))
                        };
                    };
                    let word = defs
                        .words
                        .by_hash(&hash)
                        .expect("word that was already found isn't present");
                    compose!(node.span.sp(word.sig.clone()), ident.to_string());
                    resolved_nodes.push(hash.map(Node::Ident));
                }
            }
            UnresolvedNode::Literal(lit) => {
                let node_sig = Signature::new(vec![], vec![lit.as_primitive().into()]);
                #[allow(unreachable_code)]
                {
                    compose!(node.span.sp(node_sig), panic!("literal composition failed"));
                }
                resolved_nodes.push(node.span.sp(Node::Literal(lit.clone())))
            }
            UnresolvedNode::Quotation(sub_nodes) => {
                let (sub_nodes, sub_sig) = resolve_sequence(sub_nodes, defs, name, None)?;
                let node_sig = Signature::new(vec![], vec![Primitive::Quotation(sub_sig).into()]);
                #[allow(unreachable_code)]
                {
                    compose!(
                        node.span.sp(node_sig),
                        panic!("quotation composition failed")
                    );
                }
                resolved_nodes.push(node.span.sp(Node::Quotation(sub_nodes)));
            }
        }
    }
    // Test the final signature against the given one
    let sig = sig
        .map(|sig| sig.data)
        .unwrap_or_else(|| Signature::new(vec![], vec![]));
    if let Some(given) = given_sig {
        if !given.is_equivalent_to(&sig) {
            return Err(given.span.sp(ResolutionError::SignatureMismatch {
                name: name.data.clone(),
                expected: given.data.clone(),
                found: sig,
            }));
        }
    }
    // Always use the given signature if it exists
    let sig = given_sig.map(|sig| sig.data.clone()).unwrap_or(sig);
    Ok((resolved_nodes, sig))
}

fn resolve_magic<F>(
    defs: &mut Defs,
    handler: F,
    span: Span,
    sig: &Sp<Signature>,
) -> SpResult<Sp<Hash>, ResolutionError>
where
    F: Fn(u8, u8) -> BuiltinWord,
{
    if let Some(last) = sig.after.last() {
        if let Type::Prim(Primitive::Quotation(sig)) = last {
            let builtin = handler(sig.before.len() as u8, sig.after.len() as u8);
            Ok(span.sp(defs.words.insert(builtin.ident(), builtin.into())))
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
    Ok(sig.span.sp(Signature::new(resolved_before, resolved_after)))
}

pub fn resolve_type(
    ty: &Sp<UnresolvedType>,
    defs: &Defs,
    params: &Sp<UnresolvedParams>,
) -> SpResult<Type, ResolutionError> {
    if let UnresolvedType::Ident(ident) = &**ty {
        if let Some(i) = params.iter().position(|param| ident.single_and_eq(&param)) {
            Ok(Type::Generic(Generic::new(
                ident.name.clone(),
                i as u8,
                false,
            )))
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
            if let Some((_, ty)) = defs.types.by_ident(name).next() {
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

#[derive(Debug, thiserror::Error)]
pub enum ResolutionError {
    #[error(
        "{ident} expects a before state ({}),\n\
        but the words before it have an after state of ({})",
        format_state(&output.before),
        format_state(&input.after)
    )]
    TypeMismatch {
        ident: Ident,
        output: Signature,
        input: Signature,
    },
    #[error("Unknown word \"{0}\"")]
    UnknownWord(Ident),
    #[error("Unknown type \"{0}\"")]
    UnknownType(Ident),
    #[error(
        "Incompatible word \"{ident}\"\n\
        \"{ident}\" exists, but no versions of it are compatible with the before state ({})",
        format_state(&input_sig.after)
    )]
    IncompatibleWord { ident: Ident, input_sig: Signature },
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
