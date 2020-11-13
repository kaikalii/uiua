use crate::{ast::*, codebase::*, span::*, types::*};

pub fn resolve_def(def: &Sp<UnresolvedDef>, defs: &Defs) -> SpResult<Def, ResolutionError> {
    let given_sig = if let Some(sig) = &def.sig {
        Some(resolve_sig(sig, defs, &sig.bounds)?)
    } else {
        None
    };
    let given_sig = given_sig.as_ref();
    let nodes = resolve_sequence(&def.nodes, defs, &def.name, given_sig.map(Sp::as_ref))?;
    let sig = seq_sig(&nodes, defs, given_sig, &def.name)?;
    Ok(Def {
        sig: sig.data,
        kind: DefKind::Uiua(nodes.into_iter().map(|n| n.data).collect()),
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
            UnresolvedNode::Ident(s) => {
                if &name.data == s {
                    resolved_nodes.push(node.span.sp(Node::SelfIdent));
                } else if let Some((hash, _)) = defs.def_by_name(s) {
                    resolved_nodes.push(node.span.sp(Node::Ident(*hash)));
                } else if s == "?" {
                    if i == 0 {
                        return Err(node.span.sp(ResolutionError::CallAtStart));
                    } else {
                        let sig = seq_sig(&resolved_nodes, defs, None, name)?;
                        if let Some(last) = sig.after.last() {
                            if let Type::Prim(Primitive::Quotation(sig)) = last {
                                resolved_nodes.push(
                                    node.span.sp(Node::Ident(
                                        *defs
                                            .def_by_name(&format!(
                                                "?{}--{}",
                                                sig.before.len(),
                                                sig.after.len()
                                            ))
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
                } else if s == "!" {
                    if i == 0 {
                        return Err(node.span.sp(ResolutionError::IfAtStart));
                    } else {
                        let sig = seq_sig(&resolved_nodes, defs, None, name)?;
                        if let Some(last) = sig.after.last() {
                            if let Type::Prim(Primitive::Quotation(sig)) = last {
                                resolved_nodes.push(
                                    node.span.sp(Node::Ident(
                                        *defs
                                            .def_by_name(&format!(
                                                "!{}--{}",
                                                sig.before.len(),
                                                sig.after.len()
                                            ))
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
                    return Err(node.span.sp(ResolutionError::Unknown(s.clone())));
                }
            }
            UnresolvedNode::Literal(lit) => {
                resolved_nodes.push(node.span.sp(Node::Literal(lit.clone())))
            }
            UnresolvedNode::Defered(sub_nodes) => resolved_nodes.push(node.span.sp(Node::Defered(
                resolve_sequence(sub_nodes, defs, name, None)?,
            ))),
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
            if let (Some(type_params), UnresolvedType::Ident(name)) = (params, &**unresolved) {
                if let Some(i) = type_params.iter().position(|n| &**n == name) {
                    resolved.push(Type::Generic(Generic::new(name, i as u8)));
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
            if let Some((_, ty)) = defs.type_by_name(name) {
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
    name: &Sp<String>,
) -> SpResult<Sp<Signature>, ResolutionError> {
    let mut iter = nodes.iter();
    let sig = if let Some(node) = iter.next() {
        let mut sig = node_sig(node, defs, given, name)?;
        for node in iter {
            sig = node.span.sp(sig
                .compose(&*node_sig(node, defs, given, name)?)
                .map_err(|e| node.span.sp(e.into()))?);
        }
        sig
    } else {
        Span::new(Loc::new(1, 1), Loc::new(1, 1)).sp(Signature::new(vec![], vec![]))
    };
    if let Some(given) = given {
        if given != &sig {
            return Err(given.span.sp(ResolutionError::SignatureMismatch {
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
    name: &Sp<String>,
) -> SpResult<Sp<Signature>, ResolutionError> {
    match &node.data {
        Node::Ident(hash) => {
            Ok(node
                .span
                .sp(defs.def_by_hash(hash).expect("unknown hash").sig.clone()))
        }
        Node::SelfIdent => self_sig
            .cloned()
            .ok_or_else(|| name.clone().map(ResolutionError::RecursiveNoSignature)),
        Node::Defered(nodes) => Ok(node.span.sp(Signature::new(
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
    #[error("{0}")]
    Type(#[from] TypeError),
    #[error("Unknown name {0:?}")]
    Unknown(String),
    #[error("Rescursive definition {0:?} must have an explicit type signature")]
    RecursiveNoSignature(String),
    #[error("Expected signature {expected}, but found {found}")]
    SignatureMismatch {
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
