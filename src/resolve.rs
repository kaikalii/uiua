use crate::{ast::*, codebase::*, types::*};

pub fn resolve_def(def: &UnresolvedDef, defs: &Defs) -> Result<Def, ResolutionError> {
    let given_sig = if let Some(sig) = &def.sig {
        Some(resolve_sig(sig, defs)?)
    } else {
        None
    };
    let nodes = resolve_sequence(&def.nodes, defs, Some(&def.name), given_sig.as_ref())?;
    let sig = seq_sig(&nodes, defs, given_sig.as_ref())?;
    Ok(Def {
        sig,
        kind: DefKind::Uiua(nodes),
    })
}

pub fn resolve_sequence(
    nodes: &[UnresolvedNode],
    defs: &Defs,
    name: Option<&str>,
    _sig: Option<&Signature>,
) -> Result<Vec<Node>, ResolutionError> {
    let mut resolved_nodes = Vec::new();
    for node in nodes {
        match node {
            UnresolvedNode::Ident(s) => {
                if name.map_or(false, |n| n == s) {
                    resolved_nodes.push(Node::SelfIdent);
                } else if let Some((hash, _)) = defs.def_by_name(s) {
                    resolved_nodes.push(Node::Ident(*hash));
                } else {
                    return Err(ResolutionError::Unknown(s.clone()));
                }
            }
            UnresolvedNode::Literal(lit) => resolved_nodes.push(Node::Literal(lit.clone())),
            UnresolvedNode::Defered(sub_nodes) => resolved_nodes.push(Node::Defered(
                resolve_sequence(sub_nodes, defs, None, None)?,
            )),
        }
    }
    Ok(resolved_nodes)
}

pub fn resolve_sig(
    sig: &Signature<UnresolvedType>,
    defs: &Defs,
) -> Result<Signature, ResolutionError> {
    let mut resolved_before = Vec::new();
    let mut resolved_after = Vec::new();
    for (unresolved, resolved) in &mut [
        (&sig.before, &mut resolved_before),
        (&sig.after, &mut resolved_after),
    ] {
        for unresolved in *unresolved {
            resolved.push(resolve_type(unresolved, defs)?);
        }
    }
    Ok(Signature::new(resolved_before, resolved_after))
}

pub fn resolve_type(ty: &UnresolvedType, defs: &Defs) -> Result<Type, ResolutionError> {
    match ty {
        UnresolvedType::Prim(prim) => Ok(Type::Prim(resolve_prim(prim, defs)?)),
        UnresolvedType::Other(name) => {
            if let Some((_, ty)) = defs.type_by_name(name) {
                Ok(ty.clone())
            } else {
                Err(ResolutionError::Unknown(name.clone()))
            }
        }
    }
}

pub fn resolve_prim(
    prim: &Primitive<UnresolvedType>,
    defs: &Defs,
) -> Result<Primitive, ResolutionError> {
    Ok(match prim {
        Primitive::Bool => Primitive::Bool,
        Primitive::Nat => Primitive::Nat,
        Primitive::Int => Primitive::Int,
        Primitive::Float => Primitive::Float,
        Primitive::Char => Primitive::Char,
        Primitive::Text => Primitive::Text,
        Primitive::List(ty) => Primitive::List(Box::new(resolve_type(ty, defs)?)),
        Primitive::Op(sig) => Primitive::Op(resolve_sig(sig, defs)?),
    })
}

pub fn seq_sig(
    nodes: &[Node],
    defs: &Defs,
    given: Option<&Signature>,
) -> Result<Signature, ResolutionError> {
    let mut iter = nodes.iter();
    let sig = if let Some(node) = iter.next() {
        let mut sig = node_sig(node, defs, given)?;
        for node in iter {
            sig = sig.compose(&node_sig(node, defs, given)?)?;
        }
        sig
    } else {
        Signature::new(vec![], vec![])
    };
    if let Some(given) = given {
        if given != &sig {
            return Err(ResolutionError::SignatureMismatch {
                expected: given.clone(),
                found: sig,
            });
        }
    }
    Ok(sig)
}

pub fn node_sig(
    node: &Node,
    defs: &Defs,
    self_sig: Option<&Signature>,
) -> Result<Signature, ResolutionError> {
    match node {
        Node::Ident(hash) => Ok(defs.def_by_hash(hash).expect("unknown hash").sig.clone()),
        Node::SelfIdent => self_sig
            .cloned()
            .ok_or_else(|| ResolutionError::RecursiveNoSignature(String::new())),
        Node::Defered(nodes) => seq_sig(nodes, defs, None),
        Node::Literal(lit) => Ok(Signature::new(vec![], vec![lit.as_primitive().into()])),
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
}
