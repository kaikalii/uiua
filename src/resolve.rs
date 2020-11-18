use std::iter::*;

use itertools::*;

use crate::{ast::*, builtin::*, codebase::*, span::*, types::*};

pub enum DefsIO<'a> {
    Resolution(&'a mut Defs),
    Insertion { read: &'a Defs, write: &'a mut Defs },
}

impl<'a> DefsIO<'a> {
    fn read(&self) -> &Defs {
        match self {
            DefsIO::Resolution(defs) => defs,
            DefsIO::Insertion { read, .. } => read,
        }
    }
    fn write(&mut self) -> &mut Defs {
        match self {
            DefsIO::Resolution(defs) => defs,
            DefsIO::Insertion { write, .. } => write,
        }
    }
}

pub fn resolve_item(
    item: &UnresolvedItem,
    path: &Option<String>,
    mut defs: DefsIO,
) -> SpResult<(), ResolutionError> {
    match &item {
        // Uses
        UnresolvedItem::Use(module) => {
            let module_exists = defs
                .read()
                .words
                .names
                .0
                .keys()
                .any(|ident| ident.module.as_ref().map_or(false, |m| m == &module.data))
                || defs
                    .read()
                    .types
                    .names
                    .0
                    .keys()
                    .any(|ident| ident.module.as_ref().map_or(false, |m| m == &module.data));
            if module_exists {
                defs.read().uses.lock().unwrap().insert(module.data.clone());
                Ok(())
            } else {
                Err(module.clone().map(ResolutionError::UnknownModule))
            }
        }
        // Words
        UnresolvedItem::Word(uw) => {
            let word = resolve_word(&uw, defs.read())?;
            let ident = Ident::new(path.clone(), uw.name.data.clone());
            let error_span = if let Some(unres_sig) = &uw.sig {
                uw.name.span - unres_sig.span
            } else {
                uw.name.span
            };
            insert_word(ident, word, &mut defs, error_span)
        }
        // Type aliases
        UnresolvedItem::Type(ut) => {
            // Resolve
            let (alias, words) = resolve_type_alias(ut, defs.read())?;
            // Insert alias
            let ident = Ident::new(path.clone(), ut.name.data.clone());
            insert_type_alias(ident, alias, &mut defs, ut.span)?;
            // Insert words
            for (name, word) in words {
                let ident = Ident::new(path.clone(), name);
                insert_word(ident, word, &mut defs, ut.span)?;
            }
            Ok(())
        }
    }
}

pub fn insert_word(
    ident: Ident,
    word: Word,
    defs: &mut DefsIO,
    error_span: Span,
) -> SpResult<(), ResolutionError> {
    let hash = word.hash_finish(&defs.read().words);
    // Check for identical word
    if defs
        .read()
        .words
        .joint_ident(&ident, &word.sig, Query::Pending)
        .any(|h| h != hash)
    {
        return Err(error_span.sp(ResolutionError::NameAndSignatureExist {
            ident,
            sig: word.sig,
        }));
    }
    defs.write().words.insert(ident, word);
    Ok(())
}

pub fn insert_type_alias(
    ident: Ident,
    alias: TypeAlias,
    defs: &mut DefsIO,
    error_span: Span,
) -> SpResult<(), ResolutionError> {
    let hash = alias.hash_finish(&defs.read().types);
    // Check for identical type
    if defs
        .read()
        .types
        .joint_ident(&ident, &alias.unique_name, Query::Pending)
        .any(|h| h != hash)
    {
        return Err(error_span.sp(ResolutionError::AliasNameExists(ident)));
    }
    defs.write().types.insert(ident, alias);
    Ok(())
}

pub fn resolve_type_alias(
    alias: &Sp<UnresolvedTypeAlias>,
    defs: &Defs,
) -> SpResult<(TypeAlias, Vec<(String, Word)>), ResolutionError> {
    Ok(match &alias.kind.data {
        UnresolvedTypeAliasKind::Enum(variants) => {
            let prim_ty = Type::Prim(
                Primitive::Nat,
                Some(AliasName {
                    name: alias.name.data.clone(),
                    unique: alias.unique,
                }),
            );
            let words: Vec<_> = variants
                .iter()
                .enumerate()
                .map(|(i, name)| {
                    (
                        format!("{}-{}", alias.name.data, name.data),
                        Word {
                            doc: format!("The {} variant of {}", name.data, alias.name.data),
                            sig: Signature::new(vec![], vec![prim_ty.clone()]),
                            kind: WordKind::Uiua(vec![Node::Literal(Literal::Nat(i as u64))]),
                        },
                    )
                })
                .collect();
            let alias = TypeAlias {
                params: Vec::new(),
                unique_name: if alias.unique {
                    Some(alias.name.data.clone())
                } else {
                    None
                },
                ty: prim_ty,
            };
            (alias, words)
        }
        UnresolvedTypeAliasKind::Record { params, fields } => {
            let fields: Vec<(String, Type)> = fields
                .iter()
                .map(|field| {
                    resolve_type(&field.ty, defs, params).map(|ty| (field.name.data.clone(), ty))
                })
                .collect::<Result<_, _>>()?;
            let field_types: Vec<Type> = fields.iter().map(|(_, ty)| ty.clone()).collect();
            let prim_ty = Type::Prim(
                Primitive::Tuple(field_types.clone()),
                Some(AliasName {
                    name: alias.name.data.clone(),
                    unique: alias.unique,
                }),
            );
            let fields_len = fields.len();
            // Constructor
            let mut words = vec![(
                format!("<{}>", alias.name.data),
                Word {
                    doc: format!(
                        "Construct a {} from {} values",
                        alias.name.data,
                        fields
                            .iter()
                            .map(|(name, _)| name.as_str())
                            .intersperse(", ")
                            .collect::<String>()
                    ),
                    sig: Signature::new(field_types, vec![prim_ty.clone()]),
                    kind: WordKind::Builtin(BuiltinWord::TupleCompose(fields_len as u8)),
                },
            )];
            // Getters and Setters
            words.extend(fields.into_iter().enumerate().flat_map(|(i, (name, ty))| {
                // Getters
                once((
                    format!("{}>>", name),
                    Word {
                        doc: format!("Push the top {}'s {} value", alias.name.data, name),
                        sig: Signature::new(
                            vec![prim_ty.clone()],
                            vec![prim_ty.clone(), ty.clone()],
                        ),
                        kind: WordKind::Builtin(BuiltinWord::TupleGet(fields_len as u8, i as u8)),
                    },
                ))
                // Setters
                .chain(once((
                    format!(">>{}", name),
                    Word {
                        doc: format!(
                            "Pop the top value and set the underlying {}'s {} value",
                            alias.name.data, name
                        ),
                        sig: Signature::new(vec![prim_ty.clone(), ty], vec![prim_ty.clone()]),
                        kind: WordKind::Builtin(BuiltinWord::TupleGet(fields_len as u8, i as u8)),
                    },
                )))
            }));
            let alias = TypeAlias {
                params: Vec::new(),
                unique_name: if alias.unique {
                    Some(alias.name.data.clone())
                } else {
                    None
                },
                ty: prim_ty,
            };
            (alias, words)
        }
    })
}

pub fn resolve_word(word: &Sp<UnresolvedWord>, defs: &Defs) -> SpResult<Word, ResolutionError> {
    let given_sig = if let Some(sig) = &word.sig {
        Some(resolve_sig(sig, defs, &word.params)?)
    } else {
        None
    };
    let given_sig = given_sig.as_ref();
    let (nodes, sig) = resolve_sequence(&word.nodes, defs, &word.name, given_sig)?;
    Ok(Word {
        doc: word.doc.clone(),
        sig,
        kind: WordKind::Uiua(nodes.into_iter().map(|n| n.data).collect()),
    })
}

pub fn resolve_sequence(
    nodes: &[Sp<UnresolvedNode>],
    defs: &Defs,
    name: &Sp<String>,
    given_sig: Option<&Sp<Signature>>,
) -> SpResult<(Vec<Sp<Node>>, Signature), ResolutionError> {
    let mut resolved_nodes = Vec::new();
    // let mut sig: Option<Sp<Signature>> = None;
    let mut sig: Option<Sp<Signature>> =
        given_sig.map(|sig| sig.clone().map(|sig| sig.imagine_input_sig()));
    let mut final_prepend = sig.clone().map(|sig| sig.data.after).unwrap_or_default();
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
                    let node_sig = given_sig.cloned().ok_or_else(|| {
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
                        // Use the signature to find a matching word
                        let mut matching_words =
                            defs.words.by_ident_matching_sig(ident, &sig, Query::All);
                        // Try to use only pending if there are too many words
                        if matching_words.len() > 1 {
                            let only_pending =
                                defs.words
                                    .by_ident_matching_sig(ident, &sig, Query::Pending);
                            if !only_pending.is_empty() {
                                matching_words = only_pending;
                            }
                        }
                        if matching_words.is_empty() {
                            return if defs.words.entries_by_ident(ident, Query::All).count() > 0 {
                                Err(node.span.sp(ResolutionError::IncompatibleWord {
                                    ident: ident.clone(),
                                    input_sig: sig.clone(),
                                }))
                            } else {
                                Err(node.span.sp(ResolutionError::UnknownWord(ident.clone())))
                            };
                        } else if matching_words.len() > 1 {
                            return Err(node.span.sp(ResolutionError::MultipleMatchingWords {
                                ident: ident.clone(),
                            }));
                        }
                        matching_words[0].0
                    } else {
                        let mut hashes: Vec<_> = defs
                            .words
                            .entries_by_ident(ident, Query::All)
                            .map(|(hash, _)| hash)
                            .collect();
                        if hashes.len() > 1 {
                            hashes = defs
                                .words
                                .entries_by_ident(ident, Query::Pending)
                                .map(|(hash, _)| hash)
                                .collect()
                        }
                        if hashes.len() == 1 {
                            hashes[0]
                        } else {
                            return Err(node.span.sp(if hashes.is_empty() {
                                ResolutionError::UnknownWord(ident.clone())
                            } else {
                                ResolutionError::MultipleMatchingWords {
                                    ident: ident.clone(),
                                }
                            }));
                        }
                    };
                    let hash = node.span.sp(hash);
                    let word = defs
                        .words
                        .entry_by_hash(&hash, Query::All)
                        .expect("word that was already found isn't present")
                        .item;
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
                resolved_nodes.push(node.span.sp(Node::Quotation(
                    sub_nodes.into_iter().map(|node| node.data).collect(),
                )));
            }
            UnresolvedNode::Unhashed(s) => {
                resolved_nodes.push(node.span.sp(Node::Unhashed(s.clone())))
            }
        }
    }
    // Test the final signature against the given one
    let sig = sig
        .map(|mut sig| {
            final_prepend.append(&mut sig.data.before);
            sig.data.before = final_prepend;
            sig.data
        })
        .unwrap_or_else(|| Signature::new(vec![], vec![]));
    if let Some(given) = given_sig {
        if given.data != sig {
            return Err(given.span.sp(ResolutionError::SignatureMismatch {
                name: name.data.clone(),
                expected: given.data.clone(),
                found: sig,
            }));
        }
    }
    // Always use the given signature if it exists
    let final_sig = given_sig.map(|sig| sig.data.clone()).unwrap_or(sig);
    Ok((resolved_nodes, final_sig))
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
        UnresolvedType::Prim(prim) => {
            Ok(Type::Prim(resolve_prim(prim, defs, ty.span, params)?, None))
        }
        UnresolvedType::Ident(name) => {
            if let Some((_, alias)) = defs.types.entries_by_ident(name, Query::All).next() {
                Ok(alias.item.ty)
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
        Primitive::Never => Primitive::Never,
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
        Primitive::Tuple(types) => Primitive::Tuple(
            types
                .iter()
                .map(|ty| resolve_concrete_type(ty, defs, params))
                .collect::<Result<_, _>>()?,
        ),
    })
}

#[derive(Debug, thiserror::Error)]
pub enum ResolutionError {
    #[error(
        "{ident} expects a before state ( {} ),\n\
        but the words before it have an after state of ( {} )",
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
        \"{ident}\" exists, but no versions of it are compatible with the before state ( {} )",
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
    #[error("There are multiple words in scope that match the name \"{ident}\"")]
    MultipleMatchingWords { ident: Ident },
    #[error(
        "Multiple words with the name \"{ident}\" and a signature compatible with {sig} are declared\n\
        Delete or rename one of them"
    )]
    NameAndSignatureExist { ident: Ident, sig: Signature },
    #[error(
        "Multiple types with the name \"{0}\" are declared\n\
        Delete or rename one of them"
    )]
    AliasNameExists(Ident),
    #[error("Unknown module {0}")]
    UnknownModule(String),
}

fn format_state(types: &[Type]) -> String {
    types
        .iter()
        .map(ToString::to_string)
        .intersperse(" ".into())
        .collect()
}
