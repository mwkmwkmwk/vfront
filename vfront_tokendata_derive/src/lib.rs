use quote::quote;
use syn::{parse_macro_input, punctuated::Punctuated, Data, DeriveInput, Expr, Token, LitStr, LitByteStr};
use itertools::Itertools;

#[proc_macro_derive(TokenData, attributes(keyword, token))]
pub fn derive_token_data(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let mut kw_phf = Vec::new();
    let mut tokens = Vec::new();
    if let Data::Enum(ref enumdata) = input.data {
        for var in enumdata.variants.iter() {
            let varname = &var.ident;
            for attr in var.attrs.iter() {
                if attr.path.is_ident("keyword") {
                    let exprs: Punctuated<Expr, Token![,]> =
                        attr.parse_args_with(Punctuated::parse_terminated).unwrap();
                    assert_eq!(exprs.len(), 2);
                    let kw_str = &exprs[0];
                    let kw_lang = &exprs[1];
                    kw_phf.push(quote! {
                        #kw_str => (#name::#varname, #kw_lang)
                    });
                    break;
                }
                if attr.path.is_ident("token") {
                    let names: Punctuated<LitStr, Token![,]> =
                        attr.parse_args_with(Punctuated::parse_terminated).unwrap();
                    for n in names {
                        tokens.push((n, varname));
                    }
                }
            }
        }
        let mut token_matches = Vec::new();
        tokens.sort_by_key(|(n, _)| n.value().len());
        let token_groups = tokens.into_iter().rev().group_by(|(n, _)| n.value().len());
        for (key, group) in &token_groups {
            let mut group_matches = Vec::new();
            for (n, varname) in group {
                let bytes = LitByteStr::new(n.value().as_bytes(), n.span());
                group_matches.push(quote!(
                    #bytes => return (Self::#varname, #key)
                ));
            }
            token_matches.push(quote!{
                if s.len() >= #key {
                    match &s.as_bytes()[..#key] {
                        #(#group_matches,)*
                        _ => (),
                    }
                }
            });
        }
        quote!(
            impl #name {
                const KEYWORDS : ::phf::Map<&'static str, (#name, u32)> = ::phf::phf_map! {
                    #(#kw_phf,)*
                };
                pub (crate) fn recognize_easy(s: &str) -> (Self, usize) {
                    #(#token_matches)*
                    match s.chars().next() {
                        Some(c) => (Self::Unknown, c.len_utf8()),
                        None => (Self::End, 0),
                    }
                }
            }
        )
        .into()
    } else {
        panic!("should be an enum")
    }
}
