use quote::quote;
use syn::{parse_macro_input, punctuated::Punctuated, Data, DeriveInput, Expr, Token};

#[proc_macro_derive(TokenData, attributes(keyword))]
pub fn derive_token_data(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let mut kw_phf = Vec::new();
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
            }
        }
        quote!(
            impl #name {
                const KEYWORDS : ::phf::Map<&'static str, (#name, u32)> = ::phf::phf_map! {
                    #(#kw_phf,)*
                };
            }
        )
        .into()
    } else {
        panic!("should be an enum")
    }
}
