use proc_macro::TokenStream;

#[proc_macro_derive(MollieType)]
pub fn derive_mollie_type(_item: TokenStream) -> TokenStream {
    "fn answer() -> u32 { 42 }".parse().unwrap()
}
