"
" Additional highlighting for often used C types
"
syn match   cType `\<[a-z0-9_]\+_t\>`
syn keyword cType i8 u8 i16 u16 i32 u32 i64 u64 f32 f64 usize isize
