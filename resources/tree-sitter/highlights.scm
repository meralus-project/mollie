"fn" @keyword
"import" @keyword
"impl" @keyword
"trait" @keyword
"struct" @keyword
"postfix" @keyword
"enum" @keyword
"declare" @keyword
"from" @keyword
"if" @keyword
"else" @keyword
"is" @operator
"as" @operator
"for" @keyword
"in" @keyword
"while" @keyword
"const" @keyword
(boolean) @keyword
"false" @keyword
"@" @attr
(attribute "[" "]") @attr
(attribute name: (identifier) @attr)
(scoped_identifier (identifier) @type)
(scoped_type_identifier (identifier) @type)
(path (identifier) @type)
(impl trait: (identifier) @type)
(primitive_type) @type
(node_expression name: (identifier) @type)
(type (identifier) @type)
(self) @keyword
(number) @number
(field name: (identifier) @field)
(func_header name: (identifier) @func_name)
(enum_variant name: (identifier) @enum_variant)
(generic_name name: (identifier) @type)
(impl generics: (identifier) @type)
(string) @string
(line_comment) @comment
(identifier) @var
