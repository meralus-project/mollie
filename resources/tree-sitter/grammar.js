/**
 * @file Mollie grammar for tree-sitter
 * @author AivingDev <bellyinkognito05@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

export default grammar({
  name: "mollie",

  extras: ($) => [/\s/, $.line_comment],
  // conflicts: ($) => [[$.array_expression]],
  word: ($) => $.identifier,

  rules: {
    // TODO: add the actual grammar rules
    source_file: ($) => seq(repeat($._statement), optional($._expression)),
    _statement: ($) =>
      choice(
        $.import,
        $.impl,
        $.func,
        $.variable_declaration,
        $.declaration,
        $._statement_expression,
      ),
    declaration: ($) =>
      seq(
        repeat($.attribute),
        choice(
          $.struct_declaration,
          $.enum_declaration,
          $.component_declaration,
          $.trait_declaration,
        ),
      ),

    variable_declaration: ($) =>
      seq(
        choice("const", "let"),
        $.identifier,
        optional(seq(": ", $.type)),
        "=",
        $._expression,
        ";",
      ),

    _statement_expression: ($) =>
      choice(seq($._expression, ";"), prec(1, $._expression_block)),

    _expression_block: ($) =>
      choice($.if_else_expression, $.while_expression, $.for_in_expression),

    _expression: ($) =>
      choice(
        $.binary_expression,
        $.assignment_expression,
        $.is_expression,
        $.type_cast_expression,
        $.call_expression,
        $.literal,
        prec.left(2, $.identifier),
        $.self,
        $.scoped_identifier,
        $.index_expression,
        $.array_expression,
        $.array_index_expression,
        $.parenthesized_expression,
        $.node_expression,
        $._expression_block,
      ),

    array_expression: ($) =>
      seq(
        "[",
        seq(
          optional(seq($._expression, repeat(seq(",", $._expression)))),
          optional(","),
        ),
        "]",
      ),

    parenthesized_expression: ($) => seq("(", $._expression, ")"),

    scoped_identifier: ($) =>
      seq(
        choice($.path, alias($.generic_type_with_turbofish, $.generic_type)),
        "::",
        $.identifier,
      ),
    scoped_type_identifier: ($) =>
      seq(
        choice(
          $.path,
          alias($.generic_type_with_turbofish, $.generic_type),
          $.generic_type,
        ),
        "::",
        $.identifier,
      ),
    generic_type_with_turbofish: ($) => seq($.path, "::", $.type_args),
    generic_type: ($) => seq($.path, $.type_args),
    type_args: ($) =>
      seq(
        token(prec(1, "<")),
        seq($.type, repeat(seq(",", $.type))),
        optional(","),
        ">",
      ),

    path: ($) => choice($.identifier, $.scoped_identifier),
    // opt_type_path_expression: $ => seq($.identifier, repeat(seq("::", $.identifier))),

    scoped_type_identifier_in_expression_position: ($) =>
      prec(
        -2,
        seq(
          field(
            "path",
            choice(
              $.path,
              alias($.generic_type_with_turbofish, $.generic_type),
            ),
          ),
          "::",
          field("name", $.identifier),
        ),
      ),

    node_expression: ($) =>
      seq(
        field(
          "name",
          choice(
            $.identifier,
            alias(
              $.scoped_type_identifier_in_expression_position,
              $.scoped_type_identifier,
            ),
          ),
        ),
        "{",
        optional(
          choice(
            seq($.node_field, repeat(seq(",", $.node_field)), optional(",")),
            seq(
              $.node_field,
              repeat(seq(",", $.node_field)),
              ",",
              repeat1($.node_expression),
            ),
            repeat($.node_expression),
          ),
        ),
        "}",
      ),

    node_field: ($) =>
      choice($.identifier, seq($.identifier, ":", $._expression)),

    self: (_) => "self",
    if_else_expression: ($) =>
      seq(
        "if",
        $._expression,
        $.block,
        optional(seq("else", choice($.block, $.if_else_expression))),
      ),
    while_expression: ($) => seq("while", $._expression, $.block),
    for_in_expression: ($) =>
      seq("for", $.identifier, "in", $._expression, $.block),
    index_expression: ($) => prec(9, seq($._expression, ".", $.identifier)),
    array_index_expression: ($) =>
      prec(10, seq($._expression, "[", $._expression, "]")),
    assignment_expression: ($) =>
      prec.left(0, seq($._expression, "=", $._expression)),
    is_expression: ($) =>
      prec.left(
        0,
        seq(
          $._expression,
          "is",
          seq(
            choice(
              $.identifier,
              alias(
                $.scoped_type_identifier_in_expression_position,
                $.scoped_type_identifier,
              ),
            ),
            choice(
              seq(
                "{",
                optional(seq($.identifier, repeat(seq(",", $.identifier)))),
                "}",
              ),
              $.identifier,
            ),
          ),
        ),
      ),
    call_expression: ($) =>
      prec(
        10,
        seq(
          $._expression,
          "(",
          optional(seq($._expression, repeat(seq(",", $._expression)))),
          ")",
        ),
      ),
    binary_expression: ($) =>
      choice(
        prec.left(
          7,
          seq(
            $._expression,
            field("operator", choice("*", "/")),
            $._expression,
          ),
        ),
        prec.left(
          6,
          seq(
            $._expression,
            field("operator", choice("+", "-")),
            $._expression,
          ),
        ),
        prec.left(5, seq($._expression, field("operator", "&"), $._expression)),
        prec.left(4, seq($._expression, field("operator", "|"), $._expression)),
        prec.left(
          3,
          seq(
            $._expression,
            field("operator", choice("==", "!=", ">", "<")),
            $._expression,
          ),
        ),
        prec.left(
          2,
          seq($._expression, field("operator", "&&"), $._expression),
        ),
        prec.left(
          1,
          seq($._expression, field("operator", "||"), $._expression),
        ),
      ),

    type_cast_expression: ($) =>
      prec.left(
        9,
        seq(field("value", $._expression), "as", field("type", $.type)),
      ),
    import: ($) =>
      seq(
        "import",
        choice(
          seq(
            "{",
            optional(seq($.identifier, repeat(seq(",", $.identifier)))),
            "}",
            "from",
            $.import_path,
          ),
          $.import_path,
        ),
        ";",
      ),

    import_path: ($) => seq($.identifier, repeat(seq("::", $.identifier))),

    impl: ($) =>
      seq(
        "impl",
        field(
          "generics",
          optional(
            seq(
              seq("<", seq($.identifier, repeat(seq(",", $.identifier))), ">"),
            ),
          ),
        ),
        optional(
          field(
            "trait",
            seq(
              choice($.identifier, $.scoped_type_identifier, $.generic_type),
              "for",
            ),
          ),
        ),
        $.type,
        "{",
        field("functions", repeat($.func)),
        "}",
      ),

    attribute: ($) =>
      seq(
        "@",
        "[",
        field("name", $.identifier),
        "=",
        field("value", $.literal),
        "]",
      ),
    trait_declaration: ($) =>
      seq(
        "trait",
        field("name", $.generic_name),
        "{",
        field("functions", repeat($.trait_func)),
        "}",
      ),

    trait_func: ($) => seq(repeat($.attribute), $.func_header, ";"),
    func_header: ($) =>
      seq(
        "fn",
        field("name", $.identifier),
        field("args", $.func_signature),
        optional(seq("->", field("returning_type", $.type))),
      ),
    func_signature: ($) =>
      seq("(", optional(seq($.func_arg, repeat(seq(",", $.func_arg)))), ")"),
    func_arg: ($) => choice($.self, seq($.identifier, ":", $.type)),

    func: ($) => seq(optional("postfix"), $.func_header, $.block),

    block: ($) => seq("{", repeat($._statement), optional($._expression), "}"),

    struct_declaration: ($) =>
      seq(
        "struct",
        field("name", $.generic_name),
        "{",
        field("properties", optional(seq($.field, repeat(seq(",", $.field))))),
        "}",
      ),

    component_declaration: ($) =>
      seq(
        "declare",
        field("name", $.generic_name),
        "{",
        field("properties", optional(seq($.field, repeat(seq(",", $.field))))),
        "}",
      ),

    enum_declaration: ($) =>
      seq(
        "enum",
        field("name", $.generic_name),
        "{",
        field(
          "variants",
          optional(
            seq($.enum_variant, repeat(seq(",", optional($.enum_variant)))),
          ),
        ),
        "}",
      ),

    enum_variant: ($) =>
      seq(
        repeat($.attribute),
        field("name", $.identifier),
        optional(
          seq(
            "{",
            field(
              "properties",
              optional(seq($.field, repeat(seq(",", optional($.field))))),
            ),
            "}",
          ),
        ),
      ),

    generic_name: ($) =>
      seq(
        field("name", $.identifier),
        optional(
          seq("<", seq($.generic_name, repeat(seq(",", $.generic_name))), ">"),
        ),
      ),
    field: ($) =>
      seq(
        field("name", $.identifier),
        ":",
        field("type", $.type),
        optional(field("default_value", seq("=", $._expression))),
      ),
    type: ($) =>
      seq(
        choice(
          field("primitive_type", $.primitive_type),
          $.generic_type,
          $.scoped_type_identifier,
          field("ident", $.identifier),
        ),
        optional("[]"),
      ),
    primitive_type: ($) =>
      choice(
        "boolean",
        "float",
        "int8",
        "int16",
        "int32",
        "int64",
        "int_size",
        "uint8",
        "uint16",
        "uint32",
        "uint64",
        "uint_size",
        "string",
        "component",
        "void",
      ),

    literal: ($) => choice($.string, $.number, $.boolean),
    boolean: (_) => choice("true", "false"),
    string: ($) =>
      seq(
        '"',
        repeat(
          choice(
            alias($.unescaped_double_string_fragment, $.string_fragment),
            $.escape_sequence,
          ),
        ),
        '"',
      ),
    unescaped_double_string_fragment: (_) =>
      token.immediate(prec(1, /[^"\\\r\n]+/)),
    escape_sequence: (_) =>
      token.immediate(
        seq(
          "\\",
          choice(
            /[^xu0-7]/,
            /[0-7]{1,3}/,
            /x[0-9a-fA-F]{2}/,
            /u[0-9a-fA-F]{4}/,
            /u\{[0-9a-fA-F]+\}/,
            /[\r?][\n\u2028\u2029]/,
          ),
        ),
      ),
    identifier: ($) => /[A-Za-z_]([0-9A-Za-z_]+)?/,
    number: ($) => /\d+(\.\d+)?([A-Za-z0-9]+)?/,

    line_comment: (_) =>
      seq(
        "//",
        choice(
          seq(token.immediate(prec(2, /\/\//)), /.*/),
          token.immediate(prec(1, /.*/)),
        ),
      ),
  },
});
