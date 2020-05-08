const REP = [];

for (sep of ['', ',', ';']) {
    for (rep of ['?', '*', '+']) {
        REP.push(sep + rep)
    }
}

module.exports = grammar({
    name: 'sbnf',

    word: $ => $.identifier,

    rules: {
        source_file: $ => seq(
            repeat(';'),
            repeat($._rule),
        ),

        _rule: $ => choice(
            $.rule,
        ),

        rule: $ => seq(
            field('name', $.identifier),
            '=',
            optional('|'),
            field('body', $._pattern),
            repeat1(';'),
        ),

        _pattern: $ => choice(
            $.alt_pattern,
            $.seq_pattern,
            $.rep_pattern,
            $.lit_pattern,
            $.group,
        ),

        alt_pattern: $ =>
            prec.left(2, seq($._pattern, '|', $._pattern)),

        seq_pattern: $ =>
            prec.left(3, seq($._pattern, $._pattern)),

        rep_pattern: $ =>
            prec.right(4, seq(
                $._pattern,
                choice(...REP),
            )),

        lit_pattern: $ => choice(
            $.identifier,
            $.single_quote,
            $.double_quote,
        ),

        group: $ => seq(
            '(',
            optional('|'),
            $._pattern,
            ')',
        ),

        identifier: $ => /[A-Za-z_][0-9A-Za-z_-]*/,

        single_quote: $ => /\'[^\r\n\']*\'/,

        double_quote: $ => /\"(\\.|[^\r\n\\\"])\"/,
    }
})
