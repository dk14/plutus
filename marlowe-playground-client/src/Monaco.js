/*eslint-env node*/
'use strict';

exports.isWarning_ = function (severity) {
  return severity == 4;
}

exports.isError_ = function (severity) {
  return severity == 8;
}

exports.getMonaco = function () {
  return global.monaco;
}

exports.registerLanguage_ = function (monaco, language) {
  monaco.languages.register(language);
}

exports.defineTheme_ = function (monaco, theme) {
  monaco.editor.defineTheme(theme.name, theme.themeData);
}

exports.setMonarchTokensProvider_ = function (monaco, languageId, languageDef) {
  monaco.languages.setMonarchTokensProvider(languageId, languageDef);
}

exports.setModelMarkers_ = function (monaco, model, owner, getMarkers) {
  let value = model.getValue();
  const markers = getMarkers(value);
  monaco.editor.setModelMarkers(model, owner, markers);
}

exports.getModelMarkers_ = function (monaco, model) {
  return monaco.editor.getModelMarkers({ resource: model.uri });
}

exports.create_ = function (monaco, nodeId, languageId, themeName) {
  const editor = monaco.editor.create(nodeId, {
    value: [
      'Close'
    ].join('\n'),
    language: languageId,
    theme: themeName,
  });
  return editor;
}

exports.onDidChangeContent_ = function (editor, handler) {
  editor.getModel().onDidChangeContent(function (event) {
    handler(event)();
  });
}

exports.getModel_ = function (editor) {
  return editor.getModel();
}

exports.getValue_ = function (model) {
  return model.getValue();
}

exports.setValue_ = function (model, value) {
  return model.setValue(value);
}

exports.setTokensProvider_ = function (monaco, languageId, provider) {
  monaco.languages.setTokensProvider(languageId, provider);
}

exports.completionItemKind_ = function (name) {
  return monaco.languages.CompletionItemKind[name];
}

exports.markerSeverity_ = function (name) {
  return monaco.MarkerSeverity[name];
}

exports.registerCompletionItemProvider_ = function (monaco, languageId, provider) {
  monaco.languages.registerCompletionItemProvider(languageId, provider);
}

exports.registerCodeActionProvider_ = function (monaco, languageId, actionProvider) {
  monaco.languages.registerCodeActionProvider(languageId, actionProvider);
}

exports.registerDocumentFormattingEditProvider_ = function (monaco, languageId, formatter) {
  monaco.languages.registerDocumentFormattingEditProvider(languageId, formatter);
}

exports.setPosition_ = function (editor, position) {
  editor.setPosition(position);
}

exports.revealLine_ = function (editor, lineNumber) {
  editor.revealLine(lineNumber);
}

exports.layout_ = function (editor) {
  editor.layout();
}

// you can play with this at https://microsoft.github.io/monaco-editor/monarch.html
// just paste it in and at the bottom `return haskell;`
const haskell = {
  // Set defaultToken to invalid to see what you do not tokenize yet
  defaultToken: 'invalid',

  tokenizer: {

    root: [
      ['(`)([a-zA-Z_\']*?)(`)',
        ['punctuation.definition.entity.haskell',
          'keyword.operator.function.infix.haskell',
          'punctuation.definition.entity.haskell']],
      ['\\(\\)', 'constant.language.unit.haskell'],
      ['\\[\\]', 'constant.language.empty-list.haskell'],
      ['\\b(module|signature)\\b', 'keyword.other.haskell', '@module_def'],
      ['\\bclass\\b', 'keyword.other.haskell', 'class_'],
      ['\\binstance\\b', 'keyword.other.haskell', '@instance'],
      ['import', 'keyword.other.haskell', '@import_'],
      ['(deriving|deriving anyclass|deriving stock|deriving newtype)(\\s*\\()', [
        { token: 'keyword.other.haskell' },
        { token: 'meta.deriving.haskell', next: '@deriving' }],
      ],
      ['\\binfix[lr]?\\b', 'keyword.operator.haskell'],
      ['\\b(?:do|if|then|else)\\b', 'keyword.control.haskell'],
      ['\\b(?:[0-9]+\\.[0-9]+(?:[eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+)\\b',
        'constant.numeric.float.haskell'],
      ['\\b(?:[0-9]+|0(?:[xX][0-9a-fA-F]+|[oO][0-7]+))\\b',
        'constant.numeric.haskell'],
      ['^(\\s*)(#)(\\s*\\w+)',
        ['meta.preprocessor.c',
          'punctuation.definition.preprocessor.c',
          'meta.preprocessor.c']],
      ['\\{-#', 'meta.preprocessor.haskell', '@pragma'],
      ['"', 'punctuation.definition.string.begin.haskell', '@stringx'],
      ['(\')(?:([\\ -\\[\\]-~])|(\\\\(?:NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL|[abfnrtv\\\\\\"\'\\&]))|(\\\\o[0-7]+)|(\\\\x[0-9A-Fa-f]+)|(\\^[A-Z@\\[\\]\\\\\\^_]))(\')',
        ['punctuation.definition.string.begin.haskell',
          'string.quoted.single.haskell',
          'constant.character.escape.haskell',
          'constant.character.escape.octal.haskell',
          'constant.character.escape.hexadecimal.haskell',
          'constant.character.escape.control.haskell',
          'punctuation.definition.string.end.haskell']],
      ['^(\\s*)([a-z_][a-zA-Z0-9_\']*|\\([|!%$+\\-.,=</>]+\\))(\\s*)(::)',
        [{ token: 'meta.function.type-declaration.haskell' },
        { token: 'entity.name.function.haskell' },
        { token: 'meta.function.type-declaration.haskell' },
        { token: 'keyword.other.double-colon.haskell', next: '@function_' }
        ],
      ],
      ['\\b(?:Just|Nothing|Left|Right|True|False|LT|EQ|GT|\\(\\)|\\[\\])\\b',
        'support.constant.haskell'],
      ['\\b[A-Z]\\w*\\b', 'constructor'],
      [/[a-z][a-zA-Z0-9_\']*/, 'name'],
      { include: '@comment' },
      ['\\b(?:abs|acos|acosh|all|and|any|appendFile|applyM|asTypeOf|asin|asinh|atan|atan2|atanh|break|catch|ceiling|compare|concat|concatMap|const|cos|cosh|curry|cycle|decodeFloat|div|divMod|drop|dropWhile|elem|encodeFloat|enumFrom|enumFromThen|enumFromThenTo|enumFromTo|error|even|exp|exponent|fail|filter|flip|floatDigits|floatRadix|floatRange|floor|fmap|foldl|foldl1|foldr|foldr1|fromEnum|fromInteger|fromIntegral|fromRational|fst|gcd|getChar|getContents|getLine|head|id|init|interact|ioError|isDenormalized|isIEEE|isInfinite|isNaN|isNegativeZero|iterate|last|lcm|length|lex|lines|log|logBase|lookup|map|mapM|mapM_|max|maxBound|maximum|maybe|min|minBound|minimum|mod|negate|not|notElem|null|odd|or|otherwise|pi|pred|print|product|properFraction|putChar|putStr|putStrLn|quot|quotRem|read|readFile|readIO|readList|readLn|readParen|reads|readsPrec|realToFrac|recip|rem|repeat|replicate|return|reverse|round|scaleFloat|scanl|scanl1|scanr|scanr1|seq|sequence|sequence_|show|showChar|showList|showParen|showString|shows|showsPrec|significand|signum|sin|sinh|snd|span|splitAt|sqrt|subtract|succ|sum|tail|take|takeWhile|tan|tanh|toEnum|toInteger|toRational|truncate|uncurry|undefined|unlines|until|unwords|unzip|unzip3|userError|words|writeFile|zip|zip3|zipWith|zipWith3)\\b',
        'support.function.prelude.haskell'],
      { include: '@infix_op' },
      ['[|!%$?~+:\\-.=</>\\\\]+', 'keyword.operator.haskell'],
      [',', 'punctuation.separator.comma.haskell'],
      ['(?:deriving|where|data|type|case|of|let|in|newtype|default)',
        'keyword.other.haskell'],
    ],

    class_: [
      ['\\bwhere\\b', 'keyword.other.haskell', '@pop'],
      ['\\b(?:Monad|Functor|Eq|Ord|Read|Show|Num|(?:Frac|Ra)tional|Enum|Bounded|Real(?:Frac|Float)?|Integral|Floating)\\b',
        'support.class.prelude.haskell'],
      ['[A-Z][A-Za-z_\']*', 'entity.other.inherited-class.haskell'],
      ['\\b[a-z][a-zA-Z0-9_\']*\\b',
        'variable.other.generic-type.haskell'],
      // default token
      ['.', 'keyword']
    ],
    instance: [
      ['\\bwhere\\b|$', 'keyword.other.haskell', '@pop'],
      { include: '@type_signature' },
    ],
    import_: [
      // this regex is slightly broken
      [/^|;|$/, 'meta.import.haskell', '@pop'],
      ['qualified|as|hiding', 'keyword.other.haskell'],
      { include: '@module_name' },
      { include: '@module_exports' },
    ],
    deriving: [
      ['\\)', 'meta.deriving.haskell', '@pop'],
      ['\\b[A-Z][a-zA-Z_\']*',
        'entity.other.inherited-class.haskell'],
    ],
    block_comment: [
      ['\\{-(?!#)', 'comment', '@push'],
      ['-\\}', 'comment', '@pop'],
      // default token
      ['.', 'comment'],
    ],
    comment:
      [['--.*', 'comment'],
      ['\\{-(?!#)', 'comment', '@block_comment'],
      ],
    infix_op:
      [[/([|!%$+:\-.=</>]+)|(,+)/,
        'entity.name.function.infix.haskell']],
    module_def: [
      ['\\bwhere\\b', 'keyword.other.haskell', '@pop'],
      { include: '@module_name' },
      { include: '@module_exports' },
      ['[a-z]+', 'invalid'],
    ],
    module_exports:
      [['\\(', 'meta.declaration.exports.haskell', '@push'],
      ['\\)', 'meta.declaration.exports.haskell.end', '@pop'],
      ['\\b[a-z][a-zA-Z_\']*', 'entity.name.function.haskell'],
      ['\\b[A-Z][A-Za-z_\']*', 'storage.type.haskell'],
      [',', 'punctuation.separator.comma.haskell'],
      { include: '@infix_op' },
      ['\\(.*?\\)', 'meta.other.unknown.haskell'],
      ],
    module_name:
      [['[A-Z][A-Za-z._\']*', 'support.other.module.haskell']],
    function_: [
      ['^', 'meta.function.type-declaration.haskell', '@pop'],
      { include: '@type_signature' },
    ],
    stringx:
      [['"', 'punctuation.definition.string.end.haskell', '@pop'],
      ['\\\\(?:NUL|SOH|STX|ETX|EOT|ENQ|ACK|BEL|BS|HT|LF|VT|FF|CR|SO|SI|DLE|DC1|DC2|DC3|DC4|NAK|SYN|ETB|CAN|EM|SUB|ESC|FS|GS|RS|US|SP|DEL|[abfnrtv\\\\\\"\'\\&])',
        'constant.character.escape.haskell'],
      ['\\\\o[0-7]+|\\\\x[0-9A-Fa-f]+|\\\\[0-9]+',
        'constant.character.escape.octal.haskell'],
      ['\\^[A-Z@\\[\\]\\\\\\^_]',
        'constant.character.escape.control.haskell'],
      // default token
      ['.', 'string.quoted.double.haskell']
      ],
    pragma: [
      ['#-\\}', 'meta.preprocessor.haskell', '@pop'],
      ['\\b(?:LANGUAGE|UNPACK|INLINE)\\b', 'keyword.other.preprocessor.haskell'],
      // default token
      ['.', 'keyword.other.preprocessor.haskell']
    ],
    type_signature: [
      ['(\\(\\s*)([A-Z][A-Za-z]*)(\\s+)([a-z][A-Za-z_\']*)(\\)\\s*)(=>)',
        ['meta.class-constraint.haskell',
          'entity.other.inherited-class.haskell',
          'meta.class-constraint.haskell',
          'variable.other.generic-type.haskell',
          'meta.class-constraint.haskell',
          'keyword.other.big-arrow.haskell']],
      ['\\{-#', 'meta.preprocessor.haskell', '@pragma'],
      ['->', 'keyword.other.arrow.haskell'],
      ['=>', 'keyword.other.big-arrow.haskell'],
      ['\\b(?:Int(?:eger)?|Maybe|Either|Bool|Float|Double|Char|String|Ordering|ShowS|ReadS|FilePath|IO(?:Error)?) \\b',
        'support.type.prelude.haskell'],
      ['\\b[a-z][a-zA-Z0-9_\']*\\b',
        'variable.other.generic-type.haskell'],
      ['\\b[A-Z][a-zA-Z0-9_\']*\\b', 'storage.type.haskell'],
      ['\\(\\)', 'support.constant.unit.haskell'],
      { include: '@comment' }
    ]
  },
};
