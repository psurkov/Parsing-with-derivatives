import ply.lex as lex

tokens = [
    'ID',
    'DECLARE',
    'DOT',
    'COMMA',
    'SEMICOLON',
    'LBRACKET',
    'RBRACKET',
]


def t_ID(t):
    r'[a-z_A-Z][a-z_0-9A-Z]*'
    return t


t_DECLARE = r':-'
t_DOT = r'\.'
t_COMMA = r','
t_SEMICOLON = r';'
t_LBRACKET = r'\('
t_RBRACKET = r'\)'

t_ignore = ' \t'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    # t.lexer.lexpos = 0


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


def lex_text(text):
    lexer = lex.lex()
    lexer.input(text)
    lexemes = []
    while True:
        tok = lexer.token()
        if not tok:
            break
        lexemes.append(tok)
    return lexemes

