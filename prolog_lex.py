import ply.lex as lex

tokens = [
    'ID',
    'DECLARE',
    'DOT',
    'COMMA',
    'SEMICOLON',
    'LBR',
    'RBR',
]


def t_ID(t):
    r'[a-z_A-Z][a-z_0-9A-Z]*'
    return t


t_DECLARE = r':-'
t_DOT = r'\.'
t_COMMA = r','
t_SEMICOLON = r';'
t_LBR = r'\('
t_RBR = r'\)'

t_ignore = ' \t'


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    raise RuntimeError


lexer = lex.lex()

