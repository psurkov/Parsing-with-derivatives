from prolog_lex import lex_text


class Parser:
    def __init__(self, s):
        self.ls = lex_text(s)
        self.pos = 0

    def parse_simple_lex(self, type):
        if self.pos >= len(self.ls):
            return False
        if self.ls[self.pos].type == type:
            self.pos += 1
            return True
        return False

    def parse_id(self):
        return self.parse_simple_lex('ID')

    def parse_declare(self):
        return self.parse_simple_lex('DECLARE')

    def parse_dot(self):
        return self.parse_simple_lex('DOT')

    def parse_semicolon(self):
        return self.parse_simple_lex('SEMICOLON')

    def parse_comma(self):
        return self.parse_simple_lex('COMMA')

    def parse_lbracket(self):
        return self.parse_simple_lex('LBRACKET')

    def parse_rbracket(self):
        return self.parse_simple_lex('RBRACKET')

    def parse_id_or_brackets(self):
        if self.parse_id():
            return True
        if not(self.parse_lbracket()):
            return False
        if not(self.parse_body()):
            return False
        if not(self.parse_rbracket()):
            return False
        return True

    def parse_conj(self):
        if not(self.parse_id_or_brackets()):
            return False
        while self.parse_comma():
            if not (self.parse_id_or_brackets()):
                return False
        return True

    def parse_body(self):
        if not(self.parse_conj()):
            return False
        while self.parse_semicolon():
            if not(self.parse_conj()):
                return False
        return True

    def parse(self):
        while self.pos < len(self.ls):
            if not(self.parse_id()):
                return False
            if self.parse_declare():
                if not(self.parse_body()):
                    return False
            if not(self.parse_dot()):
                return False

        return True

    def get_error_pos(self):
        pos = min(self.pos, len(self.ls) - 1)
        return 'Syntax error: line ' + str(self.ls[pos].lineno) + ' colon ' + str(self.ls[pos].lexpos)
