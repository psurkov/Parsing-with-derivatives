from abc import ABC, abstractmethod
import functools


class Regexp(ABC):
    @property
    @abstractmethod
    def type(self):
        pass


class Empty(Regexp):
    type = 'Empty'


class Epsilon(Regexp):
    type = 'Epsilon'


class Char(Regexp):
    type = 'Char'

    def __init__(self, char):
        self.char = char


class Seq(Regexp):
    type = 'Seq'

    def __init__(self, p, q):
        self.p = p
        self.q = q


class Alt(Regexp):
    type = 'Alt'

    def __init__(self, p, q):
        self.p = p
        self.q = q


class Star(Regexp):
    type = 'Star'

    def __init__(self, r):
        self.r = r


def nullable(r) -> bool:
    if r.type in ['Empty', 'Char']:
        return False
    if r.type in ['Epsilon', 'Star']:
        return True
    if r.type == 'Alt':
        return nullable(r.p) or nullable(r.q)
    if r.type == 'Seq':
        return nullable(r.p) and nullable(r.q)
    raise NotImplementedError


def derivative(r, c):
    if r.type in ['Empty', 'Epsilon']:
        return Empty()
    if r.type == 'Char':
        return Epsilon() if r.char == c else Empty()
    if r.type == 'Alt':
        return Alt(derivative(r.p, c), derivative(r.q, c))
    if r.type == 'Seq':
        if nullable(r.p):
            return Alt(Seq(derivative(r.p, c), r.q), derivative(r.q, c))
        else:
            return Seq(derivative(r.p, c), r.q)
    if r.type == 'Star':
        return Seq(derivative(r.r, c), r)
    raise NotImplementedError


def match(r: Regexp, s: str) -> bool:
    return nullable(functools.reduce(derivative, s, r))


def parse_regexp(s: str) -> Regexp:
    type_and_args = s.split(maxsplit=1)
    type_and_args += (2 - len(type_and_args)) * [None]

    type_str, args_str = type_and_args

    args = []

    bal = 0
    cur_str = ""
    if args_str:
        for c in args_str:
            if c == ' ' and len(cur_str) == 0:
                continue
            if c == '(':
                if bal > 0:
                    cur_str += c
                bal += 1
            elif c == ')':
                bal -= 1
                if bal > 0:
                    cur_str += c
            else:
                cur_str += c
            if bal == 0:
                args.append(cur_str)
                cur_str = ""
    if type_str == 'Empty':
        if len(args) != 0:
            raise RuntimeError
        return Empty()
    if type_str == 'Epsilon':
        if len(args) != 0:
            raise RuntimeError
        return Epsilon()
    if type_str == 'Char':
        if len(args) != 1 or len(args[0]) != 1:
            raise RuntimeError
        return Char(args[0][0])
    if type_str == 'Alt':
        if len(args) != 2:
            raise RuntimeError
        return Alt(parse_regexp(args[0]), parse_regexp(args[1]))
    if type_str == 'Seq':
        if len(args) != 2:
            raise RuntimeError
        return Seq(parse_regexp(args[0]), parse_regexp(args[1]))
    if type_str == 'Star':
        if len(args) != 1:
            raise RuntimeError
        return Star(parse_regexp(args[0]))
    raise NotImplementedError


def main():
    print(match(parse_regexp(input('Regexp: ')), input("String: ")))


if __name__ == "__main__":
    main()

'''
Regexp: Star (Char (a))
String: aaaaaaaaaaaaaa
True
'''
