import unittest
from main import *


class TestRegexp(unittest.TestCase):
    def test_nullable(self):
        self.assertEqual(nullable(Empty()), False)
        self.assertEqual(nullable(Epsilon()), True)
        self.assertEqual(nullable(Char('a')), False)
        self.assertEqual(nullable(Star(Char('a'))), True)
        self.assertEqual(nullable(Alt(Char('a'), Epsilon)), True)
        self.assertEqual(nullable(Alt(Alt(Alt(Epsilon, Empty), Empty), Empty)), True)
        self.assertEqual(nullable(Alt(Alt(Alt(Empty, Empty), Empty), Empty)), False)
        self.assertEqual(nullable(Seq(Seq(Seq(Epsilon, Epsilon), Epsilon), Epsilon)), True)
        self.assertEqual(nullable(Seq(Seq(Seq(Epsilon, Epsilon), Empty), Epsilon)), False)

    def test_some_derivative(self):
        self.assertIsInstance(derivative(Empty(), 'a'), Empty)
        self.assertIsInstance(derivative(Char('a'), 'b'), Empty)
        self.assertIsInstance(derivative(Char('a'), 'a'), Epsilon)

    def test_match(self):
        self.assertEqual(match(Empty(), 'a'), False)
        self.assertEqual(match(Char('a'), 'a'), True)
        self.assertEqual(match(Alt(Char('a'), Char('b')), 'b'), True)
        self.assertEqual(match(Seq(Char('a'), Char('b')), 'ab'), True)
        self.assertEqual(match(Star(Char('a')), 'aaa'), True)
        self.assertEqual(match(Star(Char('a')), ''), True)
        self.assertEqual(match(Star(Char('a')), 'aaaaaaba'), False)

    def test_parse_and_match(self):
        self.assertEqual(match(parse_regexp('Epsilon'), ''), True)
        self.assertEqual(match(parse_regexp('Alt (Char (a)) (Char (b))'), 'b'), True)
        self.assertEqual(match(parse_regexp('Alt (Char (a)) (Char (b))'), 'c'), False)
        self.assertEqual(match(parse_regexp('Star (Char (a))'), 'a'), True)
        self.assertEqual(match(parse_regexp('Alt (Char (a)) (Star (Char (b)))'), 'a'), True)
        self.assertEqual(match(parse_regexp('Alt (Char (a)) (Star (Char (b)))'), 'aa'), False)
        self.assertEqual(match(parse_regexp('Alt (Char (a)) (Star (Char (b)))'), ''), True)
        self.assertEqual(match(parse_regexp('Alt (Char (a)) (Star (Char (b)))'), 'bbbb'), True)
        self.assertEqual(match(parse_regexp('Alt (Char (a)) (Star (Char (b)))'), 'abbbb'), False)
        self.assertEqual(match(parse_regexp('Seq (Char (a)) (Char (b))'), 'ab'), True)

    # Закомментировать следующую строчку, чтобы запустить тест на 2 секунды
    @unittest.skip
    def test_2_seconds(self):
        def gen_alt(i):
            if i == 0:
                return 'Char (a)'
            s = gen_alt(i - 1)
            return 'Alt (' + s + ') (' + s + ')'
        self.assertEqual(match(parse_regexp(gen_alt(15)), 'b'), False)


if __name__ == '__main__':
    unittest.main()
