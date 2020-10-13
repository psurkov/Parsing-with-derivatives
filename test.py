import unittest
from prolog_parser import parse


class TestParser(unittest.TestCase):
    def test_simple(self):
        self.assertEqual('program (expression (atom (ID f)) DOT)', parse('f.'), )
        self.assertEqual('program (expression (atom (ID f)) DECLARE (atom (ID g)) DOT)', parse('f :- g.'))

    def test_atom(self):
        self.assertEqual('program (expression (atom (ID a) (atom (ID b))) DOT)', parse('a (b).'))
        self.assertEqual('program (expression (atom (ID a) (atom (ID b))) DOT)', parse('a (((b))).'))
        self.assertEqual('program (expression (atom (ID a) (atom (ID b) (atom (ID c) (atom (ID d))))) DOT)', parse('a (b c d).'))
        self.assertEqual('program (expression (atom (ID a) (atom (ID b) (atom (ID c))) (atom (ID d))) DOT)', parse('a (b c) d.'))

    def test_conj(self):
        self.assertEqual('program (expression (atom (ID f)) DECLARE (CONJUNCTION (atom (ID f)) (atom (ID g))) DOT)',
                         parse('f :- f, g.'))
        self.assertEqual('program (expression (atom (ID f)) DECLARE (CONJUNCTION (atom (ID f)) (CONJUNCTION (atom (ID g)) (atom (ID h)))) DOT)',
                         parse('f :- f, g, h.'))

    def test_disj(self):
        self.assertEqual('program (expression (atom (ID f)) DECLARE (DISJUNCTION (atom (ID f)) (atom (ID g))) DOT)',
                         parse('f :- f ; g.'))
        self.assertEqual(
            'program (expression (atom (ID f)) DECLARE (DISJUNCTION (atom (ID f)) (DISJUNCTION (atom (ID g)) (atom (ID h)))) DOT)',
            parse('f :- f ; g ; h.'))

    def test_prior(self):
        self.assertEqual('program (expression (atom (ID f)) DECLARE (DISJUNCTION (CONJUNCTION (atom (ID g)) (atom (ID h))) (atom (ID t))) DOT)',
                         parse('f :- g, h; t.'))

    def test_multiple_lines(self):
        self.assertEqual('program (expression (atom (ID f)) DOT) (program (expression (atom (ID g)) DOT))', parse('f.\ng.'))

    def test_error_msg(self):
        self.assertRaises(RuntimeError, lambda: parse('f'))
        self.assertRaises(RuntimeError, lambda: parse('.'))
        self.assertRaises(RuntimeError, lambda: parse('f ().'))
        self.assertRaises(RuntimeError, lambda: parse('f, g :- f.'))
        self.assertRaises(RuntimeError, lambda: parse('f :- f;'))
        self.assertRaises(RuntimeError, lambda: parse(':- f'))
        self.assertRaises(RuntimeError, lambda: parse('(a).'))


if __name__ == '__main__':
    unittest.main()