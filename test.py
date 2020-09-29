import unittest
from prolog_parser import Parser


class TestParser(unittest.TestCase):
    def test_simple(self):
        self.assertTrue(Parser('x.').parse())
        self.assertTrue(Parser('xyz.').parse())
        self.assertTrue(Parser('xy :- zy.').parse())
        self.assertFalse(Parser(':- x.').parse())
        self.assertFalse(Parser('.').parse())
        self.assertFalse(Parser('x :- .').parse())

    def test_operators_without_brackets(self):
        self.assertTrue(Parser('x :- x, y.').parse())
        self.assertTrue(Parser('x :- x, y, z, t.').parse())
        self.assertTrue(Parser('x :- x; y.').parse())
        self.assertTrue(Parser('x :- x; y; z; t.').parse())
        self.assertTrue(Parser('x :- x; y, z; t, a; b, c.').parse())
        self.assertFalse(Parser('x :- x,.').parse())
        self.assertFalse(Parser('x :- x;.').parse())
        self.assertFalse(Parser('x :- x, y, z,.').parse())
        self.assertFalse(Parser('x :- x; y; z;.').parse())
        self.assertFalse(Parser('x :- x; y; a, b, z; z, .').parse())
        self.assertFalse(Parser('x :- x;, y .').parse())
        self.assertFalse(Parser('x :- x,; y .').parse())
        self.assertFalse(Parser('x :- x,; y z .').parse())

    def test_brackets_balance(self):
        self.assertTrue(Parser('x :- (x), y.').parse())
        self.assertTrue(Parser('x :- ((((x))), (y)).').parse())
        self.assertTrue(Parser('x :- ((((x))); (y)).').parse())
        self.assertFalse(Parser('x :- (((x))), (y)).').parse())
        self.assertFalse(Parser('x :- ((((x))), (y).').parse())
        self.assertFalse(Parser('x :- ((((x)), (y)).').parse())
        self.assertFalse(Parser('x :- ((((x))), y)).').parse())

    def test_operators_and_brackets(self):
        self.assertTrue(Parser('x :- (x, y) ; z ; y; x.').parse())
        self.assertTrue(Parser('x :- (x, y) ; z ; y; x, (x, (y, z) ; (p, q)).').parse())
        self.assertFalse(Parser('x :- (x, y ; z ; y; x, (x, (y, z) ; (p, q)).').parse())

    def test_multiple_lines(self):
        self.assertTrue(Parser('x :- x.\ny :- (y,y).\na:-a.b:-b.').parse())
        self.assertFalse(Parser('x :- x.\ny :- (y,y).\na:-a.:-b.').parse())
        self.assertFalse(Parser('x :- x.\ny :- (y,y).\na:-a.b:-b').parse())

    def test_error_msg(self):
        p = Parser('x :- x.\ny :- (y,).\na:-a.:-b.')
        p.parse()
        self.assertEqual(p.get_error_pos(), 'Syntax error: line 2 colon 16')


if __name__ == '__main__':
    unittest.main()