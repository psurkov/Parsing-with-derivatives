import time
from main import *


def compare_time(sr1, r1, sr2, r2, s):
    start_time = time.time()
    match(r1, s)
    print(sr1, time.time() - start_time)
    start_time = time.time()
    match(r2, s)
    print(sr2, time.time() - start_time)


def main():
    compare_time('aa*', smart_seq(Char('a'), smart_star(Char('a'))),
                 '(a*)a(a*)', smart_seq(smart_seq(smart_star(Char('a')), Char('a')), smart_star(Char('a'))),
                 'a' * 500)
    print('')
    compare_time('a*', smart_star(Char('a')),
                 '(a|a*)*', smart_star(smart_alt(Char('a'), smart_star(Char('a')))),
                 'a' * 18)


if __name__ == "__main__":
    main()
