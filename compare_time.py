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
    compare_time('a*', Star(Char('a')),
                 '(a|a*)*', Star(Alt(Char('a'), Star(Char('a')))),
                 'a' * 16)
    print('')
    compare_time('aa*', Seq(Char('a'), Star(Char('a'))),
                 '(a*)a(a*)', Seq(Seq(Star(Char('a')), Char('a')), Star(Char('a'))),
                 'a' * 100)


if __name__ == "__main__":
    main()
