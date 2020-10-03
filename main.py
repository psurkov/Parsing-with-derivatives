import sys
from prolog_parser import parse


def main():
    if len(sys.argv) == 1:
        print('No input file')
        return
    if len(sys.argv) > 2:
        print('Too much args')
        return
    with open(sys.argv[1]) as file:
        res = ''
        try:
            res = parse(file.read())
        except:
            res = 'Syntax error'
        with open(sys.argv[1].rsplit('.', 1)[0] + ".out", 'w') as outfile:
            outfile.write(res)


if __name__ == '__main__':
    main()