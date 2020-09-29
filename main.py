import sys
from prolog_parser import Parser


def main():
    if len(sys.argv) == 1:
        print('No input file')
        return
    if len(sys.argv) > 2:
        print('Too much args')
        return
    with open(sys.argv[1]) as file:
        parser = Parser(file.read())
        if parser.parse():
            print('Ok')
        else:
            print(parser.get_error_pos())


if __name__ == '__main__':
    main()