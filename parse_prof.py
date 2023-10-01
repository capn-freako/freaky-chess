#! /usr/bin/env python3

# Filter freaky-chess profiling output.
#
# Original author: David Banas <capn.freako@gmail.com>
# Original data:   April 17, 2023
#
# Copyright (c) 2023 David Banas; all rights reserved World wide.

"""Filter the `freaky-chess-exe.prof` file for high call counts of `occupiedBy`.

Include the call stack.
"""
# Call count is 5th field. Max is > 200M.
# Header: lines 34,35.
def indent(line):
    """Return number of spaces at beginning of string.
    """
    return( len(line) - len(line.lstrip()) )

with open("freaky-chess-exe.prof", 'rt') as file:
    lines = file.readlines()
    for (ix, line) in enumerate(lines):
        if ix < 40:
            next
        elif ix < 42:
            print(line.rstrip())
        else:
            toks = line.split()
            if toks and "makeMove" in toks[0] and int(toks[4]) > 10_000_000:
                _indent = indent(line)
                _ix = ix - 1
                _lines_to_print = []
                while(True):
                    _this_line = lines[_ix]
                    _this_indent = indent(_this_line)
                    if(_this_indent < _indent):
                        _lines_to_print.insert(0, _this_line)
                        if(_this_indent < 9):
                            break
                        _indent = _this_indent
                    _ix -= 1
                for _line in _lines_to_print:
                    print(_line.rstrip())
                print(line.rstrip())
