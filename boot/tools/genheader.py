#!/usr/bin/python3

import sys

def main():
    default = find_export
    state = default
    path = sys.argv[1]
    print('// generated from ' + path + ' by ' + sys.argv[0])
    for n, line in enumerate(open(path)):
        try:
            ret = state(line.rstrip("\r\n"))
            try:
                default = ret[1]
                ret = ret[0]
            except:
                pass
            state = ret or default
        except Exception as e:
            sys.exit(path + ':' + str(n) + ': ' + str(e) + ': ' + line)
    if state != find_export:
        sys.exit(path + ': premature end of file')

def find_export(line):
    if line == 'EXPORT':
        return export_declaration
    if line == 'EXPORT_PUBLIC':
        return export_definition

def export_declaration(line):
    if line.startswith('typedef struct'):
        return curry(find_type_name, line.split('{')[0])
    if line.startswith('#define'):
        return export_macro(line)
    else:
        print(line.split('{')[0] + ';')
        return find_export

def export_definition(line):
    print(line)
    if line.startswith('}'):
        return find_export
    else:
        return export_definition

def export_macro(line):
    print(line)
    if line[-1] == '\\': 
        return export_macro

def find_type_name(partial, line):
    if line.startswith('} '):
        print(partial + line[1:])
        return find_export
    else:
        return find_type_name

def curry(f, *a):
    return lambda *b: f(*(a+b))

if __name__ == '__main__':
    main()
