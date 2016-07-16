#!/usr/bin/python3

import sys, time

last_line = 1;
position = None;

def main():
    default = find_export
    state = default
    path = sys.argv[1]
    guard = 'INCLUDED_' + path.replace('.','_').replace('/','_')
    print(
        '// generated from', path,
        'by', sys.argv[0],
        'at', time.strftime('%Y-%m-%d %H:%M:%S'))
    print('#line 1 "' + path + '"')
    print('#ifndef', guard)
    print('#define', guard)
    for n, line in enumerate(open(path)):
        # print(n, state.__qualname__, line, file=sys.stderr, end='')
        global position
        position = n + 1
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
    print('#endif')

def write(*args):
    global last_line
    if position != last_line + 1:
        print('#line', position)
    last_line = position
    print(*args)

def find_export(line):
    if line == 'HEADER_DECLARE':
        return export_declaration
    if line == '#if HEADER_ONLY':
        return header_only
    if line.startswith('HEADER('):
        assert line[-1] == ')'
        write(line[7:-1])

def export_declaration(line):
    if line.startswith('typedef struct'):
        return curry(find_type_name, line.split('{')[0])
    elif line.startswith('typedef'):
        write(line)
    elif line.startswith('#define'):
        return export_macro(line)
    elif line[-1] == '{':
        write(line.split('{')[0] + ';')
    elif line[-1] == ';':
        write('extern', line[:-1].split('=')[0] + ';')

def export_macro(line):
    write(line)
    if line[-1] == '\\':
        return export_macro

def find_type_name(partial, line):
    if line.startswith('} '):
        write(partial + line[1:])
        return find_export
    else:
        return curry(find_type_name, partial)

def header_only(line):
    if start_if(line):
        write(line)
        return header_if_block
    if line != '#endif':
        write(line)
        return header_only

def start_if(line):
    return line.startswith('#if') or line.startswith('#ifdef')

def header_if_block(line):
    write(line)
    if line == '#endif':
        return header_only
    else:
        return header_if_block

def curry(f, *a):
    return lambda *b: f(*(a+b))

if __name__ == '__main__':
    main()
