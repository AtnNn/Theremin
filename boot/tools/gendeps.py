#!/usr/bin/python3

import sys
from os.path import basename, dirname, relpath, join, exists, splitext

sources_done = []
headers_include = {}
target = None
out_path = None
out = None
objdir = None
srcdir = None
incdir = None

def main():
    global out_path, out, target, incdir
    args = sys.argv[1:]
    source = None
    ofile = None
    while args:
        if args[0] == '-deps-out':
            out_path = args[1]
            args = args[2:]
        if args[0] == '-deps-target':
            target = args[1]
            args = args[2:]
        elif args[0] == '-o':
            ofile = args[1]
            args = args[2:]
        elif args[0] == '-I':
            incdir = args[1]
            args = args[2:]
        elif args[0][0] == '-':
            args = args[1:]
        else:
            source = args[0]
            args = args[1:]
    savedirs(source, ofile)
    out = open(out_path, "w")
    process_source(source)
    out.close()

def depends(target, dep):
    out.write(target + ': ' + dep + '\n')

def savedirs(source, ofile):
    global srcdir, objdir
    c = dirname(source)
    o = dirname(ofile)
    while basename(c) == basename(o):
        c = dirname(c)
        o = dirname(o)
    srcdir = c
    objdir = o

def process_source(source):
    if source in sources_done:
        return
    sources_done.append(source)
    depends(out_path, source)
    obj = join(objdir, relpath(splitext(source)[0], srcdir)) + '.o'
    depends(target, obj)
    headers = {header for include in list_includes(source) for header in process_include(include)}
    for header in headers:
        depends(obj, header)

def process_include(include):
    header = join(srcdir, include)
    if exists(header):
        return process_header(header)
    header = join(incdir, include)
    process_source(join(srcdir, splitext(include)[0] + '.c'))
    return [header]

def process_header(header):
    global headers_include
    try:
        return headers_include[header]
    except:
        depends(out_path, header)
        ret = list_includes(header)
        headers_include[header] = ret
        return ret

def list_includes(source):
    ret = []
    for line in open(source):
        if line.startswith('#include "'):
            ret.append(line.split('"')[1])
    return ret

if __name__ == '__main__':
    main()
