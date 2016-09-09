#!/usr/bin/env python3

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
            if not ofile:
                ofile = args[1]
            args = args[2:]
        elif args[0] == '-iquote':
            if not incdir:
                incdir = args[1]
            args = args[2:]
        elif args[0][0] == '-':
            args = args[1:]
        else:
            if not source:
                source = args[0]
            args = args[1:]
    savedirs(source, ofile)
    out = open(out_path, "w")
    process_source(source)
    end()

def depends(target, dep):
    out.write(target + ': ' + dep + '\n')

def pretend_rule(file):
    out.write(file + ':\n')

def end():
    out.close()
    exit(0)

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
    pretend_rule(source)
    obj = join(objdir, relpath(splitext(source)[0], srcdir)) + '.o'
    depends(target, obj)
    for include in list_includes(source):
        for header in process_include(include):
            depends(obj, header)

def process_include(include):
    header = join(srcdir, include)
    if exists(header):
        yield header
        yield from process_header(header)
        return
    header = join(incdir, include)
    if exists(header):
        source = join(srcdir, relpath(splitext(header)[0], incdir)) + '.c'
        process_source(source)
        yield header
        yield from process_header(header)
        return
    depends(out_path, header)
    end()

def process_header(header):
    global headers_include
    try:
        return headers_include[header]
    except:
        pass
    pretend_rule(header)
    depends(out_path, header)
    includes = list_includes(header)
    headers = [list(process_include(include)) for include in includes]
    ret = sum(headers, [])
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
