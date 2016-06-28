# Poorlog

A C interpreter for a small prolog-like language

## Usage

```
$ make
$ ./poorlog -e 'between(1, 5, X), print(X), fail; nl'
12345
```

## Tests

```
$ make test
```