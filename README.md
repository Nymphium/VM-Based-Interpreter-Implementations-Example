VM-Based interpreter implementations example
===

# source language
```bnf
e ::=   x | fun x -> e | e e
      | let x = e in e
      | i | e + e | e - e
```

```
$ jbuilder exec test/vm.exe
```
