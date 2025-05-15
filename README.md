# TaP: Texting and Programming

TaP (Texting and Programming) is a programming language specifically designed for programming on any mobile device. TaP takes into account both the mobile keyboard layout and typing assistance like auto correct that many mobile devices have enabled by having a sentence-like structure, supporting periods to end statements as well as new lines. TaP also considers that switching keyboards using shift could affect productivity. As TaP aims to minimize the lines of code written, memory will be managed internally via garbage collection. TaP will also use a thread based concurrency model.

## Features

## Getting Started
```
ocamlbuild -pkgs llvm tap.native
```

```
./tap.native -l program.tap > program.out
```

```
lli program.out
```