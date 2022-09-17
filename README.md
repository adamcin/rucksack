<!--
 
-->

# rucksack: a Hack & Jack Compiler Suite written in Rust

I developed this project as I followed along with the [nand2tetris curriculum](https://www.nand2tetris.org/).

# Usage

### JackCompiler

Compile a single `.jack` file or a directory of `.jack` files into `.vm` files.

```
rucksack JackCompiler <jack_file | jack_dir> ...
```

### JackAnalyzer

Analyze the token streams of a single `_base_.jack` file or a directory of `_base_.jack` files saved in parallel files, `_base_T.xml` (just terminals) and `_base_.xml` (all jack grammar).

```
rucksack JackAnalyzer <jack_file | jack_dir> ...
```

### VMTranslator

Translate a single `.vm` file or a directory of `.vm` files into a single `.asm` file.

```
rucksack VMTranslator <vm_file | vm_dir> ...
```

### HackAssembler

Assemble a `.asm` file to a `.hack` file.

```
rucksack HackAssembler <asm_file> ...
```

