## Adding a grammar

To add a new grammar, first add the information about that grammar to the list at the top of `R/grammars.R`. Then run:

```{r}
devtools::load_all()
grammar_init("{name}")
```

That will:
- Clone the grammar from GitHub into a temporary directory.
- Run `tree-sitter generate` in the cloned directory to regenerate `parser.c` and `tree_sitter/parser.h` with the local version of the `tree-sitter` cli. Importantly this should exactly match the version of tree-sitter that we ship in `src/tree-sitter`, but there are no checks for that currently. The goal is to generate grammar files with the exact same ABI as the tree-sitter bindings themselves.
- Generate `language-{name}.R` containing `language_{name}()`.
- Generate `src/grammars/{name}` containing:
    - The following tree-sitter files:
        - `tree_sitter/parser.h`
        - `parser.c`
        - `scanner.c` (if applicable)
    - `binding.c` containing `ffi_language_{name}()`

You then need to:
- Add `ffi_language_{name}()` to `init.c`
- Add the following to Makevars:
    - `src/grammars/{name}/binding.o`
    - `src/grammars/{name}/parser.o`
    - `src/grammars/{name}/scanner.o`
- Run `devtools::document()`

You should then be able to call `language_{name}()`.

## Updating a grammar

Much the same as adding a grammar. You can just update the information in the list in `R/grammars.R` and then run `grammar_init("{name}")` again, this time ignoring the messages except for the prompt to `devtools::document()`.

Make sure and run the tests and run `devtools::check()` to make sure there are no new patches required.
