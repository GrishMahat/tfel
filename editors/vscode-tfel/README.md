# TFEL Syntax Extension

Lightweight VS Code syntax highlighting plus a minimal language-server stub for
`.tfel` and `.logical.tfel` files.

## Status

This is fully vib-coded and not tested.

I added this because my friend told me to add it because he thinks it is funny.
He is trying to teach one of his friends programming and troll him with this language.

So this extension is minimal and vib-coded. I have not tested it.
I am too lazy to install VS Code myself.

Good luck. If it works, it works. If it does not, sorry about that.

## Local install (from this repo)

1. Install extension runtime deps:
   `cd editors/vscode-tfel && npm install`
2. Create the local extensions directory:
   `mkdir -p ~/.vscode/extensions`
3. Symlink this extension:
   `ln -s "$(pwd)/editors/vscode-tfel" ~/.vscode/extensions/tfel-syntax-local`
4. Reload VS Code window (`Developer: Reload Window`).

To remove it later:

`rm ~/.vscode/extensions/tfel-syntax-local`

## Included features

- TextMate syntax highlighting for TFEL keywords/builtins/operators
- Hover docs for core keywords and builtins
- Minimal parse diagnostics (heuristic):
  - unexpected/unclosed brackets
  - unterminated strings
  - possible missing semicolons
