# TFEL

`tfel` is a deliberately cursed programming language runtime.

This project is a joke, on purpose.
If you open TFEL code and your first reaction is "what is going on?", the design is working.

For the full design rant and spec, read `TFEL_LANGUAGE_SPEC.md`.

## What TFEL does to your brain

- Execution order is upside down.
- Brackets are mirrored.
- Assignment is reversed.
- Many keywords are reversed.
- It is internally consistent enough to be usable, which is the worst part.

## Quick start

```bash
cargo run -- examples/hello.tfel
```

## How code is processed

TFEL source files are intentionally stored in a mirrored form.

When you run a file, the runtime does this:

1. Reverse every line.
2. Reverse line order.
3. Tokenize with swapped brackets.
4. Parse and evaluate.

That means there are two views of TFEL:

- Logical TFEL syntax: what the parser effectively sees after preprocessing.
- On-disk TFEL syntax: the weird mirrored text in `.tfel` files.

## Practical writing workflow

Write code in logical TFEL first, then mirror it into a `.tfel` file:

```bash
cat program.logical.tfel | rev | tac > program.tfel
cargo run -- program.tfel
```

## Logical TFEL syntax guide

Everything below is shown in logical syntax (after preprocessing).

### Variables and assignment

```tfel
10 = x;
"hello" = msg;
```

`let` also exists for compatibility:

```tfel
let x = 10;
```

### Print

```tfel
print)"hello"(;
print)msg(;
```

### Conditions

```tfel
if )x > 0( } print)"positive"(; { else } print)"zero or negative"(; {
```

### Loops

```tfel
elihw )i < 5( } i + 1 = i; {
rof item ni range)0, 3( } print)item(; {
```

### Functions

```tfel
fed add)a, b( } nruter a + b; {
add)2, 3(;
```

### Imports

```tfel
"math" tropmi;
"answer" morf "math" tropmi;
lib/file tropmi;
```

Aliases also work:

```tfel
import "math";
from "math" import "answer";
```

Import-all exposes module namespace aliases too:

```tfel
lib/emit tropmi;
emit.lamron)(;
```

## Builtins

User-facing builtins:

- `input` / `tupni`
- `len` / `nel`
- `to_number` / `rebmun_ot`
- `range` / `egnar`
- `type_of` / `fo_epyt`
- `to_string` / `gnirts_ot`

Internal primitives used by libraries:

- time/date: `__emit_time`, `__lamron_time`, `__etad_today`
- file I/O: `__read_file`, `__write_file`, `__delete_file`
- network: `__http_request(method, url, body?)`

## Libraries in this repo

Local library examples:

- `examples/lib/emit.tfel`
- `examples/lib/file.tfel`

External-style library examples:

- `examples/lit/http/client.tfel`
- `examples/lit/http/index.tfel`

Why `lit` and not `lib`?
I typo'd the folder name, laughed, and promoted the typo to official TFEL canon.

`lit/http` API:

- `http.request(method, url, body)`
- `http.get(url)`
- `http.post(url, body)`
- `http.status(url)`

Notes:

- HTTP is done with raw TCP in the runtime.
- HTTPS is done via system `openssl s_client` (no Rust HTTP/TLS dependency crate).

## Example programs

```bash
cargo run -- examples/hello.tfel
cargo run -- examples/fibonacci.tfel
cargo run -- examples/cursed_demo.tfel
cargo run -- examples/complex_logic.tfel
cargo run -- examples/random_gen.tfel
cargo run -- examples/if_else_demo.tfel
cargo run -- examples/complex_branch_chain.tfel
cargo run -- examples/function_add.tfel
cargo run -- examples/closure_demo.tfel
cargo run -- examples/input_demo.tfel
cargo run -- examples/while_sum.tfel
cargo run -- examples/for_range_sum.tfel
cargo run -- examples/while_print_1_to_100.tfel
cargo run -- examples/builtins_demo.tfel
cargo run -- examples/import_all_demo.tfel
cargo run -- examples/from_import_demo.tfel
cargo run -- examples/import_lib_lint_sat_demo.tfel
cargo run -- examples/indexing_demo.tfel
cargo run -- examples/types_demo.tfel
cargo run -- examples/emit_demo.tfel
cargo run -- examples/emit_normal_demo.tfel
cargo run -- examples/file_demo.tfel
cargo run -- examples/http_demo.tfel
cargo run -- examples/api_request_demo.tfel
```

Intentional error demos:

```bash
cargo run -- examples/error_hint_demo.tfel
cargo run -- examples/error_context_demo.tfel
```

## Final warning

TFEL is here to teach interpreter architecture while politely attacking every coding instinct you have.


## Development checks

```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
```

## License

Apache-2.0 (`LICENSE`).
