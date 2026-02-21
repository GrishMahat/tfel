# TFEL
## Language Specification & Design Document
*Version 0.1 — Initial Draft*

> "A programming language so cursed, it makes developers question their career choices."

---

## 1. What is Tfel?

Tfel (pronounced "teh-fell", or just "that thing") is an interpreted programming language built on a single, unified philosophy: everything you know about writing code is wrong. Every syntactic convention, every structural habit, every muscle memory you have built from years of programming — Tfel takes it, flips it upside down, and hands it back to you with a smile.

The name Tfel is "left" spelled backwards. This is fitting, because in Tfel, what comes last comes first, what opens must close before it opens, and what you expect to be on the left is on the right.

Tfel is not a joke language with no rules. It is a fully consistent, internally logical language. It has rules. It has structure. It has syntax. Those rules just happen to be the mirror image of everything the programming world has agreed on for 70 years. This is what makes it genuinely infuriating — you cannot dismiss it as random chaos. There is a twisted logic to it, and that logic will haunt you.

> 😈 **DESIGN NOTE:** The cruelest part of Tfel is that someone could actually learn it, get good at it, and become productive in it. Imagine having that on your resume.

---

## 2. Design Philosophy

### 2.1 The Core Rule

Every design decision in Tfel follows one rule:

> **"If a normal language does it one way, Tfel does it the opposite way."**

This rule is applied consistently and without mercy. The goal is not randomness — it is perfect, deliberate inversion. A developer reading Tfel code should feel like they are reading in a mirror.

### 2.2 Why Did We Do This?

Because it is funny. But also because building a language with a strict philosophical constraint — even a silly one — forces interesting decisions and teaches you a great deal about why languages are designed the way they are. Every time we flip a convention, we are forced to ask: why does the normal version exist? What would break if it were reversed? Can a reversed version still be coherent?

Tfel answers all of those questions. Poorly, but consistently.

### 2.3 What Tfel Is NOT

- Tfel is not an esolang with no real syntax (like Brainfuck). It has real constructs.
- Tfel is not random. Every rule has a clear opposite that it mirrors.
- Tfel is not unrunnable. Real programs can be written and executed in it.
- Tfel is not a good idea. But we are doing it anyway.

---

## 3. Core Language Rules

### 3.1 Rule 1 — Execution Order: Bottom to Top

In every normal language, code runs from the top of the file to the bottom. Line 1 executes first, then line 2, and so on. In Tfel, the last line of the file executes first. Execution travels upward through the file.

This means the entry point of your program — the first thing that runs — is at the very bottom of the file. Imports, setup, main logic — all of it is written in reverse order from what you would expect.

**Why?**
Because in every language ever made, top-to-bottom execution is so deeply assumed that it is essentially invisible. Nobody thinks about it. Reversing it makes that assumption suddenly visible and incredibly painful.

**Example:**
```
# This runs THIRD (top of file = last to execute)
10 = x

# This runs SECOND
)x(print

# This runs FIRST (bottom of file = first to execute)
"Starting..." = message
```

> **NOTE:** The mental model: imagine reading the file from the bottom with your finger. That is the order the interpreter follows.

---

### 3.2 Rule 2 — Brackets Are Swapped

In Tfel, every bracket is replaced with its mirror image:

| Normal Language | Tfel | Purpose |
|---|---|---|
| `(  )` | `)  (` | Function calls, expressions |
| `{  }` | `}  {` | Code blocks |
| `[  ]` | `]  [` | Arrays, indexing |

This means every function call, every block, every array literal looks structurally wrong to any developer who has ever written code before. Your brain will fight you every single time.

**Why?**
Brackets are the most universal visual landmark in programming. They signal structure. Swapping them breaks that signal completely while keeping the structure itself intact. The code is still parseable — the grammar still works — it just looks profoundly wrong.

**Function call example:**
```
# Normal language:
print("hello")

# Tfel:
)"hello"(print
```

**Block example:**
```
# Normal language:
if (x > 0) {
    print("yes")
}

# Tfel:
}
    )"yes"(print
{ if )x > 0(
```

> 😈 **DESIGN NOTE:** Reading a Tfel block out loud: "Closing brace... code... opening brace if... closing paren x greater than zero opening paren." Perfectly horrible.

---

### 3.3 Rule 3 — Conditions Are Written Bottom to Top

In a normal language, an if/else block is written: condition first, then the true body, then the else body. In Tfel, the else body comes first at the bottom, then the if body above it, then the condition at the very top of the construct.

**Normal language:**
```
if (x > 0) {
    print("positive")
} else {
    print("not positive")
}
```

**Tfel:**
```
else }
    )"not positive"(print
{
}
    )"positive"(print
{ if )x > 0(
```

Reading this in execution order (bottom to top): evaluate condition, if true run the upper block, if false run the lower block. It is perfectly logical. It is also perfectly horrible.

> **NOTE:** There is no "else if" in Tfel v0.1. Nested conditions are achieved by nesting cursed blocks inside cursed blocks. We are sorry.

---

### 3.4 Rule 4 — Imports Are at the Bottom

In every normal language, imports are declared at the top of the file. In Tfel, since the bottom of the file runs first, imports go at the bottom. This is actually the only rule that makes perfect internal sense — you import things before you use them, and since execution starts at the bottom, that is where imports live.

Of course, it looks completely insane when you read the file top to bottom, because the imports are hidden at the very end.

**Import syntax:**
```
# Normal: import math
"math" tropmi

# Normal: from math import sqrt
"sqrt" morf "math" tropmi
```

Keywords are also reversed: "import" becomes "tropmi", "from" becomes "morf". Because of course they are.

> 😈 **DESIGN NOTE:** Imagine being a new developer, opening a Tfel file, scrolling to the bottom looking for end-of-file comments, and finding all the imports there instead. The confusion alone is worth building this language.

---

### 3.5 Rule 5 — Assignment Is Right to Left

In normal languages, the variable goes on the left and the value goes on the right: `x = 5`. In Tfel, the value goes on the left and the variable goes on the right.

```
# Normal: x = 10
10 = x

# Normal: name = "Alice"
"Alice" = eman
```

> **NOTE:** Variable names themselves are NOT reversed. Only the assignment order is swapped. We considered reversing variable names but decided that was too unreadable even for us.

---

## 4. Full Syntax Reference

### 4.1 Variables

```
42 = age
"hello" = greeting
eurt = isValid        # true in Tfel is "eurt", false is "eslaf"
```

### 4.2 Printing Output

```
)"hello world"(print

# Print a variable
)eman(print
```

### 4.3 Functions

Function definitions put the body first at the top and the signature last at the bottom. Since execution is bottom to top, the signature is read before the body — correct logical order, just written upside down.

```
# Normal:
# def greet(name):
#     print("Hello, " + name)

# Tfel:
}
    )eman + " ,olleH"(print
{ )eman(teerg fed

# Call it:
)"dlroW"(teerg
```

> **NOTE:** "fed" is "def" backwards. Function keyword is reversed like import keywords.

### 4.4 Loops

```
# Normal: for i in range(5): print(i)

# Tfel:
}
    )i(print
{ )5(egnar ni i rof
```

### 4.5 Arrays

```
# Normal: items = [1, 2, 3]
]1 ,2 ,3[ = smeti

# Access element (Normal: first = items[0])
]0[smeti = tsrif
```

### 4.6 Complete Hello World

```
)"Hello World"(print
```

### 4.7 FizzBuzz — The True Test of Suffering

```
}
    else }
        else }
            )i(print
        {
        }
            )"zzuB"(print
        { if )0 == )5 % i(
    {
    }
        )"zziFzzuB"(print
    { if )0 == )51 % i(
{ )101(egnar ni i rof
```

> 😈 **DESIGN NOTE:** If a candidate solves FizzBuzz in Tfel during an interview, hire them immediately. They are either a genius or deeply unwell. Possibly both.

---

## 5. Keywords Reference

| Normal Keyword | Tfel Keyword | Notes |
|---|---|---|
| `def` | `fed` | Function definition |
| `import` | `tropmi` | Import module |
| `from` | `morf` | From keyword in imports |
| `return` | `nruter` | Return from function |
| `if` | `if` | NOT reversed — too confusing even for us |
| `else` | `else` | NOT reversed — readability mercy |
| `for` | `rof` | Loop keyword |
| `in` | `ni` | Loop iteration keyword |
| `while` | `elihw` | While loop |
| `true` | `eurt` | Boolean true |
| `false` | `eslaf` | Boolean false |
| `and` | `dna` | Logical and |
| `or` | `ro` | Logical or |
| `not` | `ton` | Logical not |
| `print` | `print` | NOT reversed — too essential to daily use |

> **NOTE:** We chose not to reverse 'if', 'else', and 'print' because even we have limits. The structural inversion — bottom-to-top execution and swapped brackets — is punishment enough.

---

## 6. Implementation Plan

### 6.1 Overview

Tfel will be implemented as a tree-walking interpreter written in Rust. The interpreter takes a `.tfel` source file, processes it, and executes it directly without a compilation step. This is the fastest path to a working language and the most appropriate choice for a v0.1 project.

### 6.2 Components

| Component | Responsibility | Cursed Detail |
|---|---|---|
| Lexer | Reads raw source text, produces tokens | Swaps bracket tokens during tokenization |
| Pre-processor | Reverses line order before parsing | This is where bottom-to-top execution is implemented |
| Parser | Builds an Abstract Syntax Tree from tokens | Handles inverted block structure and right-to-left assignment |
| Evaluator | Walks the AST and executes nodes | Manages scope, function calls, variable environments |
| Runtime | Built-in functions, error messages | Error messages direction — TBD |

### 6.3 Two Week Build Timeline

| Day | Goal |
|---|---|
| Day 1 | Project setup, Rust workspace, define Token enum and AST node types |
| Day 2 | Lexer — tokenize basic source text, implement bracket swapping |
| Day 3 | Pre-processor — reverse line order before handing to parser |
| Day 4 | Parser — parse expressions and assignment statements into AST |
| Day 5 | Parser continued — if/else blocks, function definitions |
| Day 6 | Evaluator — variables, right-to-left assignment, print |
| Day 7 | Evaluator — if/else execution, scope management |
| Day 8 | Evaluator — functions, calls, return values |
| Day 9 | Evaluator — for and while loops |
| Day 10 | Imports — tropmi/morf system |
| Day 11 | Arrays, indexing, basic data types |
| Day 12 | Error handling and error messages |
| Day 13 | Write real test programs in Tfel, find and fix bugs |
| Day 14 | Polish, write README, write cursed Hello World tutorial, celebrate |

### 6.4 Project Structure

```
tfel/
├── src/
│   ├── main.rs           # Entry point, reads .tfel file
│   ├── lexer.rs          # Tokenizer + bracket swapping
│   ├── preprocessor.rs   # Line reversal
│   ├── parser.rs         # AST builder
│   ├── ast.rs            # AST node definitions
│   ├── evaluator.rs      # Tree walker / executor
│   └── environment.rs    # Variable scope
├── examples/
│   ├── hello.tfel
│   ├── fibonacci.tfel
│   └── fizzbuzz.tfel
├── Cargo.toml
└── README.md             # Written straight-faced, as if this is a normal language
```

### 6.5 Learning Resources

- **Crafting Interpreters** by Robert Nystrom — free at [craftinginterpreters.com](https://craftinginterpreters.com). Follow the concepts and apply Tfel's cursed rules on top.
- **The Rust Book** — [doc.rust-lang.org/book](https://doc.rust-lang.org/book) — read the first half before starting.
- **Writing an Interpreter in Go** by Thorsten Ball — shorter alternative reference, very practical and hands-on.

---

## 7. Design Decisions Log

| Decision | What We Chose | Why |
|---|---|---|
| Execution order | Bottom to top | The most fundamental and invisible assumption in all programming. Breaking it causes maximum disorientation. |
| Brackets | Fully swapped `( ) { } [ ]` | Brackets are the most universal visual landmark in code. Swapping them breaks pattern recognition while keeping the grammar valid. |
| Keywords | Spelled backwards (fed, tropmi, elihw...) | Still recognizable if you think about them — which is what makes them cursed rather than just unreadable. |
| 'if' and 'else' | NOT reversed | Reversing these made conditions completely unreadable even in context. We chose mercy. |
| 'print' | NOT reversed | Print is used constantly, especially during debugging. Making it 'tnirp' was too cruel for practical use. |
| String content | NOT automatically reversed | Reversing string content would mean writing all strings backwards, which moves from 'cursed' to 'completely unusable'. |
| Variable names | NOT reversed | Same reason as strings. The structural inversions are sufficient suffering. |
| Interpreted vs Compiled | Interpreted | Faster to build, easier to iterate, sufficient for v0.1. Compilation can be added later. |
| Implementation language | Rust | Good learning exercise. Type safety catches parser bugs early. |
| Assignment direction | Value on left, variable on right | Jarring but readable once you know the rule. Mirrors the bottom-to-top philosophy. |
| File extension | `.tfel` | Not `.lfer` (tfel reversed). We are cursed, not incomprehensible. |

---

## 8. Open Questions

- Should error messages be printed in reverse? e.g. `".2 enil ta rorre xatnyS"` — very on-brand but might make debugging impossible.
- Should there be a standard library? If so, how are modules structured and named?
- What does a Tfel stack trace look like? Should the bottom of the stack appear at the top?
- Is there string interpolation? If so, what does it look like in Tfel?
- Should comments optionally be written right-to-left? Enforced or just convention?
- What happens when you import a normal module into Tfel? Does it need a Tfel wrapper?

---

## 9. Final Note

Tfel is a bad idea executed with full commitment. The design is internally consistent, the syntax is learnable, and real programs can be written in it. It will never be used in production. It will never ship a product. It will never appear in a job description. But building it will teach you how lexers work, how parsers work, how evaluators work, and how every single convention in programming exists for a very good reason — a reason that only becomes obvious when you remove it.

Also it is extremely funny.

---

The language design is very human, very workable, very human.

---

## 10. Lecture Recital (Improved Version)

Use this section when you want to explain Tfel to friends, teammates, or innocent bystanders.

### 10.1 One-Minute Opening

Tfel is a programming language where normal programming instincts are intentionally inverted.  
Code executes from bottom to top. Brackets are mirrored. Assignment is right-to-left.  
Keywords are mostly reversed.  
And somehow, despite all of that, the language is still consistent and runnable.

The joke is not randomness. The joke is disciplined inversion.

### 10.2 Why This Is Funny (and Useful)

- It is funny because every rule attacks muscle memory directly.
- It is useful because every inversion reveals why the original convention exists.
- It is educational because building Tfel forces real compiler/interpreter design thinking.
- It is memorable because once you write one Tfel file, your brain never forgets it.

### 10.3 Five-Minute Lecture Script

1. "Tfel is `left` backwards. That is the entire warning label."
2. "In Tfel, the bottom line runs first, so your entry point lives at the bottom."
3. "Brackets are swapped, so your eyes report syntax errors even when code is valid."
4. "Assignment is reversed: value on the left, variable on the right."
5. "Most keywords are reversed, except `if`, `else`, and `print` because we chose partial mercy."
6. "The project is cursed, but the architecture is standard: preprocess -> lex -> parse -> evaluate."
7. "So yes, this is a joke. But it is a joke that teaches real language engineering."

### 10.4 Presenter Notes

- Deliver the rules calmly, like this is completely normal software.
- Pause after each inversion and let people process the emotional damage.
- Emphasize that the system is deterministic, not random chaos.
- End with: "Tfel is bad for production and excellent for learning."

### 10.5 TL;DR for Slides

> Tfel is a mirror-world interpreter.  
> It is funny because it is coherent.  
> It is coherent because every rule is deliberately inverted.  
> It teaches language design by weaponizing discomfort.

Everything about everything should be good, even when the syntax is deeply cursed.
