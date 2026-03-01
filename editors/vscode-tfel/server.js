const {
  createConnection,
  TextDocuments,
  ProposedFeatures,
  DiagnosticSeverity,
  TextDocumentSyncKind,
  MarkupKind
} = require("vscode-languageserver/node");
const { TextDocument } = require("vscode-languageserver-textdocument");

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

const HOVER_DOCS = {
  tel: "`tel name = value;` strict declaration keyword.",
  let: "`let name = value;` compatibility declaration keyword.",
  fed: "`fed name)args( } ... {` function definition.",
  tropmi: "TFEL import keyword.",
  import: "Compatibility import keyword.",
  morf: "TFEL from-import keyword.",
  from: "Compatibility from-import keyword.",
  tropxe: "TFEL export keyword.",
  export: "Compatibility export keyword.",
  nruter: "Return from current function.",
  kaerb: "Break out of the current loop.",
  break: "Break out of the current loop.",
  eunitnoc: "Continue to next loop iteration.",
  continue: "Continue to next loop iteration.",
  rof: "For loop keyword.",
  ni: "For-loop membership keyword.",
  elihw: "While loop keyword.",
  if: "Conditional keyword.",
  else: "Conditional branch keyword.",
  dna: "Logical AND operator.",
  ro: "Logical OR operator.",
  ton: "Logical NOT operator.",
  print: "Print value and return it.",
  true: "Boolean true literal.",
  eurt: "Boolean true literal alias.",
  false: "Boolean false literal.",
  eslaf: "Boolean false literal alias.",
  input: "Read one line of input. Alias: `tupni`.",
  tupni: "Read one line of input. Alias: `input`.",
  len: "Length of array/string. Alias: `nel`.",
  nel: "Length of array/string. Alias: `len`.",
  to_number: "Convert value to number. Alias: `rebmun_ot`.",
  rebmun_ot: "Convert value to number. Alias: `to_number`.",
  range: "Create numeric range array. Alias: `egnar`.",
  egnar: "Create numeric range array. Alias: `range`.",
  type_of: "Get runtime type name. Alias: `fo_epyt`.",
  fo_epyt: "Get runtime type name. Alias: `type_of`.",
  to_string: "Convert value to string. Alias: `gnirts_ot`.",
  gnirts_ot: "Convert value to string. Alias: `to_string`.",
  __emit_time: "Internal time helper.",
  __lamron_time: "Internal normal time helper.",
  __etad_today: "Internal date helper.",
  __read_file: "Internal file read helper (requires `--allow-fs`).",
  __write_file: "Internal file write helper (requires `--allow-fs`).",
  __delete_file: "Internal file delete helper (requires `--allow-fs`).",
  __http_request: "Internal HTTP request helper (requires `--allow-net`)."
};

connection.onInitialize(() => {
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      hoverProvider: true
    }
  };
});

connection.onHover((params) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return null;
  }

  const token = tokenAtPosition(document, params.position);
  if (!token) {
    return null;
  }

  const lookup = token.split(".").pop();
  const doc = HOVER_DOCS[token] || HOVER_DOCS[lookup];
  if (!doc) {
    return null;
  }

  return {
    contents: {
      kind: MarkupKind.Markdown,
      value: `**${token}**\n\n${doc}`
    }
  };
});

documents.onDidOpen((change) => validateDocument(change.document));
documents.onDidChangeContent((change) => validateDocument(change.document));
documents.onDidClose((change) => {
  connection.sendDiagnostics({ uri: change.document.uri, diagnostics: [] });
});

documents.listen(connection);
connection.listen();

function validateDocument(document) {
  const text = document.getText();
  const diagnostics = [];

  collectStructuralDiagnostics(document, text, diagnostics);
  collectLineHeuristicDiagnostics(document, text, diagnostics);

  connection.sendDiagnostics({
    uri: document.uri,
    diagnostics
  });
}

function collectStructuralDiagnostics(document, text, diagnostics) {
  const stack = [];
  let inString = false;
  let escaped = false;
  let stringStart = -1;

  for (let i = 0; i < text.length; i += 1) {
    const ch = text[i];
    const next = text[i + 1];

    if (!inString && ch === "/" && next === "/") {
      while (i < text.length && text[i] !== "\n") {
        i += 1;
      }
      continue;
    }

    if (ch === "\"") {
      if (!inString) {
        inString = true;
        stringStart = i;
        escaped = false;
      } else if (!escaped) {
        inString = false;
        stringStart = -1;
      } else {
        escaped = false;
      }
      continue;
    }

    if (inString) {
      if (ch === "\\" && !escaped) {
        escaped = true;
      } else {
        escaped = false;
      }
      continue;
    }

    if (ch === "(" || ch === "{" || ch === "[") {
      stack.push({ ch, index: i });
      continue;
    }

    if (ch === ")" || ch === "}" || ch === "]") {
      const top = stack[stack.length - 1];
      if (!top || !isMatchingBracket(top.ch, ch)) {
        diagnostics.push({
          severity: DiagnosticSeverity.Error,
          range: rangeFromIndex(document, i, 1),
          message: `unexpected closing '${ch}'`,
          source: "tfel-lsp"
        });
      } else {
        stack.pop();
      }
    }
  }

  if (inString) {
    const length = Math.max(1, text.length - stringStart);
    diagnostics.push({
      severity: DiagnosticSeverity.Error,
      range: rangeFromIndex(document, stringStart, length),
      message: "unterminated string literal",
      source: "tfel-lsp"
    });
  }

  for (const open of stack) {
    diagnostics.push({
      severity: DiagnosticSeverity.Error,
      range: rangeFromIndex(document, open.index, 1),
      message: `unclosed opening '${open.ch}'`,
      source: "tfel-lsp"
    });
  }
}

function collectLineHeuristicDiagnostics(document, text, diagnostics) {
  const lines = text.split(/\r?\n/);

  for (let lineIndex = 0; lineIndex < lines.length; lineIndex += 1) {
    const rawLine = lines[lineIndex];
    const uncommented = rawLine.replace(/\/\/.*$/, "");
    const trimmed = uncommented.trim();

    if (!trimmed) {
      continue;
    }

    if (/^[\[\]{}()]+$/.test(trimmed)) {
      continue;
    }

    if (trimmed.startsWith(";")) {
      continue;
    }

    if (
      trimmed.endsWith(";") ||
      trimmed.endsWith("{") ||
      trimmed.endsWith("}") ||
      trimmed.endsWith("[") ||
      trimmed.endsWith("]") ||
      trimmed.endsWith(",")
    ) {
      continue;
    }

    if (/^\s*(if|else|rof|elihw|fed)\b/.test(trimmed)) {
      continue;
    }

    const firstColumn = rawLine.search(/\S/);
    const start = { line: lineIndex, character: firstColumn < 0 ? 0 : firstColumn };
    const end = { line: lineIndex, character: rawLine.length };

    diagnostics.push({
      severity: DiagnosticSeverity.Warning,
      range: { start, end },
      message: "possible missing semicolon (heuristic check)",
      source: "tfel-lsp"
    });
  }
}

function tokenAtPosition(document, position) {
  const text = document.getText();
  const offset = document.offsetAt(position);
  if (offset < 0 || offset >= text.length) {
    return null;
  }

  const isTokenChar = (ch) => /[A-Za-z0-9_\.]/.test(ch);
  const isTokenStart = (ch) => /[A-Za-z_]/.test(ch);

  let start = offset;
  while (start > 0 && isTokenChar(text[start - 1])) {
    start -= 1;
  }

  let end = offset;
  while (end < text.length && isTokenChar(text[end])) {
    end += 1;
  }

  if (start >= end) {
    return null;
  }

  const token = text.slice(start, end);
  return isTokenStart(token[0]) ? token : null;
}

function rangeFromIndex(document, startIndex, length) {
  const safeStart = Math.max(0, Math.min(startIndex, document.getText().length));
  const safeEnd = Math.max(
    safeStart,
    Math.min(safeStart + Math.max(1, length), document.getText().length)
  );
  return {
    start: document.positionAt(safeStart),
    end: document.positionAt(safeEnd)
  };
}

function isMatchingBracket(open, close) {
  return (
    (open === "(" && close === ")") ||
    (open === "{" && close === "}") ||
    (open === "[" && close === "]")
  );
}
