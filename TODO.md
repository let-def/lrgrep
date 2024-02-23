- Coverage doesn't handle EOL (EOF token that is not named EOF!) correctly
- Coverage can enter infinite loop on degenerate input
  (e.g rule main = parse error | { "Syntax error" })
