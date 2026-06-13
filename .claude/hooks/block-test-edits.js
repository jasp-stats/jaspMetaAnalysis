// PreToolUse hook: blocks Edit/Write on files under tests/
// Contract: read JSON from stdin, echo it to stdout, exit 2 to block
let d = '';
process.stdin.on('data', c => d += c);
process.stdin.on('end', () => {
  try {
    const input = JSON.parse(d);
    const filePath = input.tool_input?.file_path || '';
    if (/(^|[/\\])tests[/\\]/.test(filePath)) {
      process.stderr.write(
        '[Hook] BLOCKED: test files are human-owned. Fix source code instead.\n'
      );
      console.log(d);
      process.exit(2);
    }
  } catch {}
  console.log(d);
});
