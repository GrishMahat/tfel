#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

cargo build -q
bin="target/debug/tfel"

is_expected_failure() {
  local name="$1"
  case "$name" in
    error_context_demo.tfel | error_hint_demo.tfel) return 0 ;;
    *) return 1 ;;
  esac
}

cleanup_generated_artifacts() {
  rm -f \
    examples/scratch/demo.txt \
    examples/scratch/service_health_report.txt \
    examples/scratch/service_health_reporter.md \
    examples/scratch/service_health_reporter.json \
    examples/scratch/weekly_report.txt
  rmdir examples/scratch 2>/dev/null || true
}

run_example() {
  local path="$1"
  local name
  name="$(basename "$path")"

  local -a flags=()
  local input_line=""

  case "$name" in
    api_request_demo.tfel | http_demo.tfel)
      flags+=(--allow-net)
      ;;
    service_health_reporter.tfel)
      flags+=(--allow-fs --allow-net)
      ;;
    file_demo.tfel | service_health_report.tfel | weekly_report_showcase.tfel)
      flags+=(--allow-fs)
      ;;
    input_demo.tfel)
      input_line="demo-input"
      ;;
    fibonacci.tfel)
      input_line="20"
      ;;
  esac

  local out_file err_file
  out_file="$(mktemp)"
  err_file="$(mktemp)"

  local rc=0
  if [[ -n "$input_line" ]]; then
    printf '%s\n' "$input_line" | timeout 20s "$bin" "${flags[@]}" "$path" >"$out_file" 2>"$err_file" || rc=$?
  else
    timeout 20s "$bin" "${flags[@]}" "$path" >"$out_file" 2>"$err_file" || rc=$?
  fi

  if is_expected_failure "$name"; then
    if [[ "$rc" -eq 0 ]]; then
      echo "UNEXPECTED_PASS $name"
      unexpected_pass=$((unexpected_pass + 1))
    else
      echo "EXPECTED_FAIL $name"
      expected_fail=$((expected_fail + 1))
    fi
  else
    if [[ "$rc" -eq 0 ]]; then
      echo "PASS $name"
      pass=$((pass + 1))
    else
      echo "FAIL($rc) $name"
      sed -n '1,12p' "$err_file"
      fail=$((fail + 1))
    fi
  fi

  rm -f "$out_file" "$err_file"
}

pass=0
fail=0
expected_fail=0
unexpected_pass=0

cleanup_generated_artifacts
while IFS= read -r path; do
  run_example "$path"
done < <(find examples -maxdepth 1 -type f -name '*.tfel' | sort)
cleanup_generated_artifacts

echo "SUMMARY pass=$pass fail=$fail expected_fail=$expected_fail unexpected_pass=$unexpected_pass"
if [[ "$fail" -gt 0 || "$unexpected_pass" -gt 0 ]]; then
  exit 1
fi
