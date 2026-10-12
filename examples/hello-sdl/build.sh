#!/usr/bin/env bash
set -euo pipefail

example_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_dir="$(cd "$example_dir/../.." && pwd)"
output_dir="$example_dir/build"

if [[ "$(uname -s)" != Darwin || "$(uname -m)" != arm64 ]]; then
  echo "This example targets Apple Silicon macOS." >&2
  exit 1
fi
for tool in cargo clang pkg-config; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "Required tool not found: $tool" >&2
    exit 1
  fi
done
if ! pkg-config --atleast-version=3.2 sdl3; then
  echo "SDL3 >= 3.2 is required (and must be discoverable by pkg-config)." >&2
  exit 1
fi

mkdir -p "$output_dir"
cd "$repo_dir"
cargo run --quiet -p magelang -- compile examples/hello-sdl/main \
  --backend naive --emit-asm -o "$output_dir/hello-sdl.s"

read -r -a sdl_libs <<< "$(pkg-config --libs sdl3)"
clang -arch arm64 "$output_dir/hello-sdl.s" "${sdl_libs[@]}" \
  -framework OpenGL -o "$output_dir/hello-sdl"
printf 'Built %s\n' "$output_dir/hello-sdl"
