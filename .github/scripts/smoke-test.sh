#!/usr/bin/env bash
# Installs the packed tool from a local folder, the way a user would from nuget.org, and runs it on
# a fixture project. Usage: smoke-test.sh [folder containing the .nupkg, default: artifacts]
set -euo pipefail

artifacts="${1:-artifacts}"
package=$(ls "$artifacts"/FsClean.*.nupkg | head -n 1)
version=$(basename "$package" .nupkg)
version=${version#FsClean.}

tool=$(mktemp -d)
dotnet tool install FsClean --tool-path "$tool" --source "$artifacts" --version "$version"

"$tool/fsclean" --help > /dev/null

project=tests/fixtures/DeadCodeSample/DeadCodeSample.fsproj
dotnet restore "$project"
report=$("$tool/fsclean" "$project")
echo "$report"

# The fixture has this declaration dead on purpose.
grep -q "DeadCodeSample.Library.unusedFunction" <<< "$report"
