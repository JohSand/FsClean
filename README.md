# FsClean

Finds dead code, and unneeded project references, in F# projects, and can remove the dead code.

It type-checks your projects with the F# compiler services, builds a graph of which declarations use which,
and reports what nothing live can reach. Because it works by reachability rather than by counting references,
it also finds code that is only kept alive by other dead code, including dead cycles that the compiler's
unused-value warnings can't see.

## Install

```
dotnet tool install --global FsClean
```

It needs the .NET 10 SDK. The projects you analyze must be restored first (`dotnet restore`).

## Use

```
fsclean MyApp.fsproj                     # report dead code
fsclean --whole-program App.fsproj Lib.fsproj Tests.fsproj
fsclean --fix MyApp.fsproj               # remove it
```

Give it every project that consumes the code you care about. Without `--whole-program`, the public API of a
library counts as used, since something outside the analysis may call it. With it, only entry points and
attributed declarations (tests, for instance) are roots, which is what you want for an application and the
libraries it owns.

| Option | |
|---|---|
| `--whole-program` | Treat only entry points as roots, even in libraries. |
| `--fix` | Remove the dead code from the source files. |
| `--explain <text>` | Explain each declaration whose name contains the text: what uses it, what it uses, and what keeps it alive. |
| `--references` | List every project reference with the uses behind it, not only the ones nothing compiles against. |

### Removing code

`--fix` deletes whole declarations together with the comments and attributes directly above them, keeps line
endings and byte order marks, and tidies the blank lines. **Commit first**, so you can review the result with
`git diff`.

Nothing stays removed unless the projects still type-check. A batch that doesn't is split and retried, so one
wrong finding costs that finding and not the run. What can't be removed cleanly is left in place, with the
reason printed: the first declaration of a `let rec ... and` or `type ... and` chain whose later members are
live, and the last member of a live type.

### Project references

Each `<ProjectReference>` is judged by whether the referencing project uses anything declared in the referenced
one, both by every use and by the uses in live code only. A reference can be unneeded, needed only because dead
code uses it, or just the route to a project that should be referenced directly.

## What it can't see

Everything here is compile-time evidence. Code that is only reached through reflection, dependency injection,
serialization or generated code looks dead, and a project reference that is only needed at runtime (a plugin
loaded by reflection, a reference that copies output) looks unneeded. Review what it reports. Initializers that
might have side effects are listed for review and never removed.

## Development

```
dotnet build
dotnet run --project tests/FsClean.Tests
```

The tests run against the small projects in `tests/fixtures`.

## License

MIT
