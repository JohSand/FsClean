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
fsclean --whole-program MyApp.slnx       # every F# project in a solution
fsclean --whole-program App.fsproj Lib.fsproj Tests.fsproj
fsclean --dry MyApp.fsproj               # show what --fix would remove, as a diff, changing nothing
fsclean --fix MyApp.fsproj               # remove it
```

A `.sln`, `.slnx` or `.slnf` stands for the F# projects it lists; projects in other languages are skipped. Give it
every project that consumes the code you care about. Without `--whole-program`, the public API of a
library counts as used, since something outside the analysis may call it. With it, only entry points and
attributed declarations (tests, for instance) are roots, which is what you want for an application and the
libraries it owns.

| Option | |
|---|---|
| `--whole-program` | Treat only entry points as roots, even in libraries. |
| `--exclude <text>` | Leave out the projects whose path contains the text (repeatable), for a project in a solution that doesn't build. One that isn't left out and references it still brings it in. |
| `--fix` | Remove the dead code from the source files. |
| `--dry` | Show what `--fix` would remove, with a diff, and change no file. Implies `--fix`. |
| `--build` | With `--fix`, also build the projects with `dotnet build` once the removals type-check, and refuse whatever that rejects. Slower; see below. |
| `--explain <text>` | Explain each declaration whose name contains the text: what uses it, what it uses, and what keeps it alive. |
| `--references` | List every project reference with the uses behind it, not only the ones nothing compiles against. |

### Removing code

`--fix` deletes whole declarations together with the comments and attributes directly above them, keeps line
endings and byte order marks, and tidies the blank lines. **Commit first**, so you can review the result with
`git diff`, or run with `--dry` to see the diff without changing anything.

Nothing stays removed unless the projects still type-check, and that holds for `--dry` too: the compiler is
shown the edited files from memory. A batch that doesn't type-check is split and retried, so one wrong finding
costs that finding and not the run. What can't be removed cleanly is left in place, with the
reason printed: the first declaration of a `let rec ... and` or `type ... and` chain whose later members are
live, and the last member of a live type.

### Checking the result

The quick check uses the F# compiler service, which is not the compiler: on a large solution it accepted
a removal (a namespace left with nothing in it, and one more case that wasn't tracked down) that `dotnet build`
then rejected. Use `--build` on anything you can't afford to get wrong: after the removals type-check, the
projects are built for real, and removals that the errors name, or that edited a file with an error, are
set aside. A namespace is never left empty, since the compiler treats one as not defined.

### Big solutions

It holds a type-checked copy of the whole solution, so it needs gigabytes, and how many depends on more than the
line count. A `--dry` run took about 2 minutes and peaked at 4.6 GB on a 66k-line solution of 18 projects, and
about 2.5 minutes and 6.0 GB on one of about 400k lines (15 of its 17 projects analyzed). Plan for 6 GB or more on a large solution, and close
other memory-hungry programs first.

`fsclean opens --fix` judges each file on its own, so its cost doesn't grow with the number of rounds: on that
solution, 822 candidate opens in 364 files took about 3 minutes and peaked at about 5.1 GB (`--dry`), where checking the whole
solution once per round took over 7 minutes and 9 GB.

A run watches the machine's available memory (and its container's limit, if it has one). When that stays below
512 MB or 2% of the total, whichever is larger, even after a full collection, it stops, puts back any file
`--fix` had touched, says so, and exits with 3, instead of waiting for the kernel to kill it part-way
through an edit. `FSCLEAN_MEMORY_FLOOR_MB` changes that threshold.

It uses the server garbage collector, which about halves the time at the cost of some extra memory. `--timings`
reports how long each phase took on stderr, and a run prints what it's doing there anyway, because a check of a
large solution takes minutes.

### Newer F# than the tool

The analysis is only as good as the compiler service it's built with, which is F# 10. Code that needs a newer
language version doesn't type-check, and the tool says so instead of guessing. If `dotnet build` accepts the
projects but fsclean doesn't, that's why. To build one for the F# 11 preview:

```
dotnet build src/FsClean -c Release -p:FcsVersion=43.13.101-rc1.26425.128 -p:FSharpCoreVersion=11.0.101-rc1.26425.128
```

A project whose `global.json` pins a newer SDK than the runtime fsclean is on also needs that runtime; set
`DOTNET_ROLL_FORWARD_TO_PRERELEASE=1` if it's a preview.

### Project references

Each `<ProjectReference>` is judged by whether the referencing project uses anything declared in the referenced
one, both by every use and by the uses in live code only. A reference can be unneeded, needed only because dead
code uses it, or just the route to a project that should be referenced directly.

## What it can't see

Everything here is compile-time evidence. Code that is only reached through reflection, dependency injection,
serialization or generated code looks dead, and a project reference that is only needed at runtime (a plugin
loaded by reflection, a reference that copies output) looks unneeded. Review what it reports. Initializers that
might have side effects are listed for review and never removed.

Conventions where a framework calls a method by name are known for ASP.NET middleware (`Invoke`,
`InvokeAsync`) and FsCheck generators; others (for instance a serializer that finds members by reflection)
are not, so a compile-time-clean result still deserves a look.


## Development

```
dotnet build
dotnet run --project tests/FsClean.Tests
```

The tests run against the small projects in `tests/fixtures`.

## License

MIT
