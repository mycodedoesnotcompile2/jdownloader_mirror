# JDownloader Project Rules

## Version Control

**This project uses SVN (Subversion), not git.**

When asked for a commit message, provide the message as **plain text with no git-specific syntax** — no `git commit -m` wrapper and no other git wrappers.

Always wrap the commit message in a fenced ```text code block so it can be copied with one click. The block must contain only the commit message itself (subject line, blank line, body), nothing else.

## Comments

**All code comments must be written in English.** This applies to every comment (line comments, block comments, Javadoc), regardless of the language used in the conversation.

## Java Compatibility

**All Java code in this project must be Java 1.6 compatible.**

The following Java 1.7+ features are forbidden:

| Feature | Example (forbidden) |
|---|---|
| Diamond operator | `new ArrayList<>()` → use `new ArrayList<String>()` |
| Try-with-resources | `try (InputStream is = ...)` → use `finally { is.close(); }` |
| Multi-catch | `catch (A \| B e)` → use separate catch blocks |
| Switch on Strings | `switch (str) { case "foo": }` → use `if/else` chains |
| Lambdas | `list.forEach(x -> ...)` → use explicit loops |
| Streams | `list.stream().filter(...)` → use loops |
| Binary literals | `0b1010` → use `10` or hex |
| Underscores in literals | `1_000_000` → use `1000000` |

## Enum Evaluation

**Always evaluate `enum` values with a `switch` statement, never with `if` / `else if` chains.**

A `switch` over an enum makes every possible constant explicit, lets the compiler warn about unhandled constants, and keeps the branches easy to extend. Always include a `default` branch; when an enum value already has a dedicated branch but should share the fallback behaviour (e.g. `AUTO`), fall through into `default` by placing its `case` label directly above it.

**Forbidden:**
```java
if (mode == SubfolderByPackage.ENABLED) {
    result = true;
} else if (mode == SubfolderByPackage.DISABLED) {
    result = false;
} else {
    result = computeAuto();
}
```

**Correct:**
```java
switch (mode) {
case ENABLED:
    result = true;
    break;
case DISABLED:
    result = false;
    break;
case AUTO:
default:
    result = computeAuto();
    break;
}
```

Note: `switch` over an `enum` is Java 1.6 compatible — only `switch` over `String` is forbidden (see Java Compatibility above).

## Shared Plugin State (Cross-Instance Variables)

Fields intended to be shared across plugin instances must **not** use `static`. Instead, they must be `final` and use thread-safe atomic types:

| Type | Use |
|---|---|
| `long` / `int` | `final AtomicLong` / `final AtomicInteger` |
| `String` | `final AtomicReference<String>` |
| Objects | `final AtomicReference<T>` |

**Example** (from `RedditComCrawler`):
```java
private final AtomicReference<String> CACHED_LOID           = new AtomicReference<String>();
private final AtomicLong              CACHED_LOID_TIMESTAMP = new AtomicLong(-1);
```

Never use `static` for such fields — `final` + atomic wrapper is the correct JDownloader convention.

## Throw After a Method That Only Ever Throws

When a method's sole purpose is to throw (it never returns normally), any call to it must be followed by an explicit `throw` — even though that code is unreachable. This keeps the compiler's control-flow analysis happy (e.g. definite-assignment, missing-return-statement) and documents the intent at the call site.

Prefer a `WTFException` (`org.appwork.exceptions.WTFException`) for this unreachable line — it signals "this can never happen" more clearly than a generic `PluginException(LinkStatus.ERROR_PLUGIN_DEFECT)`.

**Example** (from `HighWayMeFolder3`):
```java
if (account == null) {
    errorAccountNeeded();
    /* Unreachable code */
    throw new WTFException();
}
```
where `errorAccountNeeded()` always throws (e.g. `AccountRequiredException`).

Do not include a Co-Authored-By line or "Generated with Claude Code" footer in any commit message.