# JDownloader Project Rules

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
