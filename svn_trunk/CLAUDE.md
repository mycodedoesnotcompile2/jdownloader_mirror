# JDownloader Project Rules

## Version Control

**This project uses SVN (Subversion), not git.**

When asked for a commit message, provide the message as **plain text with no git-specific syntax** — no `git commit -m` wrapper and no other git wrappers.

Always wrap the commit message in a fenced ```text code block so it can be copied with one click. The block must contain only the commit message itself (subject line, blank line, body), nothing else.

## Building / Compiling

**Never compile the code yourself.** Do not invoke `javac`, Eclipse, Ant, or any other build tool to verify changes. The user compiles and tests in their own IDE. Just write the code correctly; do not run a build to check it.

## Comments

**All code comments must be written in English.** This applies to every comment (line comments, block comments, Javadoc), regardless of the language used in the conversation.

**Use multi-line block comments (`/* ... */` or Javadoc `/** ... */`), not `//` line comments.** This applies to both single- and multi-line comments.

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

## API Field Access (Null / Type Checks)

When parsing API responses (JSON/map fields), do **not** add defensive null-checks or type-checks for fields that are expected to exist. Access them directly and let a missing/malformed required field fail loudly — it signals an API change or a bug, not a case to silently absorb.

Add a null-check or type-check **only** when:

- the field is documented as **optional** in the API docs, or
- the field is explicitly requested to be guarded.

**Forbidden** (unnecessary guard on a required field):
```java
Object id = entries.get("id");
if (id != null && id instanceof Number) {
    link.setProperty("id", ((Number) id).longValue());
}
```

**Correct** (required field — access directly):
```java
final long id = ((Number) entries.get("id")).longValue();
```

**Correct** (field documented as optional — guard it):
```java
final Object description = entries.get("description"); // optional per API docs
if (description != null) {
    link.setComment(description.toString());
}
```

## Shared Plugin State (Cross-Instance Variables)

**Scope:** this rule applies **only to plugin classes** — a class that *is* a plugin or *extends* a plugin class (`PluginForHost` / `PluginForDecrypt`, or a base that extends one, e.g. `antiDDoSForHost`, `antiDDoSForDecrypt`, `UseNet`). It does **not** apply to:

- core/framework classes in `jd.plugins.*` that are not plugins (e.g. `DownloadLink`, `LinkInfo`, `DownloadInterface`) — they are loaded by the normal class loader, so their `static` fields are already shared and stay `static final`;
- helper or UI classes that merely *reference* or *use* a plugin but neither are a plugin nor extend a plugin class (e.g. a `jd.plugins.components.*` `AbstractToolBarAction`) — the same applies, keep `static final`.

Plugins are loaded through JDownloader's `PluginClassLoader`. For a field to be shared across plugin instances it must be:

- **`static`** — the value lives on the class, not the instance, and
- **not `final`** — so the class loader can carry the value across plugin (re)loads, and
- a **container type** — a class that is itself only a holder whose shared content lives *inside* it: `Map` / `Set` / `List`, or an `Atomic*` wrapper (`AtomicReference` / `AtomicLong` / `AtomicInteger` / `AtomicBoolean`).

A `static final` container is **not** reliably shared across instances loaded by different class loaders, so shared caches, tokens and counters must drop `final`.

| Shared state | Declare as |
|---|---|
| `long` / `int` counter | `static AtomicLong` / `static AtomicInteger` |
| `boolean` flag | `static AtomicBoolean` |
| `String` / object slot | `static AtomicReference<T>` |
| cache / lookup table | `static Map<...>` / `static Set<...>` / `static List<...>` |

**Example** (from `RedditComCrawler`):
```java
private static AtomicReference<String> CACHED_LOID           = new AtomicReference<String>();
private static AtomicLong              CACHED_LOID_TIMESTAMP = new AtomicLong(-1);
```

This applies only to container classes that hold shared **state**. An immutable constant — even one whose type is a container, such as a fixed `Arrays.asList(...)` of config values, or a `Pattern` — stays `static final`: it is a per-class constant, not shared cross-instance state.

## Search History Popup (Suggestions)

Applies to every search field that shows a history/suggestion popup. Reference implementation: `SearchField` together with `SearchHistoryManager`.

### Opening direction

The direction is a **fixed design decision per search field**, derived from where that field is placed in the UI. It is **not** calculated at runtime (no screen/window position or free-space checks):

- Search field sits at the **bottom edge** of the window → always open **upward**, above the field. This is the case for the download and linkgrabber search fields, which are always at the very bottom of the JD window.
- Search field sits at the **top or in the middle** of the window → always open **downward**, below the field.

### Sorting

The **newest entry is always the one closest to the search field**, the oldest entry is the farthest away. The list order therefore flips with the direction:

| Direction | Order (top → bottom) |
|---|---|
| Upward | oldest … newest (newest directly above the field) |
| Downward | newest … oldest (newest directly below the field) |

- `SearchHistoryManager.listMatching` returns the entries newest first → use as is for downward, reverse for upward.
- The first arrow key press (UP or DOWN, regardless of which one) selects the entry closest to the field (the newest). Further presses move through the list; moving past either end clears the selection, so the user gets back to the typed text.
- Without a selection the popup is scrolled to the entries closest to the field (the newest); with a selection it is scrolled to the selected entry.
- Inline completion always uses the newest matching entry, independent of the direction.

### Deleting entries

- `DELETE` with a selected entry removes that history entry and does **not** delete text. Without a selected entry `DELETE` behaves like in any text field.
- Every row has a remove icon at its right end. It is only shown for the highlighted row, but its space is always reserved so the text does not jump. A click on the icon removes the entry, a click anywhere else on the row applies it.
- Entries are removed via `SearchHistoryManager.remove(...)` (shared, persistent history), then the popup is refreshed.
- After a removal the selection moves to a neighbor so several entries can be removed in a row. The neighbor is chosen by age, not by screen position: the next **newer** entry (towards the search field) if there is one, else the next **older** one, else the selection is cleared. In a downward popup the newer neighbor is therefore the one above, in an upward popup the one below.

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

## Tables

Applies to every table created in the JDownloader project.

### Structure

- Always extend `jd.gui.swing.jdgui.BasicJDTable<T>` (which extends `ExtTable<T>`) together with an `ExtTableModel<T>` and one `ExtColumn<T>` per column. Never use a raw `JTable` / `DefaultTableModel`. Plain `ExtTable` is only for the AppWorkUtils project.
- Give every table model a unique table ID (e.g. `new FilterTableModel("PackagizerFilterTable")`). Column visibility, order, widths and sort state are persisted under this ID.
- Reference implementation: `PackagizerFilterTable`.

### Selection

- Use `ListSelectionModel.MULTIPLE_INTERVAL_SELECTION` (already the `BasicJDTable` default).
- Ctrl+A (select all) must work in every table where anything can be done with the rows: context menu actions that change data or delete rows, or an "enabled" column. `ExtTable` already handles the shortcut (`onShortcutSelectAll()`), so it works as long as the table is focusable (`setFocusable(true)`, clicking a row must give it the focus, see `SolverOrderTable`), uses multiple selection and nothing swallows the key stroke. Do not disable or override it without a reason.
- Selection must always be preserved across model updates (add / remove / refresh): selected objects stay selected as long as they remain in the model.

### Sorting and columns

- All columns are sortable by default (provide a comparator).
- Exception: interactive columns whose values change live or frequently (counters, countdowns, progress, speed). Do not decide on your own for such columns: **ask the user whether sorting is wanted.**
- Columns are hideable, reorderable and their widths are persisted (default `ExtTable` behaviour; do not disable it without being asked).
- Columns that only ever hold short values of a known maximum length (e.g. an "enabled" checkbox column) may use fixed minimum / maximum widths.

### Display size and scrolling

- Tables are often displayed completely, without any scrollbar. That is the right choice if the table always has a fixed number of rows or never contains "many" rows (rule of thumb: up to about 20).
- To do so, override `isDisplayFullsizeAlways()` in the table and return `true` (see `SolverOrderTable`), and always put the table into the scroll pane returned by `createScrollPane()` instead of `new JScrollPane(table)`. It disables both scrollbars and forwards mouse wheel events to the next scroll pane above, so the surrounding panel still scrolls while the mouse is over the table. For manual sizing use `getFullDisplayHeight()` (includes the extra pixels that prevent a clipped last row and a scrollbar).
- Tables that scroll themselves must also use `createScrollPane()`. Once such a table is at its top/bottom and the user keeps scrolling in that direction, the wheel events are forwarded to the surrounding panel.
- **When in doubt, ask how the table shall be displayed (full size vs. scrollable) before building it. Always ask before building a new table.**

### Context menu and actions

- Override `onContextMenu`. Typical entries, as far as they fit the table: New, Remove, Duplicate.
- If the table has an "enabled" column, the context menu must contain `ToggleEnabledSelectionAction` (see `SolverOrderTable`, `PremiumAccountTable`).
- `onShortcutDelete`: Delete removes the selection through the same `RemoveAction` as the context menu. Delete asks for confirmation, Shift+Delete (`direct == true`) removes without confirmation.
- `setSearchEnabled(true)` for new tables.
- `onDoubleClick` opens the edit dialog, but only if the object has an editor. Otherwise do not override it.

### Threading and updates

- All access to the table and its model happens in the EDT (wrap in `EDTRunner`). Background threads only collect data.
- Frequently changing values (progress, countdown, speed) are refreshed by a periodic repaint of the affected columns, not by rebuilding the model.
## Search Suggestions (Autocomplete / History)

Applies to every new text field that offers suggestions based on previous input. Reference implementation: `SearchField` together with `SearchHistoryManager`.

### History storage

- The history data always lives in a subclass of `org.jdownloader.gui.packagehistorycontroller.HistoryManager<T>` (singleton, persisted via config). Do not build ad-hoc history lists inside the GUI component.
- `HistoryManager` already handles: newest first, case-insensitive duplicate removal, cut to `getMaxLength()`, asynchronous save and save on shutdown. Do not re-implement this.
- The maximum length comes from a config value (`getMaxLength()`): `0` = history disabled (nothing is stored or suggested), default `25`, maximum `250`.
- A term is added to the history on Enter, when a history entry is chosen and on **permanent** focus loss (`!FocusEvent.isTemporary()`, e.g. not when switching to another window). Never while typing. A pending inline completion that has not been accepted is removed before storing on focus loss. This matches the other histories, which store on focus loss as well (e.g. download destination and package name in `AbstractNodePropertiesPanel`).

### Suggestion behavior

- Matching is case-insensitive.
  - Inline completion uses **prefix** matching: it appends the rest of the newest matching entry, selected, like a browser address bar. It keeps the spelling the user typed, only runs while the caret is at the end of the text, and is skipped when deleting text (Backspace / Delete).
  - The suggestion popup uses **contains** matching, newest first. An entry equal to the current text is not listed. An empty field lists all entries.
- While an inline completion is pending, filtering uses only the typed part. Moving the caret or clicking accepts the completion.
- The popup opens above the field when the field sits at the bottom of the window. It is non-focusable so focus and key events stay in the text field, shows at most 10 rows, and is ordered with the newest entry next to the field.

### Keyboard and mouse

| Input | Behavior |
|---|---|
| Up / Down | Opens the popup if closed and moves the selection. The first selection is the entry next to the field. Moving past the first/last entry clears the selection. |
| Enter | Accepts a pending inline completion, or applies the selected entry, or adds the typed text to the history. |
| Delete | With a popup entry selected: removes that entry from the history (instead of deleting text) and selects its neighbor, so several entries can be removed in a row. |
| ESC | First press closes the popup / removes a pending inline completion. The next press clears the field. |
| Mouse | Click on an entry applies it. Click on the remove icon at the right edge of a row removes it. Hovering selects a row. |

## Search Fields: Clear Button

Every **search field** (a field that only filters or looks something up, and is allowed to be empty) shows a clear button (X icon, `IconKey.ICON_CLOSE`) at its right end. It is only visible while the field contains text, clicking it empties the field (the mouse cursor turns into a hand over it).

- `HistorySearchField` draws it by default. A field with its own clear button (e.g. `SearchField`) overrides `isClearButtonEnabled()` and returns `false`.
- Do **not** add it to fields that must always hold a value, e.g. the download directory selection, package name or file name fields. An X there would let the user create an invalid state.

## Time and Durations

- Write durations with the existing constants, not with manual multiplication: `TimeUnit.MINUTES.toMillis(5)` instead of `5 * 60 * 1000l`.
- For calculations with time (e.g. elapsed time, timeouts, wait periods) use `Time.systemIndependentCurrentJVMTimeMillis()` (`org.appwork.utils.Time`) instead of `System.currentTimeMillis()`. It is independent of changes of the system clock.
- `System.currentTimeMillis()` is only for real wall-clock timestamps (e.g. a date that is stored or displayed).

## File Names and Package Names in the GUI

File names and package names can be very long. To display them safely in the GUI, prefer the existing components `jd.controlling.downloadcontroller.FileNameField` and `jd.controlling.downloadcontroller.PackageNameField` over plain labels / text fields.

**Do not convert existing code to these components on your own. Ask first.**
