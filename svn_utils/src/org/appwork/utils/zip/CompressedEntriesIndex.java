/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.zip;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.LEB128;

/**
 * @author daniel
 * @date Aug 23, 2023
 *
 */
public class CompressedEntriesIndex {
    private static final Charset UTF8 = Charset.forName("UTF-8");

    public CompressedEntriesIndex() {
    }

    public byte[] compress(List<String> entries) {
        final ByteArrayOutputStream os = new ByteArrayOutputStream();
        try {
            compress(entries, os);
        } catch (IOException e) {
            throw new WTFException(e);
        }
        return os.toByteArray();
    }

    public void compress(List<String> entries, OutputStream os) throws IOException {
        final List<Object[]> wrapped_entries = new ArrayList<Object[]>();
        final Map<String, Integer> directory_Map = new HashMap<String, Integer>();
        for (final String entry : entries) {
            wrapped_entries.add(new Object[] { 0, entry });
            if (entry.endsWith("/")) {
                directory_Map.put(entry, wrapped_entries.size());
            }
        }
        // compactHierarchy(wrapped_entries, directory_Map);
        final List<Object[]> wrapped_paths = compactPaths(wrapped_entries);
        writeTo(os, entries, wrapped_entries, wrapped_paths);
    }

    protected void writeIntOptimized(OutputStream os, final int value) throws IOException {
        LEB128.write(os, value);
    }

    protected int readIntOptimized(final InputStream is) throws IOException {
        return LEB128.readInt(is, true);
    }

    protected byte[] readFully(final InputStream is, final int len, byte[] buffer) throws IOException {
        if (buffer == null) {
            buffer = new byte[len];
        }
        new DataInputStream(is).readFully(buffer, 0, len);
        return buffer;
    }

    protected void writeTo(OutputStream os, List<String> entries, List<Object[]> wrapped_entries, List<Object[]> wrapped_paths) throws IOException {
        writeIntOptimized(os, entries.size());
        writeIntOptimized(os, wrapped_entries.size() - entries.size());
        for (final Object[] wrappedEntry : wrapped_entries) {
            final Integer entryParentIndex = (Integer) wrappedEntry[0];
            writeIntOptimized(os, entryParentIndex.intValue());
        }
        for (final Object[] wrappedPath : wrapped_paths) {
            final Integer pathIndex = (Integer) wrappedPath[0];
            writeIntOptimized(os, pathIndex.intValue());
        }
        final ByteArrayOutputStream bos = new ByteArrayOutputStream();
        for (final Object[] wrappedPath : wrapped_paths) {
            final Integer pathIndex = (Integer) wrappedPath[0];
            if (pathIndex.intValue() == 0) {
                final String path = (String) wrappedPath[1];
                final byte[] pathBytes = path.getBytes(UTF8);
                writeIntOptimized(os, pathBytes.length);
                bos.write(pathBytes);
            }
        }
        bos.writeTo(os);
    }

    protected List<Object[]> compactPaths(List<Object[]> wrapped_paths) {
        final List<Object[]> ret = new ArrayList<Object[]>();
        final Map<String, Integer> name_Map = new HashMap<String, Integer>();
        for (int index = 0; index < wrapped_paths.size(); index++) {
            final Object[] wrappedPath = wrapped_paths.get(index);
            final String path = (String) wrappedPath[1];
            final Integer pathIndex = name_Map.get(path);
            if (pathIndex == null) {
                final Object[] pathEntry = new Object[] { 0, path };
                ret.add(pathEntry);
                name_Map.put(path, ret.size());
            } else {
                final Object[] pathEntry = new Object[] { pathIndex, path };
                ret.add(pathEntry);
            }
        }
        return ret;
    }

    protected void compactHierarchy(List<Object[]> wrapped_entries, Map<String, Integer> directory_Map) {
        int startIndex = 0;
        final BitSet skipIndex = new BitSet();
        while (true) {
            for (int index = startIndex; index < wrapped_entries.size(); index++) {
                if (skipIndex.get(index)) {
                    startIndex = skipIndex.nextClearBit(startIndex);
                    index = startIndex;
                }
                final Object[] entry = wrapped_entries.get(index);
                final String entryPath = (String) entry[1];
                final int entryPathLength = entryPath.length();
                int lastPathSeparatorIndex = entryPath.lastIndexOf('/', entryPathLength - 1);
                boolean isDirectory = lastPathSeparatorIndex == entryPathLength - 1;
                if (isDirectory) {
                    lastPathSeparatorIndex = entryPath.lastIndexOf('/', lastPathSeparatorIndex - 1);
                }
                if (lastPathSeparatorIndex == -1) {
                    if (isDirectory) {
                        final String parentElemID = entry[0] + "/" + entryPath;
                        directory_Map.put(parentElemID, index);
                        System.out.println("new parent:" + parentElemID + "\t" + index + "\t" + entryPath + "\t Path:" + entryPath);
                    }
                    skipIndex.set(index);
                    continue;
                }
                final String parentElem = entryPath.substring(0, lastPathSeparatorIndex + 1);
                String parentElemID = entry[0] + "/" + parentElem;
                Integer parentIndex = directory_Map.get(parentElemID);
                if (parentIndex == null) {
                    final Object[] newEntry = new Object[] { 0, parentElem };
                    wrapped_entries.add(newEntry);
                    parentIndex = wrapped_entries.size();
                    directory_Map.put(parentElemID, parentIndex);
                    System.out.println("new parent:" + parentElemID + "\t" + parentIndex + "\t" + parentElem + "\t Path:" + entryPath);
                } else {
                    System.out.println("existing parent:" + parentElemID + "\t" + parentIndex + "\t" + parentElem + "\t Path:" + entryPath);
                }
                final String reducedPath = entryPath.substring(lastPathSeparatorIndex + 1);
                entry[0] = parentIndex;
                entry[1] = reducedPath;
                final int nextPathSeparatorIndex = reducedPath.indexOf('/');
                if (nextPathSeparatorIndex == -1) {
                    // file
                    skipIndex.set(index);
                    continue;
                } else if (nextPathSeparatorIndex == reducedPath.length() - 1) {
                    // folder
                    parentElemID = entry[0] + "/" + reducedPath;
                    directory_Map.put(parentElemID, parentIndex);
                    System.out.println("new parent:" + parentElemID + "\t" + parentIndex + "\t" + parentElem + "\t Path:" + entryPath);
                    skipIndex.set(index);
                    continue;
                } else {
                    throw new WTFException();
                }
            }
            if (skipIndex.nextClearBit(0) == wrapped_entries.size()) {
                break;
            } else {
                throw new WTFException();
            }
        }
    }

    protected String resolveEntry(List<Object[]> wrapped_entries, int index) {
        final Object[] wrappedEntry = wrapped_entries.get(index);
        final Integer entryParentIndex = (Integer) wrappedEntry[0];
        final String path = (String) wrappedEntry[1];
        if (entryParentIndex.intValue() == 0) {
            return path;
        } else {
            final String ret = resolveEntry(wrapped_entries, entryParentIndex.intValue() - 1) + path;
            return ret;
        }
    }

    public List<String> uncompress(InputStream is) throws IOException {
        final int entries = readIntOptimized(is);
        final int size = entries + readIntOptimized(is);
        final List<Object[]> wrapped_entries = new ArrayList<Object[]>(size);
        for (int index = 0; index < size; index++) {
            final int parentIndex = readIntOptimized(is);
            wrapped_entries.add(new Object[] { parentIndex, null });
        }
        final List<Object[]> wrapped_paths = new ArrayList<Object[]>(size);
        for (int index = 0; index < size; index++) {
            final int pathIndex = readIntOptimized(is);
            wrapped_paths.add(new Object[] { (int) pathIndex, null });
        }
        for (final Object[] wrappedPath : wrapped_paths) {
            if (((Integer) wrappedPath[0]).intValue() == 0) {
                final int pathLength = readIntOptimized(is);
                wrappedPath[1] = pathLength;
            }
        }
        for (final Object[] wrappedPath : wrapped_paths) {
            if (wrappedPath[1] != null) {
                byte[] pathBytes = readFully(is, (Integer) wrappedPath[1], null);
                final String path = new String(pathBytes, UTF8);
                wrappedPath[1] = path;
            }
        }
        for (int index = 0; index < size; index++) {
            final Object[] wrappedEntry = wrapped_entries.get(index);
            Object[] wrappedPath = wrapped_paths.get(index);
            if (((Integer) wrappedPath[0]).intValue() != 0) {
                wrappedPath = wrapped_paths.get(((Integer) wrappedPath[0]).intValue() - 1);
            }
            wrappedEntry[1] = wrappedPath[1];
        }
        final List<String> ret = new ArrayList<String>(entries);
        for (int index = 0; index < entries; index++) {
            ret.add(resolveEntry(wrapped_entries, index));
        }
        return ret;
    }

    public List<String> uncompress(byte[] data) throws IOException {
        return uncompress(new ByteArrayInputStream(data));
    }
}
