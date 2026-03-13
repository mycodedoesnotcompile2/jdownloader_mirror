/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58, 91183 Abenberg, Germany
 *         (License header abbreviated; see project license.)
 * ==================================================================================================================================================== */
package org.appwork.testframework.tests;

import java.io.File;
import java.io.Serializable;

import org.appwork.testframework.JRETestTask;
import org.appwork.utils.IO;
import org.appwork.utils.IO.SYNC;

/**
 * Serializable task used by {@link TestRunInJRETask}. Writes a marker and the current Java version to the given file path so the
 * parent can verify the task ran in the child JRE.
 */
public final class RunInJRETaskTarget implements JRETestTask {
    private static final long serialVersionUID = 1L;

    private final String outputPath;

    public RunInJRETaskTarget(final String outputPath) {
        this.outputPath = outputPath;
    }

    @Override
    public void run() throws Exception {
        final String line = "OK " + System.getProperty("java.version");
        IO.secureWrite(new File(outputPath), line, SYNC.META_AND_DATA);
    }
}
