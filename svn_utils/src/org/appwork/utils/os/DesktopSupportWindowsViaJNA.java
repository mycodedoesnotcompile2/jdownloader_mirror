package org.appwork.utils.os;

import java.io.File;
import java.io.IOException;

import org.appwork.builddecision.BuildDecisionRequired;
import org.appwork.testhelper.AWTestValidateClassReference;

@BuildDecisionRequired(tags = { DesktopSupportWindowsViaJNA.DESKTOP_SUPPORT_WINDOWS_VIA_JNA_YES, DesktopSupportWindowsViaJNA.DESKTOP_SUPPORT_WINDOWS_VIA_JNA_NO }, imports = { DesktopSupportWindowsViaJNA.ORG_APPWORK_UTILS_OS_DESKTOP_SUPPORT_WINDOWS_VIA_JNA, "" }, dependsOn = { org.appwork.JNAHelper.JNA_HELPER_USE_JNA, "" })
public class DesktopSupportWindowsViaJNA extends DesktopSupportWindows {
    @AWTestValidateClassReference
    static final String        ORG_APPWORK_UTILS_OS_DESKTOP_SUPPORT_WINDOWS_VIA_JNA = "org.appwork.utils.os.DesktopSupportWindowsViaJNA";
    public static final String DESKTOP_SUPPORT_WINDOWS_VIA_JNA_NO                   = "DesktopSupportWindowsViaJNA.no";
    public static final String DESKTOP_SUPPORT_WINDOWS_VIA_JNA_YES                  = "DesktopSupportWindowsViaJNA.yes";

    public DesktopSupportWindowsViaJNA() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @see org.appwork.utils.os.DesktopSupportWindows#openFile(java.io.File, boolean)
     */
    @Override
    public void openFile(File file, boolean tryToReuseWindows) throws IOException {
        if (tryToReuseWindows) {
            if (file.isDirectory()) {
                if (WindowsUtils.explorerToFront(file)) {
                    return;
                }
            }
        }
        super.openFile(file, tryToReuseWindows);
    }
}
