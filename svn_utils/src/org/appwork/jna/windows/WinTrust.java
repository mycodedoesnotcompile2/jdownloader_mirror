/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.jna.windows;

import java.util.ArrayList;
import java.util.List;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.Guid;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;

/**
 * Minimal WinTrust API mapping for WinVerifyTrust.
 */
public interface WinTrust extends StdCallLibrary {
    final static WinTrust  INSTANCE                          = Native.load("wintrust", WinTrust.class, W32APIOptions.UNICODE_OPTIONS);
    final static Guid.GUID WINTRUST_ACTION_GENERIC_VERIFY_V2 = new Guid.GUID("{00AAC56B-CD44-11d0-8CC2-00C04FC295EE}");

    int WinVerifyTrust(HWND hwnd, Guid.GUID pgActionID, WINTRUST_DATA pWinTrustData);

    class WINTRUST_FILE_INFO extends Structure {
        public int     cbStruct;
        public WString pcwszFilePath;
        public HANDLE  hFile;
        public Pointer pgKnownSubject;

        public WINTRUST_FILE_INFO(String filePath) {
            this.cbStruct = size();
            this.pcwszFilePath = new WString(filePath);
            this.hFile = null;
            this.pgKnownSubject = null;
            write();
        }

        @Override
        protected List<String> getFieldOrder() {
            List<String> l = new ArrayList<String>();
            l.add("cbStruct");
            l.add("pcwszFilePath");
            l.add("hFile");
            l.add("pgKnownSubject");
            return l;
        }
    }

    class WINTRUST_DATA extends Structure {
        public int     cbStruct;
        public Pointer pPolicyCallbackData;
        public Pointer pSIPClientData;
        public int     dwUIChoice          = 2;                      // WTD_UI_NONE
        public int     fdwRevocationChecks = 0;                      // WTD_REVOKE_NONE
        public int     dwUnionChoice       = 1;                      // WTD_CHOICE_FILE
        public Pointer pFile;
        public int     dwStateAction       = 0;
        public HANDLE  hWVTStateData       = null;
        public Pointer pwszURLReference    = null;
        public int     dwProvFlags         = 0x00000100 | 0x00004000;
        public int     dwUIContext         = 0;

        public WINTRUST_DATA(WINTRUST_FILE_INFO fileInfo) {
            this.cbStruct = size();
            this.pFile = fileInfo.getPointer();
            write();
        }

        @Override
        protected List<String> getFieldOrder() {
            List<String> l = new ArrayList<String>();
            l.add("cbStruct");
            l.add("pPolicyCallbackData");
            l.add("pSIPClientData");
            l.add("dwUIChoice");
            l.add("fdwRevocationChecks");
            l.add("dwUnionChoice");
            l.add("pFile");
            l.add("dwStateAction");
            l.add("hWVTStateData");
            l.add("pwszURLReference");
            l.add("dwProvFlags");
            l.add("dwUIContext");
            return l;
        }
    }
}