/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
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
package org.appwork.utils.os.windows.webview2;

import java.io.File;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.appwork.JNAHelper;
import org.appwork.loggingv3.LogV3;
import org.appwork.utils.Application;
import org.appwork.utils.StringUtils;
import org.appwork.utils.os.CrossSystem;

import com.sun.jna.Function;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.NativeLibrary;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.WinReg;
import com.sun.jna.win32.W32APIOptions;

/**
 * Locates and invokes the WebView2 environment factory.
 * <p>
 * Prefers an optional {@code WebView2Loader.dll} (official SDK entry point). If absent, falls back to the installed Edge WebView2
 * Runtime export {@code CreateWebViewEnvironmentWithOptionsInternal} (no bundled DLL).
 */
public final class WebView2Runtime {
    private static final String              CLIENT_STATE_KEY = "SOFTWARE\\Microsoft\\EdgeUpdate\\ClientState\\{F3017226-FE2A-4295-8BDF-00C3A9A7E4C5}";
    /** {@code LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR | LOAD_LIBRARY_SEARCH_SYSTEM32}: dependencies only from the folder of the DLL and from System32. */
    private static final int                 OPEN_FLAGS       = 0x00000100 | 0x00000800;
    private static final Map<String, Object> LIBRARY_OPTIONS  = libraryOptions();
    private final Function                   createEnvironment;
    private final boolean                    internalApi;

    private WebView2Runtime(Function createEnvironment, boolean internalApi) {
        this.createEnvironment = createEnvironment;
        this.internalApi = internalApi;
    }

    private static Map<String, Object> libraryOptions() {
        final HashMap<String, Object> options = new HashMap<String, Object>(W32APIOptions.DEFAULT_OPTIONS);
        options.put(Library.OPTION_OPEN_FLAGS, Integer.valueOf(OPEN_FLAGS));
        return options;
    }

    /**
     * Cheap check (no DLL loading) whether a loader DLL or an installed Evergreen runtime can be found. Reads the registry and the
     * disk; a positive answer is remembered by the callers that ask often.
     */
    public static boolean isAvailable(String optionalLoaderPath) {
        if (!CrossSystem.isWindows() || !JNAHelper.isJNAAvailable()) {
            return false;
        }
        try {
            return resolveLoader(optionalLoaderPath) != null || findEmbeddedBrowserDll() != null;
        } catch (Throwable t) {
            LogV3.log(t);
            return false;
        }
    }

    public static WebView2Runtime open(String optionalLoaderPath) throws WebView2Exception {
        if (!CrossSystem.isWindows()) {
            throw new WebView2Exception("WebView2 is Windows-only");
        }
        if (!JNAHelper.isJNAAvailable()) {
            throw new WebView2Exception("WebView2 needs JNA (jna.jar and jna-platform.jar) on the class path");
        }
        final File loader = resolveLoader(optionalLoaderPath);
        if (loader != null) {
            try {
                final NativeLibrary lib = NativeLibrary.getInstance(loader.getAbsolutePath(), LIBRARY_OPTIONS);
                /* Both exports are __stdcall; on 32 bit x86 the calling convention must be named explicitly. */
                final Function fn = lib.getFunction("CreateCoreWebView2EnvironmentWithOptions", Function.ALT_CONVENTION);
                LogV3.info("WebView2: using loader " + loader.getAbsolutePath());
                return new WebView2Runtime(fn, false);
            } catch (Throwable e) {
                throw new WebView2Exception("Failed to load WebView2Loader.dll from " + loader.getAbsolutePath(), e);
            }
        }
        final File runtimeDll = findEmbeddedBrowserDll();
        if (runtimeDll == null) {
            throw new WebView2Exception("Edge WebView2 Runtime not found. Install it or set the loader path.");
        }
        try {
            final NativeLibrary lib = NativeLibrary.getInstance(runtimeDll.getAbsolutePath(), LIBRARY_OPTIONS);
            final Function fn = lib.getFunction("CreateWebViewEnvironmentWithOptionsInternal", Function.ALT_CONVENTION);
            LogV3.info("WebView2: using runtime " + runtimeDll.getAbsolutePath());
            return new WebView2Runtime(fn, true);
        } catch (Throwable e) {
            throw new WebView2Exception("Failed to load EmbeddedBrowserWebView.dll from " + runtimeDll.getAbsolutePath(), e);
        }
    }

    /**
     * Starts asynchronous environment creation. Completion is delivered on the calling STA thread via COM callbacks (message pump
     * required).
     */
    public int createEnvironment(String userDataFolder, Pointer completionHandler) {
        if (internalApi) {
            /* HRESULT(bool, runtimeType, PCWSTR userDataFolder, IUnknown* options, handler*) */
            return createEnvironment.invokeInt(new Object[] { Integer.valueOf(1), Integer.valueOf(0), userDataFolder == null ? null : new WString(userDataFolder), Pointer.NULL, completionHandler });
        }
        /* CreateCoreWebView2EnvironmentWithOptions(PCWSTR browserFolder, PCWSTR userData, options, handler) */
        return createEnvironment.invokeInt(new Object[] { Pointer.NULL, userDataFolder == null ? null : new WString(userDataFolder), Pointer.NULL, completionHandler });
    }

    /** Folder name of the WebView2 binaries for the architecture of this JVM process: {@code x64}, {@code x86} or {@code arm64}. */
    static String processArchitecture() {
        final String arch = String.valueOf(System.getProperty("os.arch")).toLowerCase(Locale.ENGLISH);
        if (arch.contains("aarch64") || arch.contains("arm64")) {
            return "arm64";
        }
        return Native.POINTER_SIZE == 8 ? "x64" : "x86";
    }

    private static File resolveLoader(String optionalLoaderPath) {
        if (StringUtils.isNotEmpty(optionalLoaderPath)) {
            final File f = new File(optionalLoaderPath);
            if (f.isFile()) {
                return f;
            }
        }
        final String[] candidates = new String[] { "tools/WebView2/" + processArchitecture() + "/WebView2Loader.dll", "tools/WebView2/WebView2Loader.dll", "WebView2Loader.dll" };
        for (int i = 0; i < candidates.length; i++) {
            final File f = Application.getResource(candidates[i]);
            if (f != null && f.isFile()) {
                return f;
            }
        }
        return null;
    }

    /** The runtime DLL of the first registration (machine, then user; 64 bit view, then 32 bit view) whose file really exists. */
    private static File findEmbeddedBrowserDll() {
        final WinReg.HKEY[] roots = new WinReg.HKEY[] { WinReg.HKEY_LOCAL_MACHINE, WinReg.HKEY_CURRENT_USER };
        final String[] keys = new String[] { CLIENT_STATE_KEY, CLIENT_STATE_KEY.replace("SOFTWARE\\Microsoft\\", "SOFTWARE\\WOW6432Node\\Microsoft\\") };
        for (WinReg.HKEY root : roots) {
            for (String key : keys) {
                final String base = readEbWebView(root, key);
                if (StringUtils.isEmpty(base)) {
                    continue;
                }
                final File dll = new File(base, "EBWebView" + File.separator + processArchitecture() + File.separator + "EmbeddedBrowserWebView.dll");
                if (dll.isFile()) {
                    return dll;
                }
            }
        }
        return null;
    }

    private static String readEbWebView(WinReg.HKEY root, String key) {
        try {
            if (Advapi32Util.registryKeyExists(root, key) && Advapi32Util.registryValueExists(root, key, "EBWebView")) {
                return Advapi32Util.registryGetStringValue(root, key, "EBWebView");
            }
        } catch (Throwable ignored) {
            /* No access or wrong type: treated as not registered. */
        }
        return null;
    }
}
