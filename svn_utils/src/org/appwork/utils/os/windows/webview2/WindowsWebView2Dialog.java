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

import javax.swing.JComponent;

import org.appwork.loggingv3.LogV3;
import org.appwork.uio.UIOConstants;
import org.appwork.uio.UIOManager;
import org.appwork.utils.locale._AWU;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.AbstractDialog;

/**
 * Dialog that shows a web page in a {@link WindowsWebView2Panel}. Windows only; check {@link WindowsWebView2Panel#isAvailable()} first.
 *
 * <pre>
 * WindowsWebView2Dialog dialog = new WindowsWebView2Dialog("Help", "https://example.com/help");
 * UIOManager.I().show(null, dialog);
 * </pre>
 *
 * The browser is closed when the dialog is closed, and the dialog closes when the page calls {@code window.close()}. Pass
 * {@link WindowsWebView2Browser.Options} to choose the profile folder, a persistent profile, debug mode, the right-click menu, a URL
 * filter or a proxy selector. Use {@link #getPanel()} before the dialog is shown to add listeners or to load another page.
 */
public class WindowsWebView2Dialog extends AbstractDialog<Integer> {
    private final WindowsWebView2Panel panel;

    /** Shows {@code url} in a dialog with a single "Close" button. */
    public WindowsWebView2Dialog(String title, String url) {
        this(title, url, null);
    }

    /**
     * Shows {@code url} in a dialog with a single "Close" button.
     *
     * @param options
     *            browser options, {@code null} for the defaults
     */
    public WindowsWebView2Dialog(String title, String url, WindowsWebView2Browser.Options options) {
        this(UIOConstants.BUTTONS.BUTTONS_HIDE_OK, title, null, _AWU.T.lit_close(), options);
        if (url != null) {
            try {
                panel.navigate(url);
            } catch (WebView2Exception e) {
                /* Not reachable: before the dialog is shown there is no browser, so the page is only queued. */
                LogV3.log(e);
            }
        }
    }

    /**
     * Dialog without a page; load one through {@link #getPanel()}.
     *
     * @param flags
     *            {@link UIOManager} dialog flags
     * @param options
     *            browser options, {@code null} for the defaults
     */
    public WindowsWebView2Dialog(int flags, String title, String okOption, String cancelOption, WindowsWebView2Browser.Options options) {
        super(flags, title, null, okOption, cancelOption);
        this.panel = new WindowsWebView2Panel(options);
        this.panel.addBrowserListener(new WindowsWebView2Browser.ListenerAdapter() {
            @Override
            public void onWindowCloseRequested() {
                closeFromPage();
            }
        });
        setPreferredWidth(1000);
        setPreferredHeight(700);
    }

    /** The page called {@code window.close()}: close the dialog like its close button does. */
    private void closeFromPage() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                try {
                    setReturnmask(false);
                    dispose();
                } catch (IllegalStateException notShown) {
                    /* The dialog was never shown or is already gone. */
                }
            }
        };
    }

    /** The browser panel of this dialog. */
    public WindowsWebView2Panel getPanel() {
        return panel;
    }

    @Override
    protected Integer createReturnValue() {
        return Integer.valueOf(getReturnmask());
    }

    @Override
    public JComponent layoutDialogContent() {
        return panel;
    }

    /*
     * No own cleanup in dispose(): disposing the window removes the panel from it, and the panel then closes its browser itself
     * (WindowsWebView2Panel.removeNotify), off the EDT.
     */
}
