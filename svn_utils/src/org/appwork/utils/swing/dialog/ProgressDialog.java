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
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.swing.dialog;

import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.SwingConstants;
import javax.swing.Timer;
import javax.swing.WindowConstants;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.appwork.swing.MigPanel;
import org.appwork.uio.UIOManager;
import org.appwork.utils.BinaryLogic;
import org.appwork.utils.InterruptibleThread;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTHelper;

/**
 * @author thomas
 *
 */
public class ProgressDialog extends AbstractDialog<Integer> implements ProgressInterface {
    public interface ProgressGetter {
        public int getProgress();

        public String getString();

        public void run() throws Exception;

        /**
         * @return
         */
        public String getLabelString();
    }

    private Thread           executer;
    protected ProgressGetter getter;
    private final String     message;
    private Timer            updater;
    private long             waitForTermination = 20000;
    protected Throwable      throwable          = null;
    private JLabel           lbl;
    protected JTextPane      textField;

    /**
     * @param progressGetter
     * @param flags
     *            TODO
     * @param icon
     *            TODO
     * @param s
     * @param s2
     */
    public ProgressDialog(final ProgressGetter progressGetter, final int flags, final String title, final String message, final Icon icon) {
        this(progressGetter, flags, title, message, icon, null, null);
    }

    public ProgressDialog(final ProgressGetter progressGetter, final int flags, final String title, final String message, final Icon icon, final String ok, final String cancel) {
        super(flags | UIOManager.BUTTONS_HIDE_OK, title, icon, ok, cancel);
        this.message = message;
        if (progressGetter == null && this instanceof ProgressGetter) {
            getter = (ProgressGetter) this;
        } else {
            getter = progressGetter;
        }
        this.setReturnmask(true);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.AbstractDialog#getRetValue()
     */
    @Override
    protected Integer createReturnValue() {        
        return this.getReturnmask();
    }

    @Override
    public void dispose() {
        if (isDisposed()) {
            return;
        }
        System.out.println("Dispose Progressdialog");
        if (this.executer.isAlive()) {
            this.executer.interrupt();
            final long waitFor = this.getWaitForTermination();
            if (waitFor > 0) {
                try {
                    this.executer.join(waitFor);
                } catch (final InterruptedException e) {
                }
            }
        }
        super.dispose();
    }

    protected void addMessageComponent(final MigPanel p) {
        textField = new JTextPane() {
            private static final long serialVersionUID = 1L;

            @Override
            public boolean getScrollableTracksViewportWidth() {
                return !BinaryLogic.containsAll(flagMask, Dialog.STYLE_LARGE);
            }
        };
        modifyTextPane(textField);
        final Font font = textField.getFont();
        if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_HTML)) {
            textField.setContentType("text/html");
            textField.addHyperlinkListener(new HyperlinkListener() {
                public void hyperlinkUpdate(final HyperlinkEvent e) {
                    if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                        CrossSystem.openURL(e.getURL());
                    }
                }
            });
        } else {
            textField.setContentType("text/plain");
            // this.textField.setMaximumSize(new Dimension(450, 600));
        }
        textField.setFont(font);
        textField.setText(message);
        textField.setEditable(false);
        textField.setBackground(null);
        textField.setOpaque(false);
        textField.setFocusable(false);
        textField.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        textField.setCaretPosition(0);
        if (BinaryLogic.containsAll(flagMask, Dialog.STYLE_LARGE)) {
            p.add(new JScrollPane(textField), "pushx,growx,spanx");
        } else {
            p.add(textField, "pushx,growx,spanx");
        }
    }

    /**
     * @param textField2
     */
    private void modifyTextPane(JTextPane textField2) {        
    }

    /**
     * @return the throwable
     */
    public Throwable getThrowable() {
        return this.throwable;
    }

    public long getWaitForTermination() {
        return this.waitForTermination;
    }

    @Override
    public JComponent layoutDialogContent() {
        this.getDialog().setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        final MigPanel p = new MigPanel("ins 0,wrap 2", "[][]", "[][]");
        this.extendLayout(p);
        addMessageComponent(p);
        this.textField.setText(this.message);
        this.extendLayout(p);
        final JProgressBar bar;
        p.add(bar = new JProgressBar(0, 100), "growx,pushx" + (this.isLabelEnabled() ? "" : ",spanx"));
        bar.setStringPainted(true);
        if (this.isLabelEnabled()) {
            this.lbl = new JLabel();
            this.lbl.setHorizontalAlignment(SwingConstants.RIGHT);
            p.add(this.lbl, "wmin 30");
        }
        this.updater = new Timer(50, new ActionListener() {
            public void actionPerformed(final ActionEvent e) {
                if (ProgressDialog.this.getter != null) {
                    final int prg = ProgressDialog.this.updateProgress(bar, ProgressDialog.this.getter);
                    ProgressDialog.this.updateText(bar, ProgressDialog.this.getter);
                    ProgressDialog.this.updateLabel();
                    if (prg >= 100) {
                        ProgressDialog.this.updater.stop();
                        ProgressDialog.this.dispose();
                        return;
                    }
                }
            }
        });
        this.updater.setRepeats(true);
        this.updater.setInitialDelay(50);
        this.updater.start();
        this.executer = new InterruptibleThread("ProgressDialogExecuter") {
            @Override
            public void run() {
                try {
                    ProgressDialog.this.getter.run();
                } catch (final Throwable e) {
                    ProgressDialog.this.throwable = e;
                    e.printStackTrace();
                    ProgressDialog.this.setReturnmask(false);
                } finally {
                    new EDTHelper<Object>() {
                        @Override
                        public Object edtRun() {
                            ProgressDialog.this.dispose();
                            return null;
                        }
                    }.start();
                    ProgressDialog.this.updater.stop();
                }
            }
        };
        this.executer.start();
        return p;
    }

    /**
     * @param p
     */
    protected void extendLayout(JPanel p) {        
    }

    /**
     * @return
     */
    protected boolean isLabelEnabled() {        
        return false;
    }

    public void setWaitForTermination(final long waitForTermination) {
        this.waitForTermination = waitForTermination;
    }

    protected void updateText(final JProgressBar bar, final ProgressGetter getter) {
        final String text = getter.getString();
        if (text == null) {
            bar.setStringPainted(false);
        } else {
            bar.setStringPainted(true);
            bar.setString(text);
        }
    }

    protected int updateProgress(final JProgressBar bar, final ProgressGetter getter) {
        final int prg = getter.getProgress();
        if (prg < 0) {
            bar.setIndeterminate(true);
        } else {
            bar.setIndeterminate(false);
            bar.setValue(prg);
        }
        return prg;
    }

    protected void updateLabel() {
        if (this.lbl != null) {
            this.lbl.setText(this.getter.getLabelString());
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.ProgressInterface#getMessage()
     */
    @Override
    public String getMessage() {        
        return this.message;
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.dialog.ProgressInterface#getValue()
     */
    @Override
    public int getProgress() {
        if (getter == this) {
            return -1;
        } else {
            return this.getter.getProgress();
        }
    }
}
