package jd.gui.swing.jdgui.views.settings.panels.anticaptcha;

import java.awt.AlphaComposite;
import java.awt.Composite;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.swing.components.ExtTextField;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.appwork.utils.swing.dialog.dimensor.RememberLastDialogDimension;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.controller.host.LazyHostPlugin;

import jd.gui.swing.dialog.HosterChooserTable;
import net.miginfocom.swing.MigLayout;

/**
 * Dialog of the captcha wizard to add a domain. It looks and works like the hoster selection of the "Add account" dialog (search field
 * above a hoster table, Up/Down in the search field moves the selection, the table is filtered while typing).
 */
public class CaptchaWizardAddDomainDialog extends AbstractDialog<Integer> {
    private final List<LazyHostPlugin> plugins;
    private ExtTextField               filter;
    private HosterChooserTable         hosterTable;
    private volatile String            domain;

    /**
     * @param plugins
     *            the hoster plugins the user can choose from
     * @return the domain the user chose, or null if the dialog was canceled
     */
    public static String showDialog(final Collection<LazyHostPlugin> plugins) {
        final CaptchaWizardAddDomainDialog dialog = new CaptchaWizardAddDomainDialog(plugins);
        try {
            Dialog.getInstance().showDialog(dialog);
        } catch (final DialogNoAnswerException e) {
            return null;
        }
        return dialog.domain;
    }

    private CaptchaWizardAddDomainDialog(final Collection<LazyHostPlugin> plugins) {
        super(Dialog.STYLE_HIDE_ICON, _GUI.T.CaptchaWizard_addDomain_title(), null, _GUI.T.lit_add(), _GUI.T.lit_cancel());
        this.plugins = new ArrayList<LazyHostPlugin>(plugins);
        setDimensor(new RememberLastDialogDimension(getClass().getSimpleName()));
    }

    @Override
    protected Integer createReturnValue() {
        return getReturnmask();
    }

    @Override
    protected int getPreferredWidth() {
        return 500;
    }

    @Override
    public JComponent layoutDialogContent() {
        filter = new ExtTextField() {
            private static final long serialVersionUID = 1L;

            @Override
            protected void paintComponent(final Graphics g) {
                super.paintComponent(g);
                final Graphics2D g2 = (Graphics2D) g;
                final Composite comp = g2.getComposite();
                g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.6f));
                new AbstractIcon(IconKey.ICON_SEARCH, 16).paintIcon(this, g2, 3, 3);
                g2.setComposite(comp);
            }
        };
        filter.setHelpText("Search Plugins");
        filter.setBorder(BorderFactory.createCompoundBorder(filter.getBorder(), BorderFactory.createEmptyBorder(0, 20, 0, 0)));
        filter.addKeyListener(new KeyListener() {
            @Override
            public void keyTyped(final KeyEvent e) {
            }

            @Override
            public void keyReleased(final KeyEvent e) {
            }

            @Override
            public void keyPressed(final KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_DOWN) {
                    hosterTable.onKeyDown();
                }
                if (e.getKeyCode() == KeyEvent.VK_UP) {
                    hosterTable.onKeyUp();
                }
            }
        });
        hosterTable = new HosterChooserTable(plugins);
        /* Double click on a hoster adds it right away. */
        hosterTable.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(final MouseEvent e) {
                if (e.getClickCount() == 2 && hosterTable.getSelectedPlugin() != null) {
                    getOkButton().doClick();
                }
            }
        });
        final DocumentListener listener = new DocumentListener() {
            private final DelayedRunnable delayedRefresh = new DelayedRunnable(200, 1000) {
                private String lastText = null;

                @Override
                public String getID() {
                    return "CaptchaWizardAddDomainDialog";
                }

                @Override
                public void delayedrun() {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            final String text = filter.getText();
                            if (!StringUtils.equals(lastText, text)) {
                                lastText = text;
                                hosterTable.refresh(text);
                            }
                        }
                    }.waitForEDT();
                }
            };

            @Override
            public void removeUpdate(final DocumentEvent e) {
                delayedRefresh.resetAndStart();
            }

            @Override
            public void insertUpdate(final DocumentEvent e) {
                delayedRefresh.resetAndStart();
            }

            @Override
            public void changedUpdate(final DocumentEvent e) {
                delayedRefresh.resetAndStart();
            }
        };
        filter.getDocument().addDocumentListener(listener);
        final JPanel content = new JPanel(new MigLayout("ins 0, wrap 1", "[grow,fill]"));
        final JLabel header = new JLabel(_GUI.T.AddAccountDialog_layoutDialogContent_choosehoster_());
        header.setFont(header.getFont().deriveFont(Font.BOLD));
        content.add(header, "pushx,growx");
        content.add(filter, "pushx,growx");
        final JScrollPane sp = new JScrollPane(hosterTable);
        sp.setFocusable(false);
        sp.getVerticalScrollBar().setFocusable(false);
        sp.getViewport().setFocusable(false);
        content.add(sp, "pushy,growy,height 250:300:");
        return content;
    }

    @Override
    protected void packed() {
        super.packed();
        filter.requestFocusInWindow();
    }

    @Override
    public void actionPerformed(final java.awt.event.ActionEvent e) {
        if (e.getSource() == getOkButton()) {
            final LazyHostPlugin selected = hosterTable.getSelectedPlugin();
            domain = selected == null ? null : selected.getHost().toLowerCase(java.util.Locale.ENGLISH);
        }
        super.actionPerformed(e);
    }
}
