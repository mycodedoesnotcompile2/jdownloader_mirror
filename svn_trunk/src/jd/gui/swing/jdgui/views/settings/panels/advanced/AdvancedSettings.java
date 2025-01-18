package jd.gui.swing.jdgui.views.settings.panels.advanced;

import java.awt.AlphaComposite;
import java.awt.Composite;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import jd.gui.swing.jdgui.JDGui;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.HelpNotifier;
import org.appwork.utils.swing.HelpNotifierCallbackListener;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.settings.AbstractConfigPanel;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.advanced.AdvancedConfigEventListener;
import org.jdownloader.settings.advanced.AdvancedConfigManager;
import org.jdownloader.translate._JDT;

public class AdvancedSettings extends AbstractConfigPanel implements DocumentListener, AdvancedConfigEventListener {

    /*
     * (non-Javadoc)
     *
     * @see org.jdownloader.gui.settings.AbstractConfigPanel#onShow()
     */
    @Override
    protected void onShow() {
        super.onShow();
        AdvancedConfigManager.getInstance().getEventSender().addListener(this);
        JDGui.help(_GUI.T.AdvancedSettings_onShow_title_(), _GUI.T.AdvancedSettings_onShow_msg_(), new AbstractIcon(IconKey.ICON_WARNING, 32));

    }

    /*
     * (non-Javadoc)
     *
     * @see org.jdownloader.gui.settings.AbstractConfigPanel#onHide()
     */
    @Override
    protected void onHide() {
        super.onHide();
        AdvancedConfigManager.getInstance().getEventSender().removeListener(this);
    }

    private static final long     serialVersionUID = 1L;
    private final JTextField      filterText;
    private final String          filterHelp;
    private final AdvancedTable   table;
    private final DelayedRunnable delayedRefresh;

    public String getTitle() {
        return _GUI.T.gui_settings_advanced_title();
    }

    public AdvancedSettings() {
        super();
        this.addHeader(getTitle(), NewTheme.I().getIcon(IconKey.ICON_ADVANCEDCONFIG, 32));
        this.addDescription(_JDT.T.gui_settings_advanced_description());
        filterHelp = _GUI.T.AdvancedSettings_AdvancedSettings_filter_();
        filterText = new JTextField() {

            private static final long serialVersionUID = 1L;
            private boolean           drawReset        = false;

            {
                this.getDocument().addDocumentListener(new DocumentListener() {

                    private boolean hasText() {
                        final String text = getText();
                        return text != null && text.length() > 0 && !text.equals(filterHelp);
                    }

                    @Override
                    public void removeUpdate(DocumentEvent e) {
                        drawReset = hasText();
                    }

                    @Override
                    public void insertUpdate(DocumentEvent e) {
                        drawReset = hasText();
                    }

                    @Override
                    public void changedUpdate(DocumentEvent e) {
                        drawReset = hasText();
                    }
                });
                this.addMouseMotionListener(new MouseMotionListener() {

                    @Override
                    public void mouseMoved(MouseEvent e) {
                        if (!hasFocus()) {
                            return;
                        }
                        if (drawReset && closeXPos > 0 && e.getX() > closeXPos) {
                            setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
                            setCaretColor(getBackground());
                        } else {
                            setCursor(Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR));
                            setCaretColor(null);
                        }
                    }

                    @Override
                    public void mouseDragged(MouseEvent e) {
                    }
                });

                this.addMouseListener(new MouseListener() {

                    @Override
                    public void mouseReleased(MouseEvent e) {
                    }

                    @Override
                    public void mousePressed(MouseEvent e) {
                    }

                    @Override
                    public void mouseExited(MouseEvent e) {
                    }

                    @Override
                    public void mouseEntered(MouseEvent e) {
                    }

                    @Override
                    public void mouseClicked(MouseEvent e) {
                        if (drawReset && closeXPos > 0 && e.getX() > closeXPos) {
                            setText("");
                        }
                    }
                });
            }

            final Image               close            = NewTheme.I().getImage(IconKey.ICON_CLOSE, -1);
            final AbstractIcon        search           = new AbstractIcon(IconKey.ICON_SEARCH, 16);
            private int               closeXPos        = -1;

            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                final Graphics2D g2 = (Graphics2D) g;
                final Composite comp = g2.getComposite();
                g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.6f));
                search.paintIcon(this, g2, 3, 3);
                g2.setComposite(comp);
                if (drawReset) {
                    closeXPos = getWidth() - close.getWidth(null) - (getHeight() - close.getHeight(null)) / 2;
                    g2.drawImage(close, closeXPos, (getHeight() - close.getHeight(null)) / 2, close.getWidth(null), close.getHeight(null), null);
                }
            }

        };

        HelpNotifier.register(filterText, new HelpNotifierCallbackListener() {

            public void onHelpNotifyShown(JComponent c) {
            }

            public void onHelpNotifyHidden(JComponent c) {
            }
        }, filterHelp);

        // filterText.setOpaque(false);
        // filterText.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        // filterText.setBorder(null);
        filterText.setBorder(BorderFactory.createCompoundBorder(filterText.getBorder(), BorderFactory.createEmptyBorder(0, 20, 0, 0)));
        add(filterText, "gapleft " + getLeftGap() + ",spanx,growx,pushx");
        filterText.getDocument().addDocumentListener(this);
        add(new JScrollPane(table = new AdvancedTable()));
        delayedRefresh = new DelayedRunnable(200, 1000) {

            @Override
            public String getID() {
                return "AdvancedSettings";
            }

            @Override
            public void delayedrun() {
                new EDTRunner() {

                    @Override
                    protected void runInEDT() {
                        final String text = filterText.getText();
                        if (text != null && !text.equals(filterHelp)) {
                            table.filter(text);
                        } else {
                            table.filter(null);
                        }
                    }
                };
            }

        };
    }

    @Override
    public Icon getIcon() {
        return NewTheme.I().getIcon(IconKey.ICON_ADVANCEDCONFIG, 20);
    }

    @Override
    public void save() {

    }

    @Override
    public void updateContents() {
        delayedRefresh.resetAndStart();
    }

    public void insertUpdate(DocumentEvent e) {
        delayedRefresh.resetAndStart();
    }

    public void removeUpdate(DocumentEvent e) {
        delayedRefresh.resetAndStart();
    }

    public void changedUpdate(DocumentEvent e) {
        delayedRefresh.resetAndStart();
    }

    public void onAdvancedConfigUpdate() {
        delayedRefresh.resetAndStart();
    }
}