package jd.gui.swing.jdgui.views.settings.panels.advanced;

import java.awt.AlphaComposite;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import jd.gui.swing.jdgui.JDGui;

import org.appwork.scheduler.DelayedRunnable;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTRunner;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.packagehistorycontroller.AbstractSearchHistoryManager;
import org.jdownloader.gui.packagehistorycontroller.AdvancedSettingsSearchHistoryManager;
import org.jdownloader.gui.settings.AbstractConfigPanel;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.components.HistorySearchField;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.advanced.AdvancedConfigEventListener;
import org.jdownloader.settings.advanced.AdvancedConfigManager;
import org.jdownloader.translate._JDT;

public class AdvancedSettings extends AbstractConfigPanel implements AdvancedConfigEventListener {

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
    private final HistorySearchField filterText;
    private final String          filterHelp;
    private final AdvancedTable   table;
    private final DelayedRunnable delayedRefresh;

    public String getTitle() {
        return _GUI.T.gui_settings_advanced_title();
    }

    @Override
    public String getSidebarTooltip() {
        return _GUI.T.SettingsSidebar_tooltip_Advanced();
    }

    public AdvancedSettings() {
        super();
        this.addHeader(getTitle(), NewTheme.I().getIcon(IconKey.ICON_ADVANCEDCONFIG, 32));
        this.addDescription(_JDT.T.gui_settings_advanced_description());
        filterHelp = _GUI.T.AdvancedSettings_AdvancedSettings_filter_();
        filterText = new HistorySearchField() {

            private static final long serialVersionUID = 1L;
            {
                this.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(MouseEvent e) {
                        if (StringUtils.isEmpty(getText()) && SwingUtilities.isLeftMouseButton(e)) {
                            /* Click into the empty field shows the whole history */
                            updateHistoryPopup(true);
                        }
                    }
                });
            }

            @Override
            protected AbstractSearchHistoryManager getSearchHistoryManager() {
                return AdvancedSettingsSearchHistoryManager.getInstance();
            }

            /* The search field is located at the top of the settings panel -> popup opens downwards */
            @Override
            protected boolean isHistoryPopupUpwards() {
                return false;
            }

            @Override
            protected void onSearchTextChanged() {
                /* delayedRefresh does not exist yet while the help text is set in the constructor */
                if (AdvancedSettings.this.delayedRefresh != null) {
                    AdvancedSettings.this.delayedRefresh.resetAndStart();
                }
            }

            final AbstractIcon search = new AbstractIcon(IconKey.ICON_SEARCH, 16);

            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                final Graphics2D g2 = (Graphics2D) g;
                final Composite comp = g2.getComposite();
                g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.6f));
                search.paintIcon(this, g2, 3, 3);
                g2.setComposite(comp);
            }
        };

        filterText.setHelpText(filterHelp);
        /*
         * Ctrl+F focuses the search field, same as in the download list and the linkgrabber. WHEN_IN_FOCUSED_WINDOW makes it work no
         * matter which component of this panel has the focus.
         */
        final String focusSearchKey = "focusSearch";
        getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_F, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), focusSearchKey);
        getActionMap().put(focusSearchKey, new AbstractAction() {
            private static final long serialVersionUID = 1L;

            @Override
            public void actionPerformed(ActionEvent e) {
                filterText.requestFocusInWindow();
                filterText.selectAll();
            }
        });

        // filterText.setOpaque(false);
        // filterText.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        // filterText.setBorder(null);
        filterText.setBorder(BorderFactory.createCompoundBorder(filterText.getBorder(), BorderFactory.createEmptyBorder(0, 20, 0, 0)));
        add(filterText, "gapleft " + getLeftGap() + ",spanx,growx,pushx");
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
                        final String text = filterText.getFilterText();
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

    public void onAdvancedConfigUpdate() {
        delayedRefresh.resetAndStart();
    }
}