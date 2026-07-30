package jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.editdialog;

import java.awt.Component;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtCheckBox;
import org.appwork.utils.DebugMode;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.dimensor.RememberLastDialogDimension;
import org.appwork.utils.swing.dialog.locator.RememberAbsoluteDialogLocator;
import org.jdownloader.controlling.filter.LinkFilterController;
import org.jdownloader.controlling.filter.LinkgrabberFilterRule;
import org.jdownloader.gui.translate._GUI;

import jd.controlling.linkcrawler.CrawledLink;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.test.TestWaitDialog;
import jd.gui.swing.jdgui.views.settings.panels.linkgrabberfilter.test.ViewTestResultTableModel;
import jd.gui.swing.laf.LookAndFeelController;

public class ExceptionsRuleDialog extends ConditionDialog<LinkgrabberFilterRule> {
    private LinkgrabberFilterRule rule;

    public ExceptionsRuleDialog(LinkgrabberFilterRule filterRule) {
        super();
        this.rule = filterRule;
        setTitle(_GUI.T.ExceptionsRuleDialog_ExceptionsRuleDialog_title_());
        setLocator(new RememberAbsoluteDialogLocator(getClass().getSimpleName()));
        setDimensor(new RememberLastDialogDimension(getClass().getSimpleName()));
    }

    protected void runTest(String text) {
        TestWaitDialog d;
        try {
            LinkFilterController lfc = LinkFilterController.createEmptyTestInstance();
            LinkgrabberFilterRule rule = getCurrentCopy();
            rule.setEnabled(true);
            lfc.add(rule);
            java.util.List<CrawledLink> ret = Dialog.getInstance().showDialog(d = new TestWaitDialog(text, _GUI.T.FilterRuleDialog_runTest_title_(rule.toString()), lfc) {
                @Override
                protected ExtTableModel<CrawledLink> createTableModel() {
                    return new ViewTestResultTableModel();
                }
            });
        } catch (DialogClosedException e) {
            e.printStackTrace();
        } catch (DialogCanceledException e) {
            e.printStackTrace();
        }
    }

    /**
     * Returns a Linkgrabberfilter representing current settings. does NOT save the original one
     *
     * @return
     */
    private LinkgrabberFilterRule getCurrentCopy() {
        LinkgrabberFilterRule ret = this.rule.duplicate();
        save(ret);
        return ret;
    }

    public static void main(String[] args) {
        try {
            LookAndFeelController.getInstance().setUIManager();
            Dialog.getInstance().showDialog(new ExceptionsRuleDialog(new LinkgrabberFilterRule()));
        } catch (DialogClosedException e) {
            e.printStackTrace();
        } catch (DialogCanceledException e) {
            e.printStackTrace();
        }
    }

    @Override
    protected LinkgrabberFilterRule createReturnValue() {
        return rule;
    }

    @Override
    protected void setReturnmask(boolean b) {
        super.setReturnmask(b);
        if (b) {
            save(this.rule);
        }
    }

    private void save(LinkgrabberFilterRule rule) {
        rule.setPackagenameFilter(getPackagenameFilter());
        rule.setCommentFilter(getCommentFilter());
        rule.setFilenameFilter(getFilenameFilter());
        rule.setHosterURLFilter(getHosterFilter());
        rule.setName(getName());
        rule.setFilesizeFilter(getFilersizeFilter());
        rule.setSourceURLFilter(getSourceFilter());
        rule.setOriginFilter(getOriginFilter());
        rule.setLinkEnabledFilter(getLinkEnabledFilter());
        rule.setDownloadListDupeFilter(getDownloadListDupeFilter());
        rule.setFiletypeFilter(getFiletypeFilter());
        rule.setOnlineStatusFilter(getOnlineStatusFilter());
        rule.setPluginStatusFilter(getPluginStatusFilter());
        rule.setAccept(true);
        if (cbNegate != null) {
            // only overwrite when the checkbox is actually present (IDE/debug); otherwise keep the stored value untouched so a
            // negated rule created in the IDE is not silently reset to false when saved from a release build.
            rule.setNegated(isNegated());
        }
        rule.setTestUrl(getTxtTestUrl());
        rule.setIconKey(getIconKey());
    }

    @Override
    protected void addNegateGui(JComponent panel) {
        // The negate/invert feature is still experimental: only expose the checkbox when running from the IDE. The underlying
        // "negated" flag stays fully functional in release builds (evaluated in the view path), it just cannot be toggled via GUI.
        if (!DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
            return;
        }
        cbNegate = new ExtCheckBox();
        panel.add(cbNegate);
        panel.add(new JLabel(_GUI.T.ExceptionsRuleDialog_negate_label()), "spanx,growx,pushx");
    }

    private void updateGUI() {
        regexFields.clear();
        setIconKey(rule.getIconKey());
        setPackagenameFilter(rule.getPackagenameFilter());
        setCommentFilter(rule.getCommentFilter());
        setFilenameFilter(rule.getFilenameFilter());
        setHosterFilter(rule.getHosterURLFilter());
        setName(rule.getName());
        txtTestUrl.setText(rule.getTestUrl());
        setFilesizeFilter(rule.getFilesizeFilter());
        setOriginFilter(rule.getOriginFilter());
        setLinkEnabledFilter(rule.getLinkEnabledFilter());
        setDownloadListDupeFilter(rule.getDownloadListDupeFilter());
        setOnlineStatusFilter(rule.getOnlineStatusFilter());
        setPluginStatusFilter(rule.getPluginStatusFilter());
        setSourceFilter(rule.getSourceURLFilter());
        setFiletypeFilter(rule.getFiletypeFilter());
        setNegated(rule.isNegated());
    }

    protected String getIfText() {
        return _GUI.T.ExceptionsRuleDialog_getIfText_();
    }

    private void disable(JComponent ret) {
        ret.setEnabled(false);
        for (Component c : ret.getComponents()) {
            if (c instanceof JComponent) {
                disable((JComponent) c);
            }
        }
    }

    @Override
    public JComponent layoutDialogContent() {
        MigPanel ret = (MigPanel) super.layoutDialogContent();
        // ret.add(createHeader(_GUI.T.ExceptionsRuleDialog_layoutDialogContent_then()),
        // "gaptop 10, spanx,growx,pushx");
        updateGUI();
        if (rule.isStaticRule()) {
            okButton.setEnabled(false);
            okButton.setText(_GUI.T.PackagizerFilterRuleDialog_layoutDialogContent_cannot_modify_());
            disable(ret);
        }
        JScrollPane sp = new JScrollPane(ret);
        sp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        sp.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        sp.setBorder(null);
        return sp;
    }
}
