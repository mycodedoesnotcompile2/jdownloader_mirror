package jd.gui.swing.jdgui.views.settings.panels.accountmanager;

import javax.swing.AbstractAction;

import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtButton;
import org.appwork.swing.exttable.utils.MinimumSelectionObserver;
import org.jdownloader.gui.IconKey;
import org.jdownloader.images.AbstractIcon;

import jd.gui.swing.jdgui.interfaces.SwitchPanel;
import jd.plugins.Account;
import net.miginfocom.swing.MigLayout;

public class AccountListPanel extends SwitchPanel {
    private MigPanel            tb;
    private PremiumAccountTable table;

    public AccountListPanel(AccountManager accountManager) {
        super(new MigLayout("ins 0,wrap 1", "[grow,fill]", "[grow,fill][]"));
        table = new PremiumAccountTable(this);
        tb = new MigPanel("ins 0", "[][][][][grow,fill]", "");
        tb.setOpaque(false);
        NewAction na;
        tb.add(new ExtButton(na = new NewAction()), "sg 1,height 26!");
        na.putValue(AbstractAction.SMALL_ICON, new AbstractIcon(IconKey.ICON_ADD, 20));
        RemoveAction ra;
        tb.add(new ExtButton(ra = new RemoveAction(table)), "sg 1,height 26!");
        table.getSelectionModel().addListSelectionListener(new MinimumSelectionObserver(table, ra, 1));
        tb.add(new ExtButton(new BuyAction(table)), "sg 2,height 26!");
        final RefreshAction refreshAction = new RefreshAction();
        tb.add(new ExtButton(refreshAction), "sg 2,height 26!");
        add(table.createScrollPane());
        add(tb);
    }

    @Override
    protected void onShow() {
    }

    @Override
    protected void onHide() {
    }

    public void selectAccount(Account account) {
        for (AccountEntry ae : table.getModel().getTableData()) {
            if (account.equals(ae.getAccount())) {
                table.getModel().setSelectedObject(ae);
            }
        }
    }
}
