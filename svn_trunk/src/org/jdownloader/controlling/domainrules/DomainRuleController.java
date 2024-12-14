package org.jdownloader.controlling.domainrules;

import java.util.ArrayList;
import java.util.List;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.uio.ExceptionDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.swing.dialog.ExceptionDialog;
import org.jdownloader.controlling.domainrules.event.DomainRuleControllerEvent;
import org.jdownloader.controlling.domainrules.event.DomainRuleControllerEventSender;
import org.jdownloader.controlling.domainrules.event.DomainRuleControllerListener;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;

import jd.plugins.Account;

public class DomainRuleController implements GenericConfigEventListener<Object> {
    private static final DomainRuleController INSTANCE = new DomainRuleController();

    /**
     * get the only existing instance of DomainRuleController. This is a singleton
     *
     * @return
     */
    public static DomainRuleController getInstance() {
        return DomainRuleController.INSTANCE;
    }

    private List<CompiledDomainRule>              rules;
    final private DomainRuleControllerEventSender eventSender;
    private int                                   maxSimultaneousDownloads;

    /**
     * Create a new instance of DomainRuleController. This is a singleton class. Access the only existing instance by using
     * {@link #getInstance()}.
     */
    private DomainRuleController() {
        eventSender = new DomainRuleControllerEventSender();
        CFG_GENERAL.DOMAIN_RULES.getEventSender().addListener(this);
        update();
    }

    public DomainRuleControllerEventSender getEventSender() {
        return eventSender;
    }

    @Override
    public void onConfigValidatorError(KeyHandler<Object> keyHandler, Object invalidValue, ValidationException validateException) {
    }

    @Override
    public void onConfigValueModified(KeyHandler<Object> keyHandler, Object newValue) {
        update();
    }

    private void update() {
        final List<DomainRule> rules = CFG_GENERAL.CFG.getDomainRules();
        final ArrayList<CompiledDomainRule> newList = new ArrayList<CompiledDomainRule>();
        int maxDownloads = 0;
        if (rules != null) {
            for (final DomainRule dr : rules) {
                if (dr == null) {
                    continue;
                } else if (!dr.isEnabled()) {
                    continue;
                }
                try {
                    newList.add(new CompiledDomainRule(dr));
                    if (dr.isAllowToExceedTheGlobalLimit()) {
                        maxDownloads = Math.max(maxDownloads, dr.getMaxSimultanDownloads());
                    }
                } catch (Throwable e) {
                    UIOManager.I().show(ExceptionDialogInterface.class, new ExceptionDialog(UIOManager.BUTTONS_HIDE_CANCEL, _GUI.T.lit_error_occured(), e.getMessage() + "\r\n" + JSonStorage.toString(dr), e, _GUI.T.lit_close(), null));
                }
            }
        }
        this.rules = newList;
        this.maxSimultaneousDownloads = maxDownloads;
        eventSender.fireEvent(new DomainRuleControllerEvent() {
            @Override
            public void sendTo(DomainRuleControllerListener listener) {
                listener.onDomainRulesUpdated();
            }
        });
    }
    // public int getMaxSimulanDownloadsbyDomain(String candidateLinkHost) {
    // int max = 0;
    //
    // for (CompiledDomainRule rule : rules) {
    //
    // }
    // return 0;
    // }

    public int getMaxSimultaneousDownloads() {
        return maxSimultaneousDownloads;
    }

    public DomainRuleSet createRuleSet(final Account account, final String downloadDomain, final String pluginDomain, final String name) {
        final DomainRuleSet set = new DomainRuleSet();
        for (final CompiledDomainRule rule : rules) {
            if (rule.matches(account, downloadDomain, pluginDomain, name)) {
                set.add(rule);
            }
        }
        return set;
    }
}
