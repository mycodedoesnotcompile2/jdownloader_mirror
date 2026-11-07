package org.jdownloader.gui.packagehistorycontroller;

import java.util.List;

import org.jdownloader.settings.staticreferences.CFG_GENERAL;

/**
 * History of search terms of the search field in the advanced settings.
 */
public class AdvancedSettingsSearchHistoryManager extends AbstractSearchHistoryManager {
    private static final AdvancedSettingsSearchHistoryManager INSTANCE = new AdvancedSettingsSearchHistoryManager();

    /**
     * get the only existing instance of AdvancedSettingsSearchHistoryManager. This is a singleton
     *
     * @return
     */
    public static AdvancedSettingsSearchHistoryManager getInstance() {
        return AdvancedSettingsSearchHistoryManager.INSTANCE;
    }

    /**
     * Create a new instance of AdvancedSettingsSearchHistoryManager. This is a singleton class. Access the only existing instance by
     * using {@link #getInstance()}.
     */
    private AdvancedSettingsSearchHistoryManager() {
        super(CFG_GENERAL.CFG.getAdvancedSettingsSearchHistory(), CFG_GENERAL.ADVANCED_SETTINGS_SEARCH_HISTORY);
    }

    @Override
    protected int getMaxLength() {
        return CFG_GENERAL.CFG.getAdvancedSettingsSearchHistoryLength();
    }

    @Override
    protected void saveToConfig(List<SearchHistoryEntry> list) {
        CFG_GENERAL.CFG.setAdvancedSettingsSearchHistory(list);
    }
}
