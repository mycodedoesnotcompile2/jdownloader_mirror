package org.jdownloader.gui.packagehistorycontroller;

import java.util.List;

import org.jdownloader.settings.staticreferences.CFG_GENERAL;

/**
 * History of search terms, shared by the search fields of the download and linkgrabber tab.
 */
public class SearchHistoryManager extends AbstractSearchHistoryManager {
    private static final SearchHistoryManager INSTANCE = new SearchHistoryManager();

    /**
     * get the only existing instance of SearchHistoryManager. This is a singleton
     *
     * @return
     */
    public static SearchHistoryManager getInstance() {
        return SearchHistoryManager.INSTANCE;
    }

    /**
     * Create a new instance of SearchHistoryManager. This is a singleton class. Access the only existing instance by using
     * {@link #getInstance()}.
     */
    private SearchHistoryManager() {
        super(CFG_GENERAL.CFG.getSearchHistory(), CFG_GENERAL.SEARCH_HISTORY);
    }

    @Override
    protected int getMaxLength() {
        return CFG_GENERAL.CFG.getSearchHistoryLength();
    }

    @Override
    protected void saveToConfig(List<SearchHistoryEntry> list) {
        CFG_GENERAL.CFG.setSearchHistory(list);
    }
}
