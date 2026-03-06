package jd.controlling.accountchecker;

import jd.controlling.accountchecker.AccountChecker.AccountCheckJob;
import jd.http.BrowserSettingsThread;
import jd.plugins.PluginForHost;

public class AccountCheckerThread extends BrowserSettingsThread {
    protected volatile AccountCheckJob job;
    protected volatile PluginForHost   plugin;

    public AccountCheckJob getJob() {
        return job;
    }

    public PluginForHost getPlugin() {
        return plugin;
    }

    public void setPlugin(PluginForHost plugin) {
        this.plugin = plugin;
    }

    public AccountCheckerThread() {
        super();
    }

    public static boolean isForced() {
        final Thread thread = Thread.currentThread();
        if (thread instanceof AccountCheckerThread) {
            final AccountCheckJob job = ((AccountCheckerThread) thread).getJob();
            return job.isForce();
        }
        return false;
    }

}
