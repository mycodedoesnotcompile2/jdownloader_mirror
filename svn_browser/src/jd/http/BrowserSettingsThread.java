package jd.http;

import org.appwork.utils.logging2.LogInterface;

public class BrowserSettingsThread extends Thread implements BrowserSettings {

    private ProxySelectorInterface proxySelector;
    private boolean                debug;
    private boolean                verbose;
    protected LogInterface         logger;

    public BrowserSettingsThread() {
        this.copySettings();
    }

    public BrowserSettingsThread(ThreadGroup group, Runnable target, String name) {
        super(group, target, name);
        this.copySettings();
    }

    public BrowserSettingsThread(ThreadGroup group, Runnable target) {
        super(group, target);
        this.copySettings();
    }

    public BrowserSettingsThread(ThreadGroup group, String name) {
        super(group, name);
        this.copySettings();
    }

    public BrowserSettingsThread(final Runnable r) {
        super(r);
        this.copySettings();
    }

    public BrowserSettingsThread(final Runnable r, final String name) {
        super(r, name);
        this.copySettings();
    }

    public BrowserSettingsThread(final String name) {
        super(name);
        this.copySettings();
    }

    private void copySettings() {
        final Thread currentThread = Thread.currentThread();
        /**
         * use BrowserSettings from current thread if available
         */
        if (currentThread != null && currentThread instanceof BrowserSettings) {
            final BrowserSettings settings = (BrowserSettings) currentThread;
            this.proxySelector = settings.getProxySelector();
            this.debug = settings.isDebug();
            this.verbose = settings.isVerbose();
            this.logger = settings.getLogger();
        }
    }

    public static ProxySelectorInterface getThreadProxySelector() {
        final Thread currentThread = Thread.currentThread();
        /**
         * use BrowserSettings from current thread if available
         */
        if (currentThread != null && currentThread instanceof BrowserSettings) {
            final BrowserSettings settings = (BrowserSettings) currentThread;
            return settings.getProxySelector();
        }
        return null;
    }

    @Override
    public ProxySelectorInterface getProxySelector() {
        return this.proxySelector;
    }

    @Override
    public LogInterface getLogger() {
        return this.logger;
    }

    @Override
    public boolean isDebug() {
        return this.debug;
    }

    @Override
    public boolean isVerbose() {
        return this.verbose;
    }

    @Override
    public void setProxySelector(final ProxySelectorInterface proxy) {
        this.proxySelector = proxy;
    }

    @Override
    public void setDebug(final boolean b) {
        this.debug = b;
    }

    @Override
    public void setLogger(final LogInterface logger) {
        this.logger = logger;
    }

    @Override
    public void setVerbose(final boolean b) {
        this.verbose = b;
    }

}
