package jd.http;

import org.appwork.utils.logging2.LogInterface;

public interface BrowserSettings {

    public ProxySelectorInterface getProxySelector();

    public LogInterface getLogger();

    public boolean isDebug();

    public boolean isVerbose();

    public void setProxySelector(ProxySelectorInterface proxy);

    public void setDebug(boolean b);

    public void setLogger(LogInterface logger);

    public void setVerbose(boolean b);

}
