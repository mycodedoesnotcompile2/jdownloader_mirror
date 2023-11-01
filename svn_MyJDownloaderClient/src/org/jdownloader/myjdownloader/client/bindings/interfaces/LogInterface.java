package org.jdownloader.myjdownloader.client.bindings.interfaces;

import java.util.List;

import org.jdownloader.myjdownloader.client.bindings.ClientApiNameSpace;
import org.jdownloader.myjdownloader.client.bindings.LogFolderStorable;

@ClientApiNameSpace(LogInterface.NAMESPACE)
public interface LogInterface {
    public static final String NAMESPACE = "log";

    public List<LogFolderStorable> getAvailableLogs();

    public String sendLogFile(LogFolderStorable[] logFolders);
}
