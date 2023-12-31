package org.jdownloader.updatev2;

import java.util.Collection;
import java.util.HashSet;
import java.util.Locale;

public class InstallLog {
    private final HashSet<String> sourcePackages;

    public InstallLog() {
        modifiedDirects = new HashSet<String>();
        modifiedFiles = new HashSet<String>();
        modifiedPlugins = new HashSet<String>();
        modifiedExtensionFiles = new HashSet<String>();
        modifiedRestartRequiredFiles = new HashSet<String>();
        sourcePackages = new HashSet<String>();
    }

    public HashSet<String> getSourcePackages() {
        return sourcePackages;
    }

    public Collection<String> getModifiedFiles() {
        return modifiedFiles;
    }

    private final Collection<String> modifiedFiles;
    private final Collection<String> modifiedPlugins;
    private final Collection<String> modifiedDirects;

    public Collection<String> getModifiedPlugins() {
        return modifiedPlugins;
    }

    public Collection<String> getModifiedDirects() {
        return modifiedDirects;
    }

    public Collection<String> getModifiedRestartRequiredFiles() {
        return modifiedRestartRequiredFiles;
    }

    public Collection<String> getModifiedExtensionFiles() {
        return modifiedExtensionFiles;
    }

    private final Collection<String> modifiedRestartRequiredFiles;
    private final Collection<String> modifiedExtensionFiles;
    public static final String       FILE_EXT_JARSIGNATURE    = ".jarSignature";
    public static final String       FILE_EXT_UPDATESIGNATURE = ".updateSignature";
    public static final String       FILE_EXT_REMOVEDFILE     = ".removed";
    public static final String       FILE_EXT_JAR             = ".jar";
    public static final String       CLIENT_OPTIONS           = ".clientOptions";
    public static final String       SERVER_OPTIONS           = ".serverOptions";

    public void add(String relPath) {
        if (relPath.endsWith(FILE_EXT_UPDATESIGNATURE)) {
            return;
        } else if (relPath.endsWith(FILE_EXT_JARSIGNATURE)) {
            return;
        }
        modifiedFiles.add(relPath);
        String check = relPath;
        if (check.endsWith(CLIENT_OPTIONS)) {
            check = relPath.substring(0, relPath.length() - CLIENT_OPTIONS.length());
        } else if (check.endsWith(SERVER_OPTIONS)) {
            check = relPath.substring(0, relPath.length() - SERVER_OPTIONS.length());
        } else if (check.endsWith(FILE_EXT_REMOVEDFILE)) {
            check = relPath.substring(0, relPath.length() - FILE_EXT_REMOVEDFILE.length());
        }
        if (check.equals("build.json")) {
            modifiedDirects.add(relPath);
        } else if (check.endsWith(".lng")) {
            modifiedDirects.add(relPath);
        } else if (check.endsWith(".class") && check.toLowerCase(Locale.ENGLISH).startsWith("jd/plugins")) {
            modifiedPlugins.add(relPath);
        } else if (relPath.startsWith("extensions/")) {
            modifiedExtensionFiles.add(relPath);
            modifiedRestartRequiredFiles.add(relPath);
        } else {
            modifiedRestartRequiredFiles.add(relPath);
        }
    }

    public void merge(InstallLog installLog) {
        modifiedDirects.addAll(installLog.modifiedDirects);
        modifiedFiles.addAll(installLog.modifiedFiles);
        modifiedPlugins.addAll(installLog.modifiedPlugins);
        modifiedExtensionFiles.addAll(installLog.modifiedExtensionFiles);
        modifiedRestartRequiredFiles.addAll(installLog.modifiedRestartRequiredFiles);
    }
}
