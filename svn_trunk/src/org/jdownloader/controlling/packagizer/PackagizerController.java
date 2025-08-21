package org.jdownloader.controlling.packagizer;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownEvent;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.ConfigEvent;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.IO;
import org.appwork.utils.ModifyLock;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.encoding.URLEncode;
import org.appwork.utils.event.EventSuppressor;
import org.appwork.utils.event.queue.QueueAction;
import org.appwork.utils.logging2.LogSource;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.controlling.FileCreationEvent;
import org.jdownloader.controlling.FileCreationListener;
import org.jdownloader.controlling.FileCreationManager;
import org.jdownloader.controlling.packagizer.PackagizerControllerListener.STATE;
import org.jdownloader.extensions.extraction.ArchiveFile;
import org.jdownloader.extensions.extraction.BooleanStatus;
import org.jdownloader.extensions.extraction.ExtractionController;
import org.jdownloader.extensions.extraction.bindings.downloadlink.DownloadLinkArchive;
import org.jdownloader.extensions.extraction.bindings.downloadlink.DownloadLinkArchiveFile;
import org.jdownloader.logging.LogController;
import org.jdownloader.settings.staticreferences.CFG_GENERAL;

import jd.controlling.TaskQueue;
import jd.controlling.linkcollector.LinkCollectingJob;
import jd.controlling.linkcollector.LinknameCleaner;
import jd.controlling.linkcollector.PackagizerInterface;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;
import jd.controlling.linkcrawler.PackageInfo;
import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;
import jd.plugins.ParsedFilename;

public class PackagizerController implements PackagizerInterface, FileCreationListener {
    public static final HashMap<String, Object> GLOBAL_PROPERTIES = new HashMap<String, Object>();

    public static Object getGlobalProperty(final String key) {
        synchronized (GLOBAL_PROPERTIES) {
            return GLOBAL_PROPERTIES.get(key);
        }
    }

    public static Object putGlobalProperty(final String key, final Object value) {
        synchronized (GLOBAL_PROPERTIES) {
            return GLOBAL_PROPERTIES.put(key, value);
        }
    }

    private final PackagizerSettings              config;
    private volatile ArrayList<PackagizerRule>    list                       = new ArrayList<PackagizerRule>();
    private final PackagizerControllerEventSender eventSender;
    private volatile List<PackagizerRuleWrapper>  rules                      = null;
    public static final String                    ORGFILENAME                = "orgfilename";
    public static final String                    ORGFILENAMEWITHOUTEXT      = "orgfilenamewithoutext";
    public static final String                    ORGFILETYPE                = "orgfiletype";
    public static final String                    HOSTER                     = "hoster";
    public static final String                    SOURCE                     = "source";
    public static final String                    ENV                        = "env";
    public static final String                    PACKAGENAME                = "packagename";
    public static final String                    SIMPLEDATE                 = "simpledate";
    public static final String                    INDEXOF                    = "indexof";
    private final static PackagizerReplacer       DATER_REPLACER             = new PackagizerReplacer() {
                                                                                 public String getID() {
                                                                                     return SIMPLEDATE;
                                                                                 }

                                                                                 public String replace(REPLACEVARIABLE replaceVariable, String modifiers, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                                                                                     if (StringUtils.isNotEmpty(modifiers)) {
                                                                                         final String dateString = new SimpleDateFormat(modifiers).format(new Date());
                                                                                         return Pattern.compile("<jd:simpledate:" + Pattern.quote(modifiers) + "/?>").matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, dateString)));
                                                                                     }
                                                                                     return input;
                                                                                 }
                                                                             };
    private final static PackagizerReplacer       ENV_REPLACER               = new PackagizerReplacer() {
                                                                                 private final Map<String, String> env = System.getenv();

                                                                                 public String getID() {
                                                                                     return ENV;
                                                                                 }

                                                                                 public String replace(REPLACEVARIABLE replaceVariable, String modifiers, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                                                                                     if (StringUtils.isNotEmpty(modifiers)) {
                                                                                         String value = env.get(modifiers);
                                                                                         if (value == null) {
                                                                                             for (Entry<String, String> entry : env.entrySet()) {
                                                                                                 if (StringUtils.containsIgnoreCase(entry.getKey(), modifiers)) {
                                                                                                     value = entry.getValue();
                                                                                                     break;
                                                                                                 }
                                                                                             }
                                                                                         }
                                                                                         return Pattern.compile("<jd:env:" + Pattern.quote(modifiers) + "/?>").matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, StringUtils.valueOrEmpty(value))));
                                                                                     }
                                                                                     return input;
                                                                                 }
                                                                             };
    private final static PackagizerReplacer       SUBFOLDERBYPLUGIN_REPLACER = new PackagizerReplacer() {
                                                                                 private final Pattern pat = Pattern.compile("<jd:" + DownloadLink.RELATIVE_DOWNLOAD_FOLDER_PATH + "/?>");

                                                                                 public String replace(REPLACEVARIABLE replaceVariable, String modifiers, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                                                                                     String subFolder = null;
                                                                                     final DownloadLink dlLink = link.getDownloadLink();
                                                                                     if (dlLink != null) {
                                                                                         Object subFolderByPlugin = dlLink.getProperty(DownloadLink.RELATIVE_DOWNLOAD_FOLDER_PATH);
                                                                                         if (subFolderByPlugin != null && subFolderByPlugin instanceof String) {
                                                                                             final String pathParts[] = ((String) subFolderByPlugin).split("/");
                                                                                             final StringBuilder sb = new StringBuilder();
                                                                                             for (String pathPart : pathParts) {
                                                                                                 if (sb.length() > 0) {
                                                                                                     sb.append("/");
                                                                                                 }
                                                                                                 pathPart = preprocessReplacement(replaceVariable, pathPart);
                                                                                                 if (StringUtils.isNotEmpty(pathPart)) {
                                                                                                     sb.append(pathPart);
                                                                                                 }
                                                                                             }
                                                                                             subFolder = sb.toString();
                                                                                             if (CrossSystem.isAbsolutePath(subFolder)) {
                                                                                                 subFolder = null;
                                                                                             }
                                                                                         }
                                                                                     }
                                                                                     if (StringUtils.isEmpty(subFolder)) {
                                                                                         return pat.matcher(input).replaceAll(Matcher.quoteReplacement(""));
                                                                                     } else {
                                                                                         return pat.matcher(input).replaceAll(Matcher.quoteReplacement(subFolder));
                                                                                     }
                                                                                 }

                                                                                 public String getID() {
                                                                                     return DownloadLink.RELATIVE_DOWNLOAD_FOLDER_PATH;
                                                                                 }
                                                                             };
    private static final PackagizerController     INSTANCE                   = new PackagizerController(false);
    public static final String                    ORGPACKAGENAME             = "orgpackagename";
    private HashMap<String, PackagizerReplacer>   replacers                  = new HashMap<String, PackagizerReplacer>();
    private final boolean                         testInstance;
    private final KeyHandler<Object>              ruleListHandler;

    public static PackagizerController getInstance() {
        return INSTANCE;
    }

    public static PackagizerController createEmptyTestInstance() {
        return new PackagizerController(true);
    }

    public boolean isTestInstance() {
        return testInstance;
    }

    private synchronized ArrayList<PackagizerRule> readConfig() {
        ArrayList<PackagizerRule> list = new ArrayList<PackagizerRule>();
        if (config == null) {
            return list;
        } else {
            try {
                list = config.getRuleList();
            } catch (Throwable e) {
                // restoring list may fail.
                LogController.CL(false).log(e);
            }
            final int sizeLoaded = list != null ? list.size() : 0;
            list = addDefaultRules(list);
            final int sizeNow = list != null ? list.size() : 0;
            if (sizeLoaded != sizeNow) {
                save(list);
            }
            return list;
        }
    }

    private ArrayList<PackagizerRule> addDefaultRules(List<PackagizerRule> list) {
        if (list == null) {
            list = new ArrayList<PackagizerRule>();
        }
        ArrayList<PackagizerRule> ret = new ArrayList<PackagizerRule>();
        HashSet<String> dupefinder = new HashSet<String>();
        DisableRevFilesPackageRule disableRevFilesRule = null;
        SubFolderByPackageRule subFolderByPackgeRule = null;
        SubFolderByPluginRule subFolderByPluginRule = null;
        DisableParFilesPackageRule disableParFilesRule = null;
        for (PackagizerRule rule : list) {
            final PackagizerRule clone = JSonStorage.restoreFromString(JSonStorage.serializeToJson(rule), new TypeRef<PackagizerRule>() {
            });
            clone.setCreated(-1);
            if (!dupefinder.add(JSonStorage.serializeToJson(clone))) {
                //
                continue;
            }
            if (SubFolderByPackageRule.ID.equals(rule.getId())) {
                if (dupefinder.add(rule.getId())) {
                    final SubFolderByPackageRule r = new SubFolderByPackageRule();
                    ret.add(r);
                    r.init();
                    r.setEnabled(rule.isEnabled());
                    subFolderByPackgeRule = r;
                }
            } else if (SubFolderByPluginRule.ID.equals(rule.getId())) {
                if (dupefinder.add(rule.getId())) {
                    final SubFolderByPluginRule r = new SubFolderByPluginRule();
                    ret.add(r);
                    r.init();
                    r.setEnabled(rule.isEnabled());
                    subFolderByPluginRule = r;
                }
            } else if (DisableRevFilesPackageRule.ID.equals(rule.getId())) {
                if (dupefinder.add(rule.getId())) {
                    final DisableRevFilesPackageRule r = new DisableRevFilesPackageRule();
                    ret.add(r);
                    r.init();
                    r.setEnabled(rule.isEnabled());
                    disableRevFilesRule = r;
                }
            } else if (DisableParFilesPackageRule.ID.equals(rule.getId())) {
                if (dupefinder.add(rule.getId())) {
                    final DisableParFilesPackageRule r = new DisableParFilesPackageRule();
                    ret.add(r);
                    r.init();
                    r.setEnabled(rule.isEnabled());
                    disableParFilesRule = r;
                }
            } else {
                ret.add(rule);
            }
        }
        if (disableRevFilesRule == null) {
            ret.add(disableRevFilesRule = new DisableRevFilesPackageRule());
            disableRevFilesRule.init();
        }
        if (disableParFilesRule == null) {
            ret.add(disableParFilesRule = new DisableParFilesPackageRule());
            disableParFilesRule.init();
        }
        if (subFolderByPackgeRule == null) {
            ret.add(subFolderByPackgeRule = new SubFolderByPackageRule());
            subFolderByPackgeRule.init();
        }
        if (subFolderByPluginRule == null) {
            ret.add(subFolderByPluginRule = new SubFolderByPluginRule());
            subFolderByPluginRule.init();
        }
        return ret;
    }

    public PackagizerController(boolean testInstance) {
        this.testInstance = testInstance;
        eventSender = new PackagizerControllerEventSender();
        if (!isTestInstance()) {
            config = JsonConfig.create(PackagizerSettings.class);
            ruleListHandler = config._getStorageHandler().getKeyHandler("RuleList");
        } else {
            ruleListHandler = null;
            config = null;
        }
        if (!isTestInstance()) {
            list = readConfig();
        }
        update();
        if (!isTestInstance()) {
            ruleListHandler.getEventSender().addListener(new GenericConfigEventListener<Object>() {
                @Override
                public void onConfigValueModified(KeyHandler<Object> keyHandler, Object newValue) {
                    if (newValue == null) {
                        setList(null);
                    } else {
                        setList((List<PackagizerRule>) newValue);
                    }
                }

                @Override
                public void onConfigValidatorError(KeyHandler<Object> keyHandler, Object invalidValue, ValidationException validateException) {
                }
            });
            ShutdownController.getInstance().addShutdownEvent(new ShutdownEvent() {
                @Override
                public void onShutdown(final ShutdownRequest shutdownRequest) {
                    synchronized (PackagizerController.this) {
                        save(list);
                    }
                }

                @Override
                public long getMaxDuration() {
                    return 0;
                }

                @Override
                public String toString() {
                    return "save packagizer...";
                }
            });
            FileCreationManager.getInstance().getEventSender().addListener(this);
        }
        addReplacer(ENV_REPLACER);
        addReplacer(DATER_REPLACER);
        addReplacer(new PackagizerReplacer() {
            public String getID() {
                return SOURCE;
            }

            public String replace(REPLACEVARIABLE replaceVariable, String modifiers, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                if (StringUtils.isEmpty(modifiers)) {
                    return input;
                }
                final int id = Integer.parseInt(modifiers);
                String output = input;
                // the i counter allows us to write regular expressions that address a certain line only.
                final String pattern = lgr.getSourceRule()._getPattern().pattern();
                final boolean indexed = pattern.matches("^\\-?\\d+\\\\\\. .+");
                final boolean inverted = indexed && pattern.startsWith("-");
                final String[] sources;
                if (link.getSourceUrls() != null) {
                    sources = link.getSourceUrls();
                } else {
                    /* the first link never has sourceURLs */
                    sources = new String[2];
                    sources[0] = link.getURL();
                    LinkCollectingJob job = link.getSourceJob();
                    if (job != null) {
                        sources[1] = job.getCustomSourceUrl();
                    }
                }
                int i = 1;
                for (int j = inverted ? 0 : sources.length - 1; (inverted ? (j < sources.length) : (j >= 0)); j = (inverted ? (j + 1) : (j - 1))) {
                    final String s = sources[j];
                    if (s == null) {
                        continue;
                    }
                    final String toMatch = indexed ? (inverted ? "-" : "") + (i++) + ". " + s : s;
                    Regex regex = new Regex(toMatch, lgr.getSourceRule()._getPattern());
                    String[] values = null;
                    if (regex.matches()) {
                        values = regex.getRow(0);
                    } else {
                        regex = new Regex(s, lgr.getSourceRule()._getPattern());
                        if (regex.matches()) {
                            values = regex.getRow(0);
                        }
                    }
                    if (values != null && values.length > (id - 1)) {
                        final String value = URLEncode.decodeURIComponent(StringUtils.valueOrEmpty(values[id - 1]));
                        output = Pattern.compile("<jd:source:" + Pattern.quote(String.valueOf(id)) + "\\s*/?\\s*>").matcher(output).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, value)));
                    }
                }
                return output;
            }
        });
        addReplacer(new PackagizerReplacer() {
            private final Pattern pat = Pattern.compile("<jd:" + ORGFILENAME + "\\s*/?\\s*>");

            public String replace(REPLACEVARIABLE replaceVariable, String modifiers, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                final String name = link.getName();
                if (StringUtils.isNotEmpty(modifiers)) {
                    final Pattern pattern = lgr.getFileNameRule()._getPattern();
                    final String rep = StringUtils.valueOrEmpty(new Regex(name, pattern).getMatch(Integer.parseInt(modifiers) - 1));
                    return Pattern.compile("<jd:" + ORGFILENAME + ":" + Pattern.quote(modifiers) + "\\s*/?\\s*>").matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, rep)));
                } else {
                    return pat.matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, name)));
                }
            }

            public String getID() {
                return ORGFILENAME;
            }
        });
        addReplacer(new PackagizerReplacer() {
            private final Pattern pat = Pattern.compile("<jd:" + ORGPACKAGENAME + "\\s*/?\\s*>");

            public String replace(REPLACEVARIABLE replaceVariable, String modifiers, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                String packagename = null;
                if (link != null) {
                    final CrawledPackage parentNode = link.getParentNode();
                    if (parentNode != null) {
                        packagename = parentNode.getName();
                    }
                    final PackageInfo dpi = link.getDesiredPackageInfo();
                    if (StringUtils.isEmpty(packagename) && dpi != null) {
                        packagename = dpi.getName();
                    }
                }
                if (StringUtils.isEmpty(packagename)) {
                    packagename = "";
                }
                if (StringUtils.isNotEmpty(modifiers)) {
                    final Pattern patt = lgr.getPackageNameRule()._getPattern();
                    final String[] matches = new Regex(packagename, patt).getRow(0);
                    return Pattern.compile("<jd:" + ORGPACKAGENAME + ":" + Pattern.quote(modifiers) + "\\s*/?\\s*>").matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, StringUtils.valueOrEmpty(matches[Integer.parseInt(modifiers) - 1]))));
                    //
                }
                return pat.matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, packagename)));
            }

            public String getID() {
                return ORGPACKAGENAME;
            }
        });
        addReplacer(SUBFOLDERBYPLUGIN_REPLACER);
        addReplacer(new PackagizerReplacer() {
            private final Pattern pat = Pattern.compile("<jd:orgfiletype/?>");

            public String replace(REPLACEVARIABLE replaceVariable, String modifiers, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                String fileType = new Regex(link.getName(), "\\.([0-9a-zA-Z]+)$").getMatch(0);
                if (fileType == null) {
                    fileType = "";
                }
                if (StringUtils.isNotEmpty(modifiers)) {
                    return Pattern.compile("<jd:orgfiletype:" + Pattern.quote(modifiers) + "/?>").matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, StringUtils.valueOrEmpty(new Regex(fileType, lgr.getFileNameRule()._getPattern()).getRow(0)[Integer.parseInt(modifiers) - 1]))));
                } else {
                    return pat.matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, fileType)));
                }
            }

            public String getID() {
                return ORGFILETYPE;
            }
        });
        addReplacer(new PackagizerReplacer() {
            private final Pattern pat = Pattern.compile("<jd:orgfilenamewithoutext/?>");

            public String replace(REPLACEVARIABLE replaceVariable, String modifiers, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                final String filenameFull = link.getName();
                if (filenameFull == null) {
                    return pat.matcher(input).replaceAll("");
                } else {
                    final ParsedFilename pfname = ParsedFilename.parse(filenameFull);
                    final String filenameWithoutExt = pfname.getFilenameWithoutExtensionAdvanced();
                    return pat.matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, filenameWithoutExt)));
                }
            }

            public String getID() {
                return ORGFILENAMEWITHOUTEXT;
            }
        });
        addReplacer(new PackagizerReplacer() {
            public String replace(REPLACEVARIABLE replaceVariable, String modifiers, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                if (StringUtils.isEmpty(modifiers)) {
                    return input;
                }
                final int id = Integer.parseInt(modifiers);
                if (id == -1) {
                    final String host = link.getHost();
                    final String value = URLEncode.decodeURIComponent(StringUtils.valueOrEmpty(host));
                    return Pattern.compile("<jd:hoster:" + Pattern.quote(String.valueOf(id)) + "/?>").matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, value)));
                }
                final String url;
                if (link.getDownloadLink() != null) {
                    url = link.getDownloadLink().getContentUrlOrPatternMatcher();
                } else {
                    url = link.getURL();
                }
                final Regex regex = new Regex(url, lgr.getHosterRule()._getPattern());
                if (regex.matches()) {
                    final String[] values = regex.getRow(0);
                    final String value = URLEncode.decodeURIComponent(StringUtils.valueOrEmpty(values[id - 1]));
                    return Pattern.compile("<jd:hoster:" + Pattern.quote(String.valueOf(id)) + "/?>").matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, value)));
                } else {
                    return input;
                }
            }

            public String getID() {
                return HOSTER;
            }
        });
        addReplacer(new PackagizerReplacer() {
            public String replace(REPLACEVARIABLE replaceVariable, String modifiers, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                if (StringUtils.isEmpty(modifiers) || link.getDownloadLink() == null) {
                    return input;
                }
                final Object property = link.getDownloadLink().getProperty(modifiers);
                if (property == null || (!(property instanceof String) && !(property instanceof Number))) {
                    return Pattern.compile("<jd:prop:" + Pattern.quote(modifiers) + "/?>").matcher(input).replaceAll("");
                } else {
                    return Pattern.compile("<jd:prop:" + Pattern.quote(modifiers) + "/?>").matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, property.toString())));
                }
            }

            public String getID() {
                return "prop";
            }
        });
        addReplacer(new PackagizerReplacer() {
            public String replace(REPLACEVARIABLE replaceVariable, String key, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                if (StringUtils.isEmpty(key)) {
                    return input;
                }
                final Object property = getGlobalProperty(key);
                if (property == null || (!(property instanceof String) && !(property instanceof Number))) {
                    return Pattern.compile("<jd:globprop:" + Pattern.quote(key) + "/?>").matcher(input).replaceAll("");
                } else {
                    return Pattern.compile("<jd:globprop:" + Pattern.quote(key) + "/?>").matcher(input).replaceAll(Matcher.quoteReplacement(preprocessReplacement(replaceVariable, property.toString())));
                }
            }

            public String getID() {
                return "globprop";
            }
        });
        addReplacer(new PackagizerReplacer() {
            private final Pattern pat = Pattern.compile("<jd:append/?>");

            @Override
            public String replace(REPLACEVARIABLE replaceVariable, String modifiers, CrawledLink link, String input, PackagizerRuleWrapper lgr) {
                final CrawledPackage parentNode = link.getParentNode();
                final PackageInfo dpi = link.getDesiredPackageInfo();
                String append = null;
                switch (replaceVariable) {
                case COMMENT:
                    append = link.getComment();
                    break;
                case DIRECTORY:
                    if (parentNode != null) {
                        append = parentNode.getDownloadFolder();
                    }
                    if (dpi != null && StringUtils.isEmpty(append)) {
                        append = dpi.getDestinationFolder();
                    }
                    break;
                case FILENAME:
                    append = link.getName();
                    break;
                case PACKAGENAME:
                    if (parentNode != null) {
                        append = parentNode.getName();
                    }
                    if (dpi != null && StringUtils.isEmpty(append)) {
                        append = dpi.getName();
                    }
                    break;
                case PACKAGEKEY:
                    if (dpi != null) {
                        append = dpi.getPackageKey();
                    }
                    break;
                default:
                    break;
                }
                if (StringUtils.isEmpty(append)) {
                    return pat.matcher(input).replaceAll("");
                } else {
                    final String ret = pat.matcher(input).replaceAll(Matcher.quoteReplacement(append));
                    if (REPLACEVARIABLE.DIRECTORY.equals(replaceVariable)) {
                        return CrossSystem.fixPathSeparators(ret);
                    } else {
                        return ret;
                    }
                }
            }

            @Override
            public String getID() {
                return "append";
            }
        });
    }

    private static String preprocessReplacement(REPLACEVARIABLE replaceVariable, final String string) {
        if (replaceVariable == null) {
            return string;
        } else {
            switch (replaceVariable) {
            case DIRECTORY:
                return CrossSystem.alleviatePathParts(string);
            default:
                return string;
            }
        }
    }

    private void addReplacer(PackagizerReplacer replacer) {
        this.replacers.put(replacer.getID().toLowerCase(Locale.ENGLISH), replacer);
    }

    public PackagizerControllerEventSender getEventSender() {
        return eventSender;
    }

    public java.util.List<PackagizerRule> list() {
        synchronized (this) {
            return new ArrayList<PackagizerRule>(list);
        }
    }

    public void setList(final List<PackagizerRule> tableData) {
        final ArrayList<PackagizerRule> newList = new ArrayList<PackagizerRule>();
        if (tableData != null) {
            newList.addAll(tableData);
        }
        synchronized (this) {
            list = newList;
            save(newList);
        }
        update();
    }

    public void update() {
        if (isTestInstance()) {
            updateInternal();
        } else {
            TaskQueue.getQueue().add(new QueueAction<Void, RuntimeException>() {
                @Override
                protected Void run() throws RuntimeException {
                    updateInternal();
                    return null;
                }
            });
        }
    }

    private void updateInternal() {
        // url filter only require the urls, and thus can be done before linkcheck
        final ArrayList<PackagizerRuleWrapper> newRules = new ArrayList<PackagizerRuleWrapper>();
        synchronized (this) {
            for (final PackagizerRule lgr : list) {
                if (!lgr.isEnabled() || !lgr._isValid()) {
                    continue;
                }
                try {
                    final PackagizerRuleWrapper compiled = lgr.compile();
                    lgr._setBroken(false);
                    newRules.add(compiled);
                } catch (final Throwable e) {
                    lgr.setEnabled(false);
                    lgr._setBroken(true);
                    LogController.CL().log(e);
                }
            }
            if (!isTestInstance() && rules != null && newRules.size() != rules.size()) {
                save(list);
            }
        }
        rules = newRules;
        if (!getEventSender().hasListener()) {
            return;
        }
        getEventSender().fireEvent(new PackagizerControllerEvent() {
            @Override
            public void sendTo(PackagizerControllerListener listener) {
                listener.onPackagizerUpdate();
            }
        });
    }

    public void add(PackagizerRule linkFilter) {
        if (linkFilter != null) {
            final List<PackagizerRule> addAll = new ArrayList<PackagizerRule>();
            addAll.add(linkFilter);
            addAll(addAll);
        }
    }

    public void addAll(java.util.List<PackagizerRule> all) {
        if (all == null || all.size() == 0) {
            return;
        }
        synchronized (this) {
            final HashSet<String> dupecheck = createDupeSet();
            for (final PackagizerRule rule : all) {
                if (rule.isStaticRule()) {
                    continue;
                }
                if (dupecheck.add(JSonStorage.serializeToJson(rule))) {
                    list.add(rule);
                }
            }
            save(list);
        }
        update();
    }

    private HashSet<String> createDupeSet() {
        final HashSet<String> ret = new HashSet<String>();
        synchronized (this) {
            for (final PackagizerRule rule : list) {
                ret.add(JSonStorage.serializeToJson(rule));
            }
        }
        return ret;
    }

    public void remove(PackagizerRule lf) {
        if (lf == null) {
            return;
        }
        synchronized (this) {
            list.remove(lf);
            save(list);
        }
        update();
    }

    private synchronized final void save(ArrayList<PackagizerRule> rules) {
        if (config == null) {
            return;
        }
        final EventSuppressor<ConfigEvent> eventSuppressor;
        if (ruleListHandler != null) {
            final Thread thread = Thread.currentThread();
            eventSuppressor = new EventSuppressor<ConfigEvent>() {
                @Override
                public boolean suppressEvent(ConfigEvent eventType) {
                    return Thread.currentThread() == thread;
                }
            };
            ruleListHandler.getEventSender().addEventSuppressor(eventSuppressor);
        } else {
            eventSuppressor = null;
        }
        try {
            if (rules == null) {
                config.setRuleList(new ArrayList<PackagizerRule>(0));
            } else {
                config.setRuleList(new ArrayList<PackagizerRule>(rules));
            }
        } finally {
            if (ruleListHandler != null) {
                ruleListHandler.getEventSender().removeEventSuppressor(eventSuppressor);
            }
        }
    }

    public void runByFile(final CrawledLink link) {
        final STATE firstState;
        if (isTestInstance() == false && !org.jdownloader.settings.staticreferences.CFG_PACKAGIZER.PACKAGIZER_ENABLED.isEnabled()) {
            firstState = STATE.AFTER;
        } else {
            firstState = STATE.BEFORE;
        }
        if (getEventSender().hasListener()) {
            getEventSender().fireEvent(new PackagizerControllerEvent() {
                @Override
                public void sendTo(PackagizerControllerListener listener) {
                    listener.onPackagizerRunAfterLinkcheck(link, firstState);
                }
            });
        }
        if (STATE.BEFORE.equals(firstState)) {
            applyPackagizerRules(link, rules, true);
            if (getEventSender().hasListener()) {
                getEventSender().fireEvent(new PackagizerControllerEvent() {
                    @Override
                    public void sendTo(PackagizerControllerListener listener) {
                        listener.onPackagizerRunAfterLinkcheck(link, STATE.AFTER);
                    }
                });
            }
        }
    }

    public void runByUrl(final CrawledLink link) {
        final STATE firstState;
        if (isTestInstance() == false && !org.jdownloader.settings.staticreferences.CFG_PACKAGIZER.PACKAGIZER_ENABLED.isEnabled()) {
            firstState = STATE.AFTER;
        } else {
            firstState = STATE.BEFORE;
        }
        if (getEventSender().hasListener()) {
            getEventSender().fireEvent(new PackagizerControllerEvent() {
                @Override
                public void sendTo(PackagizerControllerListener listener) {
                    listener.onPackagizerRunBeforeLinkcheck(link, firstState);
                }
            });
        }
        if (STATE.BEFORE.equals(firstState)) {
            applyPackagizerRules(link, rules, false);
            if (getEventSender().hasListener()) {
                getEventSender().fireEvent(new PackagizerControllerEvent() {
                    @Override
                    public void sendTo(PackagizerControllerListener listener) {
                        listener.onPackagizerRunBeforeLinkcheck(link, STATE.AFTER);
                    }
                });
            }
        }
    }

    private void applyPackagizerRules(final CrawledLink link, final List<PackagizerRuleWrapper> rules, final boolean afterOnlineCheck) {
        if (rules == null) {
            return;
        }
        for (final PackagizerRuleWrapper lgr : rules) {
            if (lgr.getAlwaysFilter() != null && lgr.getAlwaysFilter().isEnabled()) {
                /* Apply rule */
                set(link, lgr);
                continue;
            }
            if (!lgr.checkHoster(link)) {
                continue;
            }
            if (!lgr.checkPluginStatus(link)) {
                continue;
            }
            if (!lgr.checkOrigin(link)) {
                continue;
            }
            if (!lgr.checkConditions(link)) {
                continue;
            }
            if (!lgr.checkSource(link)) {
                continue;
            }
            if (!lgr.checkPackageName(link)) {
                continue;
            }
            if (!lgr.checkFileType(link)) {
                continue;
            }
            if (!lgr.checkOnlineStatus(link)) {
                continue;
            }
            if (!lgr.checkFileName(link)) {
                continue;
            }
            if (!lgr.checkFileSize(link)) {
                continue;
            }
            /* Apply rule */
            set(link, lgr);
            if (lgr.isStopAfterThisRule()) {
                // TODO: Add logging?
                break;
            }
        }
    }

    /** Returns true if the rule was _really_ applied (in most cases). */
    protected boolean set(CrawledLink link, PackagizerRuleWrapper lgr) {
        if (SubFolderByPackageRule.ID.equals(lgr.getRule().getId()) && StringUtils.contains(CFG_GENERAL.DEFAULT_DOWNLOAD_FOLDER.getValue(), PACKAGETAG)) {
            // ignore SubFolderByPackageRule when default folder already contains variables
            return false;
        }
        boolean packageKeyFlag = false;
        String packageKey = null;
        boolean packageNameFlag = false;
        String packageNameValue = null;
        boolean packageDirectoryFlag = false;
        String packageDirectoryValue = null;
        boolean fileNameFlag = false;
        String fileNameValue = null;
        // PARSE=========================================================================================
        if (lgr.getRule().getChunks() >= 0) {
            /* customize chunk numbers */
            link.setChunks(lgr.getRule().getChunks());
        }
        if (!StringUtils.isEmpty(lgr.getRule().getDownloadDestination())) {
            /* customize download destination folder */
            packageDirectoryFlag = true;
            packageDirectoryValue = replaceVariables(REPLACEVARIABLE.DIRECTORY, lgr.getRule().getDownloadDestination(), link, lgr);
        }
        if (lgr.getRule().getLinkEnabled() != null) {
            link.setEnabled(lgr.getRule().getLinkEnabled());
        }
        if (!StringUtils.isEmpty(lgr.getRule().getPackageName())) {
            /* customize package name */
            packageNameFlag = true;
            packageNameValue = replaceVariables(REPLACEVARIABLE.PACKAGENAME, lgr.getRule().getPackageName(), link, lgr);
        }
        if (lgr.getRule().getPriority() != null) {
            /* customize priority */
            link.setPriority(lgr.getRule().getPriority());
        }
        if (!StringUtils.isEmpty(lgr.getRule().getFilename())) {
            /* customize filename */
            fileNameFlag = true;
            fileNameValue = replaceVariables(REPLACEVARIABLE.FILENAME, lgr.getRule().getFilename(), link, lgr);
        }
        if (!StringUtils.isEmpty(lgr.getRule().getComment())) {
            /* customize filename */
            link.setComment(replaceVariables(REPLACEVARIABLE.COMMENT, lgr.getRule().getComment(), link, lgr));
        }
        if (StringUtils.isNotEmpty((packageKey = lgr.getRule().getPackageKey()))) {
            /* customize packagekey */
            packageKeyFlag = true;
            packageKey = replaceVariables(REPLACEVARIABLE.PACKAGEKEY, lgr.getRule().getPackageKey(), link, lgr);
        }
        if (fileNameFlag && StringUtils.isNotEmpty(fileNameValue)) {
            link.setName(fileNameValue);
        }
        // MODIFY=========================================================================================
        if (packageKeyFlag) {
            final PackageInfo dpi;
            if (link.getDesiredPackageInfo() != null) {
                if (StringUtils.isNotEmpty(packageKey)) {
                    dpi = link.getDesiredPackageInfo();
                } else {
                    dpi = null;
                }
            } else {
                if (StringUtils.isNotEmpty(packageKey)) {
                    dpi = new PackageInfo();
                } else {
                    dpi = null;
                }
            }
            if (dpi != null) {
                dpi.setPackageKey(packageKey);
                dpi.setPackagizerRuleMatched(true);
                link.setDesiredPackageInfo(dpi);
            }
        }
        if (packageNameFlag) {
            final PackageInfo dpi;
            if (link.getDesiredPackageInfo() != null) {
                if (StringUtils.isNotEmpty(packageNameValue)) {
                    dpi = link.getDesiredPackageInfo();
                } else {
                    dpi = null;
                }
            } else {
                if (StringUtils.isNotEmpty(packageNameValue)) {
                    dpi = new PackageInfo();
                } else {
                    dpi = null;
                }
            }
            if (dpi != null) {
                dpi.setPackagizerRuleMatched(true);
                dpi.setName(packageNameValue);
                link.setDesiredPackageInfo(dpi);
            }
        }
        if (packageDirectoryFlag) {
            final PackageInfo dpi;
            if (link.getDesiredPackageInfo() != null) {
                if (StringUtils.isNotEmpty(packageDirectoryValue)) {
                    dpi = link.getDesiredPackageInfo();
                } else {
                    dpi = null;
                }
            } else {
                if (StringUtils.isNotEmpty(packageDirectoryValue)) {
                    dpi = new PackageInfo();
                } else {
                    dpi = null;
                }
            }
            if (dpi != null) {
                dpi.setPackagizerRuleMatched(true);
                dpi.setDestinationFolder(packageDirectoryValue);
                link.setDesiredPackageInfo(dpi);
            }
        }
        Boolean b = null;
        if ((b = lgr.getRule().isAutoExtractionEnabled()) != null) {
            /* customize auto extract */
            link.getArchiveInfo().setAutoExtract(b ? BooleanStatus.TRUE : BooleanStatus.FALSE);
        }
        if ((b = lgr.getRule().isAutoAddEnabled()) != null) {
            /* customize auto add */
            link.setAutoConfirmEnabled(b);
        }
        if ((b = lgr.getRule().isAutoStartEnabled()) != null) {
            /* customize auto start */
            link.setAutoStartEnabled(b);
        }
        if ((b = lgr.getRule().isAutoForcedStartEnabled()) != null) {
            /* customize auto start */
            link.setForcedAutoStartEnabled(b);
        }
        return true;
    }

    public static final String PACKAGETAG = "<jd:" + PackagizerController.PACKAGENAME + ">";
    public static final String DATETAG    = "<jd:" + PackagizerController.SIMPLEDATE + ":";
    public static final String INDEXOFTAG = "<jd:" + PackagizerController.INDEXOF + ">";
    public static final String ENVTAG     = "<jd:" + PackagizerController.ENV + ":";

    private final static int padLength(final int size) {
        if (size < 10) {
            return 1;
        } else if (size < 100) {
            return 2;
        } else if (size < 1000) {
            return 3;
        } else if (size < 10000) {
            return 4;
        } else if (size < 100000) {
            return 5;
        } else if (size < 1000000) {
            return 6;
        } else if (size < 10000000) {
            return 7;
        } else {
            return 8;// hello djmakinera
        }
    }

    private static String replaceDynamicTags(REPLACEVARIABLE type, String input, String tag, PackagizerReplacer replacer, AtomicBoolean modifyFlag) {
        int lastStart = -1;
        while (true) {
            final int start;
            if (lastStart == -1) {
                start = input.indexOf(tag);
            } else {
                start = input.indexOf(tag, lastStart);
            }
            if (start > lastStart) {
                if (modifyFlag != null) {
                    modifyFlag.set(true);
                }
                lastStart = start;
                int end = start + tag.length();
                while (end < input.length() && input.charAt(end) != '>') {
                    end++;
                }
                final String modifier = input.substring(start + tag.length(), end);
                try {
                    input = replacer.replace(type, modifier, null, input, null);
                } catch (final Throwable e) {
                    LogController.CL().log(e);
                }
            } else {
                break;
            }
        }
        return input;
    }

    public static String replaceDynamicTags(String input, String packageName, AbstractNode node) {
        return replaceDynamicTags(input, packageName, node, null);
    }

    public static String replaceDynamicTags(String input, String packageName, AbstractNode node, final Set<String> replaceTags) {
        if (input == null || !input.contains("<jd:")) {
            return input;
        }
        String ret = input;
        final AtomicBoolean modifyFlag = new AtomicBoolean(false);
        if (ret.contains(PACKAGETAG) && (replaceTags == null || replaceTags.contains(PACKAGETAG))) {
            modifyFlag.set(true);
            if (StringUtils.isEmpty(packageName)) {
                ret = ret.replace(PACKAGETAG, "");
            } else {
                ret = ret.replace(PACKAGETAG, LinknameCleaner.cleanPackagename(packageName, true));
            }
        }
        if (ret.contains(INDEXOFTAG) && (replaceTags == null || replaceTags.contains(INDEXOFTAG))) {
            modifyFlag.set(true);
            AbstractPackageNode parentNode = null;
            if (!(node instanceof AbstractPackageChildrenNode) || (parentNode = ((AbstractPackageChildrenNode<AbstractPackageNode>) node).getParentNode()) == null) {
                ret = ret.replace(INDEXOFTAG, "");
            } else {
                final ModifyLock modifyLock = parentNode.getModifyLock();
                final int index;
                final int size;
                final boolean readL = modifyLock.readLock();
                try {
                    index = parentNode.indexOf((AbstractPackageChildrenNode) node);
                    size = parentNode.getChildren().size();
                } finally {
                    modifyLock.readUnlock(readL);
                }
                if (index >= 0) {
                    final String replacement = String.format(Locale.US, "%0" + padLength(size) + "d", index + 1);
                    ret = ret.replace(INDEXOFTAG, replacement);
                } else {
                    ret = ret.replace(INDEXOFTAG, "");
                }
            }
        }
        ret = replaceDynamicTags(REPLACEVARIABLE.DIRECTORY, ret, ENVTAG, ENV_REPLACER, modifyFlag);
        ret = replaceDynamicTags(REPLACEVARIABLE.DIRECTORY, ret, DATETAG, DATER_REPLACER, modifyFlag);
        if (modifyFlag.get()) {
            ret = CrossSystem.fixPathSeparators(ret);
        }
        ret = ret.trim();
        return ret;
    }

    public static enum REPLACEVARIABLE {
        PACKAGEKEY,
        DIRECTORY,
        COMMENT,
        FILENAME,
        PACKAGENAME
    }

    public String replaceVariables(final REPLACEVARIABLE replaceVariable, String txt, CrawledLink link, PackagizerRuleWrapper lgr) {
        final String[][] matches = new Regex(txt, "<jd:([^>:]+)(?::(.+?))?\\s*/?\\s*>").getMatches();
        if (matches == null) {
            return txt;
        }
        for (String m[] : matches) {
            try {
                final PackagizerReplacer replacer = replacers.get(m[0].toLowerCase(Locale.ENGLISH));
                if (replacer == null) {
                    /* User has entered garbage -> Ignore it */
                    continue;
                }
                final String modifier;
                if (StringUtils.isEmpty(m[1])) {
                    modifier = null;
                } else {
                    modifier = m[1];
                }
                txt = replacer.replace(replaceVariable, modifier, link, txt, lgr);
            } catch (final Throwable e) {
                LogController.CL(false).log(e);
            }
        }
        return txt;
    }

    // TODO: add support for deep extraction
    public void onNewFile(Object caller, File[] fileList) {
        if (!org.jdownloader.settings.staticreferences.CFG_PACKAGIZER.PACKAGIZER_ENABLED.isEnabled()) {
            return;
        }
        if (!(caller instanceof ExtractionController) || caller == this) {
            return;
        }
        if (!(((ExtractionController) caller).getArchive() instanceof DownloadLinkArchive)) {
            return;
        }
        CrawledPackage crawledPackage = null;
        for (final ArchiveFile af : ((ExtractionController) caller).getArchive().getArchiveFiles()) {
            if (!(af instanceof DownloadLinkArchiveFile)) {
                continue;
            }
            for (final DownloadLink link : ((DownloadLinkArchiveFile) af).getDownloadLinks()) {
                for (final File file : fileList) {
                    if (!file.exists()) {
                        continue;
                    }
                    final CrawledLink cl = new CrawledLink(link) {
                        @Override
                        protected void passwordForward(DownloadLink dlLink) {
                            /* not needed and used in constructor */
                        }
                    };
                    cl.setName(file.getName());
                    final ArrayList<String> sourceURLs = new ArrayList<String>();
                    String url = link.getOriginUrl();
                    if (url != null) {
                        sourceURLs.add(url);
                    }
                    url = link.getReferrerUrl();
                    if (url != null) {
                        sourceURLs.add(url);
                    }
                    url = link.getContainerUrl();
                    if (url != null) {
                        sourceURLs.add(url);
                    }
                    url = link.getContentUrl();
                    if (url != null) {
                        sourceURLs.add(url);
                    }
                    url = link.getCustomUrl();
                    if (url != null) {
                        sourceURLs.add(url);
                    }
                    if (sourceURLs.size() > 0) {
                        cl.setSourceUrls(sourceURLs.toArray(new String[0]));
                    }
                    final FilePackage filePackage = link.getLastValidFilePackage();
                    if (filePackage != null) {
                        crawledPackage = new CrawledPackage();
                        crawledPackage.setName(filePackage.getName());
                        crawledPackage.setDownloadFolder(filePackage.getDownloadDirectory());
                        cl.setParentNode(crawledPackage);
                    } else {
                        cl.setParentNode(crawledPackage);
                    }
                    runAfterExtraction(file, cl);
                }
            }
        }
    }

    private final static String ORGPACKAGETAG = "<jd:" + PackagizerController.ORGPACKAGENAME + ">";

    private void runAfterExtraction(File file, CrawledLink dummyLink) {
        final String originalFolder = file.getParent();
        String moveToFolder = originalFolder;
        final String originalFileName = dummyLink.getName();
        for (PackagizerRuleWrapper lgr : rules) {
            String renameRule = lgr.getRule().getRename();
            String moveRule = lgr.getRule().getMoveto();
            if (!StringUtils.isEmpty(renameRule) || !StringUtils.isEmpty(moveRule)) {
                if (lgr.getAlwaysFilter() == null || !lgr.getAlwaysFilter().isEnabled()) {
                    if (!lgr.checkHoster(dummyLink)) {
                        continue;
                    }
                    if (!lgr.checkPluginStatus(dummyLink)) {
                        continue;
                    }
                    if (!lgr.checkOrigin(dummyLink)) {
                        continue;
                    }
                    if (!lgr.checkConditions(dummyLink)) {
                        continue;
                    }
                    if (!lgr.checkSource(dummyLink)) {
                        continue;
                    }
                    if (!lgr.checkPackageName(dummyLink)) {
                        continue;
                    }
                    if (!lgr.checkFileType(dummyLink)) {
                        continue;
                    }
                    if (!lgr.checkFileName(dummyLink)) {
                        continue;
                    }
                    if (!lgr.checkFileSize(dummyLink)) {
                        continue;
                    }
                }
                if (!StringUtils.isEmpty(renameRule)) {
                    renameRule = renameRule.replace(PACKAGETAG, ORGPACKAGETAG);
                    dummyLink.setName(replaceVariables(REPLACEVARIABLE.FILENAME, renameRule, dummyLink, lgr));
                }
                if (!StringUtils.isEmpty(moveRule)) {
                    moveRule = moveRule.replace(PACKAGETAG, ORGPACKAGETAG);
                    moveToFolder = replaceVariables(REPLACEVARIABLE.DIRECTORY, moveRule, dummyLink, lgr);
                }
            }
        }
        if (!originalFolder.equals(moveToFolder) || !originalFileName.equals(dummyLink.getName())) {
            final File newFile = new File(moveToFolder, dummyLink.getName());
            if (newFile.getParentFile().exists() == false && FileCreationManager.getInstance().mkdir(newFile.getParentFile()) == false) {
                LogController.CL(false).warning("Packagizer could not create " + newFile.getParentFile());
                return;
            }
            boolean successful = false;
            if ((successful = file.renameTo(newFile)) == false) {
                final LogSource log = LogController.CL(false);
                log.warning("Packagizer rename failed " + file + " to" + newFile);
                try {
                    log.warning("Packagizer try copy " + file + " to" + newFile);
                    IO.copyFile(file, newFile);
                    FileCreationManager.getInstance().delete(file, null);
                    successful = true;
                } catch (final Throwable e) {
                    FileCreationManager.getInstance().delete(newFile, null);
                    log.warning("Packagizer could not move/rename " + file + " to" + newFile);
                }
            }
            if (successful) {
                LogController.CL(false).info("Packagizer moved/renamed " + file + " to " + newFile);
                FileCreationManager.getInstance().getEventSender().fireEvent(new FileCreationEvent(PackagizerController.this, FileCreationEvent.Type.NEW_FILES, new File[] { newFile }));
            }
        }
    }

    @Override
    public void onNewFolder(Object caller, File folder) {
    }
}
