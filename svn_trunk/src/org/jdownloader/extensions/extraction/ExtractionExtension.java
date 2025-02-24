//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program  is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSSE the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://gnu.org/licenses/>.
package org.jdownloader.extensions.extraction;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.regex.Pattern;

import org.appwork.shutdown.ShutdownController;
import org.appwork.shutdown.ShutdownRequest;
import org.appwork.shutdown.ShutdownVetoException;
import org.appwork.shutdown.ShutdownVetoListener;
import org.appwork.uio.UIOManager;
import org.appwork.utils.JVMVersion;
import org.appwork.utils.StringUtils;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.controlling.FileCreationListener;
import org.jdownloader.controlling.FileCreationManager;
import org.jdownloader.controlling.contextmenu.ActionData;
import org.jdownloader.controlling.contextmenu.ContextMenuManager;
import org.jdownloader.controlling.contextmenu.MenuContainerRoot;
import org.jdownloader.controlling.contextmenu.MenuExtenderHandler;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.controlling.contextmenu.SeparatorData;
import org.jdownloader.controlling.packagizer.PackagizerController;
import org.jdownloader.extensions.AbstractExtension;
import org.jdownloader.extensions.ExtensionConfigPanel;
import org.jdownloader.extensions.StartException;
import org.jdownloader.extensions.StopException;
import org.jdownloader.extensions.extraction.actions.ExtractAction;
import org.jdownloader.extensions.extraction.bindings.crawledlink.CrawledLinkFactory;
import org.jdownloader.extensions.extraction.bindings.downloadlink.DownloadLinkArchiveFactory;
import org.jdownloader.extensions.extraction.bindings.file.FileArchiveFactory;
import org.jdownloader.extensions.extraction.bindings.file.FileArchiveFile;
import org.jdownloader.extensions.extraction.contextmenu.ArchivesSubMenu;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.ArchiveValidator;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.CleanupSubMenu;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.action.AbortExtractionAction;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.action.AutoExtractEnabledToggleAction;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.action.CleanupAutoDeleteFilesEnabledToggleAction;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.action.CleanupAutoDeleteLinksEnabledToggleAction;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.action.ExtractArchiveNowAction;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.action.SetExtractPasswordAction;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.action.SetExtractToAction;
import org.jdownloader.extensions.extraction.contextmenu.downloadlist.action.ValidateArchivesAction;
import org.jdownloader.extensions.extraction.gui.bubble.ExtractionBubbleSupport;
import org.jdownloader.extensions.extraction.gui.config.ExtractionConfigPanel;
import org.jdownloader.extensions.extraction.multi.ArchiveException;
import org.jdownloader.extensions.extraction.multi.CheckException;
import org.jdownloader.extensions.extraction.multi.Multi;
import org.jdownloader.extensions.extraction.multi.Zip4J;
import org.jdownloader.extensions.extraction.split.HJSplit;
import org.jdownloader.extensions.extraction.split.HachaSplit;
import org.jdownloader.extensions.extraction.split.UnixSplit;
import org.jdownloader.extensions.extraction.split.XtreamSplit;
import org.jdownloader.extensions.extraction.translate.ExtractionTranslation;
import org.jdownloader.gui.mainmenu.MenuManagerMainmenu;
import org.jdownloader.gui.mainmenu.container.ExtensionsMenuContainer;
import org.jdownloader.gui.mainmenu.container.OptionalContainer;
import org.jdownloader.gui.notify.BubbleNotify;
import org.jdownloader.gui.toolbar.MenuManagerMainToolbar;
import org.jdownloader.gui.views.downloads.context.submenu.MoreMenuContainer;
import org.jdownloader.gui.views.downloads.contextmenumanager.MenuManagerDownloadTableContext;
import org.jdownloader.gui.views.linkgrabber.contextmenu.LinkGrabberMoreSubMenu;
import org.jdownloader.gui.views.linkgrabber.contextmenu.MenuManagerLinkgrabberTableContext;
import org.jdownloader.images.NewTheme;
import org.jdownloader.settings.IfFileExistsAction;
import org.jdownloader.settings.staticreferences.CFG_LINKGRABBER;
import org.jdownloader.translate._JDT;

import jd.SecondLevelLaunch;
import jd.controlling.downloadcontroller.DownloadController;
import jd.controlling.downloadcontroller.SingleDownloadController;
import jd.controlling.linkcollector.LinkCollector;
import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.packagecontroller.PackageControllerModifyVetoListener;
import jd.plugins.AddonPanel;
import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

public class ExtractionExtension extends AbstractExtension<ExtractionConfig, ExtractionTranslation> implements FileCreationListener, MenuExtenderHandler, PackageControllerModifyVetoListener<FilePackage, DownloadLink> {
    private ExtractionQueue       extractionQueue = new ExtractionQueue();
    private ExtractionEventSender eventSender     = new ExtractionEventSender();

    public ExtractionEventSender getEventSender() {
        return eventSender;
    }

    private final Set<IExtraction>     extractors        = new CopyOnWriteArraySet<IExtraction>();
    private ExtractionConfigPanel      configPanel;
    private static ExtractionExtension INSTANCE;
    private ExtractionListenerIcon     statusbarListener = null;
    private ShutdownVetoListener       listener          = null;
    private boolean                    lazyInitOnStart   = false;
    private final Object               PWLOCK            = new Object();

    public ExtractionExtension() throws StartException {
        super();
        setTitle(T.name());
        INSTANCE = this;
    }

    @Override
    public boolean isDefaultEnabled() {
        return true;
    }

    public static ExtractionExtension getInstance() {
        return INSTANCE;
    }

    /**
     * Adds all internal extraction plugins.
     */
    private void initExtractors() {
        /* the order is important because hjsplit and multi listen to same patterns (xy.001, because 7zip can have that pattern as well) */
        addExtractor(new UnixSplit(this));
        addExtractor(new XtreamSplit(this));
        addExtractor(new HachaSplit(this));
        addExtractor(new HJSplit(this));
        try {
            if (JVMVersion.isMinimum(JVMVersion.JAVA_1_7)) {
                addExtractor(new Zip4J(this));
            }
        } catch (UnsupportedClassVersionError ignore) {
        }
        /* must be last one! */
        addExtractor(new Multi(this));
    }

    /**
     * Adds an extraction plugin to the framework.
     *
     * @param extractor
     *            The extractor.
     */
    public void addExtractor(IExtraction extractor) {
        extractors.add(extractor);
        extractor.setLogger(logger);
    }

    public synchronized ExtractionController addToQueue(final Archive archive, boolean forceAskForUnknownPassword) {
        return addToQueue(null, archive, forceAskForUnknownPassword);
    }

    /**
     * Adds an archive to the extraction queue.
     */
    public synchronized ExtractionController addToQueue(Object caller, final Archive archive, boolean forceAskForUnknownPassword) {
        if (archive == null) {
            return null;
        } else if (archive.getArchiveFiles().size() == 0) {
            logger.info("Empty Archive(" + archive.getArchiveID() + "|" + caller + "):" + archive.getName());
            return null;
        }
        // check if we have this archive already in queue.
        for (final ExtractionController ec : extractionQueue.getJobs()) {
            if (ec.isSameArchive(archive)) {
                return ec;
            } else if (ec.isOutDatedArchive(archive)) {
                if (extractionQueue.remove(ec)) {
                    logger.info("removed outdated Archive(" + ec.getArchive().getArchiveID() + "|started:" + ec.gotStarted());
                }
            }
        }
        DummyArchive dummyArchive = null;
        try {
            dummyArchive = createDummyArchive(archive);
        } catch (CheckException e) {
            logger.log(e);
        }
        if (dummyArchive == null || !dummyArchive.isComplete()) {
            if (dummyArchive == null) {
                logger.info("Incomplete Archive(" + archive.getArchiveID() + "|" + caller + "):" + archive.getName());
            } else {
                logger.info("Incomplete Archive(" + archive.getArchiveID() + "|" + caller + "):" + dummyArchive.toString());
            }
            return null;
        }
        final IExtraction extractor = getExtractorInstanceByFactory(archive.getFactory());
        if (extractor == null) {
            logger.info("Unsupported Archive(" + archive.getArchiveID() + "|" + caller + "):" + archive.getName());
            return null;
        } else {
            logger.info("Supported Archive(" + archive.getArchiveID() + "|" + caller + "):" + dummyArchive.toString());
        }
        archive.getFactory().fireArchiveAddedToQueue(archive);
        final ExtractionController controller = new ExtractionController(this, archive, extractor);
        controller.setAskForUnknownPassword(forceAskForUnknownPassword);
        controller.setIfFileExistsAction(getIfFileExistsAction(archive));
        extractor.setConfig(getSettings());
        extractionQueue.addAsynch(controller);
        fireEvent(new ExtractionEvent(controller, ExtractionEvent.Type.QUEUED));
        return controller;
    }

    public boolean isRemoveDownloadLinksAfterExtractEnabled(Archive archive) {
        switch (archive.getSettings().getRemoveDownloadLinksAfterExtraction()) {
        case FALSE:
            return false;
        case TRUE:
            return true;
        default:
            return getSettings().isDeleteArchiveDownloadlinksAfterExtraction();
        }
    }

    public FileCreationManager.DeleteOption getRemoveFilesAfterExtractAction(Archive archive) {
        switch (archive.getSettings().getRemoveFilesAfterExtraction()) {
        case FALSE:
            return FileCreationManager.DeleteOption.NO_DELETE;
        case TRUE:
            switch (getSettings().getDeleteArchiveFilesAfterExtractionAction()) {
            case NO_DELETE:
            case RECYCLE:
                return FileCreationManager.DeleteOption.RECYCLE;
            default:
                return FileCreationManager.DeleteOption.NULL;
            }
        default:
            return getSettings().getDeleteArchiveFilesAfterExtractionAction();
        }
    }

    @Override
    public String getIconKey() {
        return org.jdownloader.gui.IconKey.ICON_EXTRACT;
    }

    public synchronized void abortAll() {
        extractionQueue.killQueue();
    }

    /**
     * Builds an archive for an {@link DownloadLink}.
     *
     * @param link
     * @return
     * @throws ArchiveException
     */
    public Archive buildArchive(final ArchiveFactory factory) throws ArchiveException {
        if (Boolean.FALSE.equals(factory.isPartOfAnArchive())) {
            return null;
        }
        final ExtractionController ec = getExtractionController(factory);
        final Archive existing = ec != null ? ec.getArchive() : null;
        if (existing == null) {
            Throwable throwable = null;
            final boolean deepInspection = !(factory instanceof CrawledLinkFactory);
            for (IExtraction extractor : extractors) {
                try {
                    if (!Boolean.FALSE.equals(extractor.isSupported(factory, deepInspection))) {
                        final Archive archive = extractor.buildArchive(factory, deepInspection);
                        if (archive != null) {
                            for (final ArchiveFile archiveFile : archive.getArchiveFiles()) {
                                archiveFile.setArchive(archive);
                            }
                            return archive;
                        }
                    }
                } catch (final Throwable e) {
                    throwable = e;
                    logger.log(e);
                }
            }
            if (throwable == null) {
                factory.setPartOfAnArchive(false);
            }
        }
        return existing;
    }

    public DummyArchive createDummyArchive(final Archive archive) throws CheckException {
        if (archive == null) {
            return null;
        }
        final ArchiveFactory factory = archive.getFactory();
        if (Boolean.FALSE.equals(factory.isPartOfAnArchive())) {
            return null;
        }
        final boolean deepInspection = !(factory instanceof CrawledLinkFactory);
        for (IExtraction extractor : extractors) {
            try {
                if (!Boolean.FALSE.equals(extractor.isSupported(factory, deepInspection))) {
                    final DummyArchive dummyArchive = extractor.checkComplete(archive);
                    if (dummyArchive != null) {
                        return dummyArchive;
                    }
                }
            } catch (final Throwable e) {
                logger.log(e);
            }
        }
        return null;
    }

    public boolean isComplete(Archive archive) {
        try {
            final DummyArchive ret = archive != null ? createDummyArchive(archive) : null;
            final boolean isComplete = ret != null && ret.isComplete();
            return isComplete;
        } catch (CheckException e) {
            logger.log(e);
        }
        return false;
    }

    /**
     * Returns the extractor for the {@link DownloadLink}.
     *
     * @param link
     * @return
     */
    private IExtraction getExtractorInstanceByFactory(final ArchiveFactory factory) {
        if (Boolean.FALSE.equals(factory.isPartOfAnArchive())) {
            return null;
        }
        for (IExtraction extractor : extractors) {
            try {
                if (Boolean.TRUE.equals(extractor.isSupported(factory, true))) {
                    final IExtraction ret = extractor.getClass().getConstructor(new Class[] { this.getClass() }).newInstance(this);
                    ret.setLogger(extractor.logger);
                    return ret;
                }
            } catch (Throwable e) {
                getLogger().log(e);
            }
        }
        return null;
    }

    @Override
    protected void stop() throws StopException {
        ShutdownController.getInstance().removeShutdownVetoListener(listener);
        LinkCollector.getInstance().setArchiver(null);
        if (!org.appwork.utils.Application.isHeadless()) {
            MenuManagerDownloadTableContext.getInstance().unregisterExtender(this);
            MenuManagerLinkgrabberTableContext.getInstance().unregisterExtender(this);
            MenuManagerMainmenu.getInstance().unregisterExtender(this);
            MenuManagerMainToolbar.getInstance().unregisterExtender(this);
        }
        DownloadController.getInstance().removeVetoListener(this);
        FileCreationManager.getInstance().getEventSender().removeListener(this);
        if (!org.appwork.utils.Application.isHeadless()) {
            SecondLevelLaunch.GUI_COMPLETE.executeWhenReached(new Runnable() {
                public void run() {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            if (statusbarListener != null) {
                                statusbarListener.cleanup();
                                eventSender.removeListener(statusbarListener);
                            }
                            if (bubbleSupport != null) {
                                eventSender.removeListener(bubbleSupport);
                                BubbleNotify.getInstance().unregisterTypes(bubbleSupport);
                            }
                        }
                    };
                }
            });
        }
    }

    public void addPassword(String pw) {
        if (StringUtils.isEmpty(pw)) {
            return;
        }
        synchronized (PWLOCK) {
            java.util.List<String> pwList = getSettings().getPasswordList();
            if (pwList == null) {
                pwList = new ArrayList<String>();
            }
            /* avoid duplicates */
            pwList.remove(pw);
            pwList.add(0, pw);
            getSettings().setPasswordList(pwList);
        }
    }

    @Override
    public boolean isHeadlessRunnable() {
        return true;
    }

    private ExtractionBubbleSupport bubbleSupport;

    @Override
    protected void start() throws StartException {
        lazyInitOnceOnStart();
        if (!org.appwork.utils.Application.isHeadless()) {
            MenuManagerDownloadTableContext.getInstance().registerExtender(this);
            MenuManagerLinkgrabberTableContext.getInstance().registerExtender(this);
            MenuManagerMainmenu.getInstance().registerExtender(this);
            MenuManagerMainToolbar.getInstance().registerExtender(this);
        }
        LinkCollector.getInstance().setArchiver(this);
        DownloadController.getInstance().addVetoListener(this);
        FileCreationManager.getInstance().getEventSender().addListener(this);
        if (!org.appwork.utils.Application.isHeadless()) {
            SecondLevelLaunch.GUI_COMPLETE.executeWhenReached(new Runnable() {
                public void run() {
                    new EDTRunner() {
                        @Override
                        protected void runInEDT() {
                            if (statusbarListener != null) {
                                statusbarListener.cleanup();
                            }
                            eventSender.addListener(statusbarListener = new ExtractionListenerIcon(ExtractionExtension.this));
                            bubbleSupport = new ExtractionBubbleSupport(T.bubbletype(), CFG_EXTRACTION.BUBBLE_ENABLED_IF_ARCHIVE_EXTRACTION_IS_IN_PROGRESS);
                            eventSender.addListener(bubbleSupport, true);
                            BubbleNotify.getInstance().registerType(bubbleSupport);
                        }
                    };
                }
            });
        }
        ShutdownController.getInstance().addShutdownVetoListener(listener = new ShutdownVetoListener() {
            @Override
            public void onShutdownVetoRequest(ShutdownRequest request) throws ShutdownVetoException {
                if (request.hasVetos()) {
                    /* we already abort shutdown, no need to ask again */
                    return;
                }
                if (request.isSilent()) {
                    if (!extractionQueue.isEmpty()) {
                        throw new ShutdownVetoException("ExtractionExtension is still running", this);
                    }
                } else {
                    if (!extractionQueue.isEmpty()) {
                        if (UIOManager.I().showConfirmDialog(Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN | UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL, _JDT.T.Extraction_onShutdownRequest_(), _JDT.T.Extraction_onShutdownRequest_msg(), NewTheme.I().getIcon(org.jdownloader.gui.IconKey.ICON_EXTRACT, 32), _JDT.T.literally_yes(), null)) {
                            return;
                        }
                        throw new ShutdownVetoException("ExtractionExtension is still running", this);
                    }
                }
            }

            @Override
            public long getShutdownVetoPriority() {
                return 0;
            }

            @Override
            public void onShutdown(ShutdownRequest request) {
            }

            @Override
            public void onShutdownVeto(ShutdownRequest request) {
            }
        });
    }

    private void lazyInitOnceOnStart() {
        if (lazyInitOnStart) {
            return;
        }
        lazyInitOnStart = true;
        initExtractors();
        // addListener(new ExtractionListenerFile());
        eventSender.addListener(new ExtractionListenerList());
        final Iterator<IExtraction> it = extractors.iterator();
        final List<IExtraction> remove = new ArrayList<IExtraction>();
        while (it.hasNext()) {
            IExtraction extractor = it.next();
            if (!extractor.isAvailable(this)) {
                logger.severe("Extractor " + extractor.getClass().getName() + " plugin could not be initialized");
                remove.add(extractor);
            }
        }
        extractors.removeAll(remove);
    }

    void fireEvent(ExtractionEvent event) {
        eventSender.fireEvent(event);
    }

    @Override
    public void handleCommand(String command, String... parameters) {
        if (command.equalsIgnoreCase("add-passwords") || command.equalsIgnoreCase("add-passwords") || command.equalsIgnoreCase("p")) {
            synchronized (PWLOCK) {
                List<String> lst = getSettings().getPasswordList();
                ArrayList<String> ret = new ArrayList<String>();
                if (lst != null) {
                    ret.addAll(lst);
                }
                Collection<String> newPws = Arrays.asList(parameters);
                ret.removeAll(newPws);
                ret.addAll(0, newPws);
                getSettings().setPasswordList(ret);
                logger.info("Added Passwords: " + newPws + " New List Size: " + ret.size());
            }
        }
    }

    // public void handleStartupParameters(ParameterParser parameters) {
    // +#
    // cs=parameters.getCommandSwitch(")
    //
    // ;
    // }
    @Override
    protected void initExtension() throws StartException {
        ArchiveValidator.EXTENSION = this;
    }

    @Override
    public boolean hasConfigPanel() {
        return true;
    }

    @Override
    public String getDescription() {
        return T.description();
    }

    @Override
    public AddonPanel<ExtractionExtension> getGUI() {
        return null;
    }

    @Override
    public ExtensionConfigPanel<ExtractionExtension> getConfigPanel() {
        if (configPanel != null) {
            return configPanel;
        }
        return new EDTHelper<ExtractionConfigPanel>() {
            @Override
            public ExtractionConfigPanel edtRun() {
                if (configPanel != null) {
                    return configPanel;
                }
                configPanel = new ExtractionConfigPanel(ExtractionExtension.this);
                return configPanel;
            }
        }.getReturnValue();
    }

    public ExtractionQueue getJobQueue() {
        return extractionQueue;
    }

    /**
     * Cancels a job
     *
     * @param activeValue
     */
    public boolean cancel(final ExtractionController controller) {
        final boolean wasInProgress = getJobQueue().isInProgress(controller);
        final boolean ret;
        if (wasInProgress) {
            ret = true;
            if (!controller.isFinished() && !controller.gotKilled()) {
                controller.kill();
            }
        } else {
            ret = getJobQueue().remove(controller);
        }
        return ret;
    }

    public synchronized List<ExtractionController> getExtractionControllers(final Object archiveFactory) {
        final List<ExtractionController> ret = new ArrayList<ExtractionController>();
        for (ExtractionController ec : extractionQueue.getJobs()) {
            final Archive archive = ec.getArchive();
            if (archive.contains(archiveFactory)) {
                ret.add(ec);
            }
        }
        if (ret.size() > 0) {
            return ret;
        } else {
            return null;
        }
    }

    public synchronized ExtractionController getExtractionController(final Object archiveFactory) {
        final List<ExtractionController> ret = getExtractionControllers(archiveFactory);
        if (ret != null && ret.size() > 0) {
            return ret.get(0);
        } else {
            return null;
        }
    }

    private boolean matchesDeepExtractionBlacklist(File[] fileList) {
        final String[] patternStrings = getSettings().getDeepExtractionBlacklistPatterns();
        if (fileList != null && fileList.length > 0 && patternStrings != null && patternStrings.length > 0) {
            final ArrayList<Pattern> patterns = new ArrayList<Pattern>();
            for (final String patternString : patternStrings) {
                try {
                    if (StringUtils.isNotEmpty(patternString) && !patternString.startsWith("##")) {
                        patterns.add(Pattern.compile(patternString));
                    }
                } catch (final Throwable e) {
                    logger.log(e);
                }
            }
            if (patterns.size() > 0) {
                for (File file : fileList) {
                    for (Pattern pattern : patterns) {
                        final String path = file.getAbsolutePath();
                        if (pattern.matcher(path).matches()) {
                            logger.info("Skip deep extraction: " + pattern.toString() + " matches file " + path);
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    public void onNewFile(final Object caller, final File[] fileList) {
        if (fileList == null || fileList.length == 0) {
            return;
        }
        try {
            if (caller instanceof SingleDownloadController) {
                final DownloadLink link = ((SingleDownloadController) caller).getDownloadLink();
                if (getExtractionController(link) == null) {
                    final Archive archive = buildArchive(new DownloadLinkArchiveFactory(link));
                    if (archive != null && isAutoExtractEnabled(archive)) {
                        addToQueue(caller, archive, false);
                    }
                }
            } else if (caller instanceof ExtractionController) {
                if (getSettings().isDeepExtractionEnabled() && matchesDeepExtractionBlacklist(fileList) == false) {
                    final List<File> files = new ArrayList<File>(fileList.length);
                    for (File file : fileList) {
                        if (getExtractionController(file) == null) {
                            files.add(file);
                        }
                    }
                    if (files.size() > 0) {
                        final ExtractionController con = (ExtractionController) caller;
                        final ArrayList<String> knownPasswords = new ArrayList<String>();
                        final String usedPassword = con.getArchive().getFinalPassword();
                        if (StringUtils.isNotEmpty(usedPassword)) {
                            knownPasswords.add(usedPassword);
                        }
                        final List<String> archiveSettingsPasswords = con.getArchive().getSettings().getPasswords();
                        if (archiveSettingsPasswords != null) {
                            knownPasswords.addAll(archiveSettingsPasswords);
                        }
                        final List<ArchiveFactory> archiveFactories = new ArrayList<ArchiveFactory>(files.size());
                        for (File archiveStartFile : files) {
                            archiveFactories.add(new FileArchiveFactory(archiveStartFile, con.getArchive()));
                        }
                        final List<Archive> newArchives = ArchiveValidator.getArchivesFromPackageChildren(archiveFactories);
                        for (Archive newArchive : newArchives) {
                            if (newArchive != null && isAutoExtractEnabled(newArchive)) {
                                final ArchiveFile firstArchiveFile = newArchive.getArchiveFiles().get(0);
                                if (firstArchiveFile instanceof FileArchiveFile) {
                                    newArchive.getSettings().setExtractPath(((FileArchiveFile) firstArchiveFile).getFile().getParent());
                                }
                                newArchive.getSettings().setPasswords(knownPasswords);
                                addToQueue(caller, newArchive, false);
                            }
                        }
                    }
                }
            } else {
                final List<File> files = new ArrayList<File>(fileList.length);
                for (File file : fileList) {
                    if (getExtractionController(file) == null) {
                        files.add(file);
                    }
                }
                final List<Archive> newArchives = ArchiveValidator.getArchivesFromPackageChildren(files);
                for (Archive newArchive : newArchives) {
                    if (newArchive != null && isAutoExtractEnabled(newArchive)) {
                        addToQueue(caller, newArchive, false);
                    }
                }
            }
        } catch (Exception e) {
            logger.log(e);
        }
    }

    public boolean isAutoExtractEnabled(Archive archive) {
        switch (archive.getSettings().getAutoExtract()) {
        case FALSE:
            return false;
        case TRUE:
            return true;
        case UNSET:
            return CFG_LINKGRABBER.AUTO_EXTRACTION_ENABLED.isEnabled();
        }
        return false;
    }

    public Archive getArchiveByFactory(ArchiveFactory clf) {
        try {
            return buildArchive(clf);
        } catch (ArchiveException e) {
            e.printStackTrace();
            return null;
        }
    }

    public List<Archive> getArchivesFromPackageChildren(List<? extends Object> nodes, Set<String> ignoreArchiveIDs, int maxArchives) {
        final ArrayList<Archive> archives = new ArrayList<Archive>();
        HashSet<String> archiveIDs = null;
        if (ignoreArchiveIDs != null) {
            archiveIDs = new HashSet<String>();
            archiveIDs.addAll(ignoreArchiveIDs);
        }
        buildLoop: for (final Object child : nodes) {
            if (child instanceof CrawledLink) {
                final DownloadLink dlLink = ((CrawledLink) child).getDownloadLink();
                if (dlLink != null && (Boolean.FALSE.equals(dlLink.isPartOfAnArchive()) || (archiveIDs != null && archiveIDs.contains(dlLink.getArchiveID())))) {
                    //
                    continue buildLoop;
                }
            } else if (child instanceof DownloadLink) {
                final DownloadLink dlLink = (DownloadLink) child;
                if (Boolean.FALSE.equals(dlLink.isPartOfAnArchive()) || (archiveIDs != null && archiveIDs.contains(dlLink.getArchiveID()))) {
                    //
                    continue buildLoop;
                }
            } else if (child instanceof ArchiveFactory) {
                final ArchiveFactory af = ((ArchiveFactory) child);
                if (Boolean.FALSE.equals(af.isPartOfAnArchive()) || (archiveIDs != null && archiveIDs.contains(af.getArchiveID()))) {
                    //
                    continue buildLoop;
                }
            }
            for (final Archive archive : archives) {
                if (archive.contains(child)) {
                    continue buildLoop;
                }
            }
            final ArchiveFactory af;
            if (child instanceof CrawledLink) {
                af = new CrawledLinkFactory(((CrawledLink) child));
            } else if (child instanceof DownloadLink) {
                af = new DownloadLinkArchiveFactory(((DownloadLink) child));
            } else if (child instanceof File) {
                af = new FileArchiveFactory(((File) child));
            } else if (child instanceof ArchiveFactory) {
                af = (ArchiveFactory) child;
            } else {
                continue buildLoop;
            }
            final Archive archive = getArchiveByFactory(af);
            if (archive != null) {
                archives.add(archive);
                if (archiveIDs == null) {
                    archiveIDs = new HashSet<String>();
                }
                archiveIDs.add(archive.getArchiveID());
                if (maxArchives > 0 && archives.size() >= maxArchives) {
                    return archives;
                }
            }
        }
        return archives;
    }

    public IfFileExistsAction getIfFileExistsAction(Archive archive) {
        IfFileExistsAction ret = archive.getSettings()._getIfFileExistsAction();
        if (ret == null) {
            ret = getSettings().getIfFileExistsAction();
        }
        return ret;
    }

    public File getFinalExtractToFolder(Archive archive, boolean raw) {
        String path = null;
        if (StringUtils.isEmpty(path)) {
            path = archive.getSettings().getExtractPath();
            if (!StringUtils.isEmpty(path)) {
                /* use customized extracttofolder */
                if (!raw) {
                    path = PackagizerController.replaceDynamicTags(path, ArchiveFactory.PACKAGENAME, null);
                }
                path = archive.getFactory().createExtractSubPath(path, archive);
                File ret = new File(path);
                ret = appendSubFolder(archive, ret);
                return ret;
            }
        }
        if (getSettings().isCustomExtractionPathEnabled()) {
            /* customized extractpath is enabled */
            path = getSettings().getCustomExtractionPath();
        }
        if (StringUtils.isEmpty(path)) {
            /* extractpath is still emptry, create default one */
            path = archive.getFactory().createDefaultExtractToPath(archive);
        }
        if (StringUtils.isEmpty(path)) {
            return null;
        }
        if (!raw) {
            path = PackagizerController.replaceDynamicTags(path, ArchiveFactory.PACKAGENAME, null);
        }
        path = archive.getFactory().createExtractSubPath(path, archive);
        File ret = new File(path);
        ret = appendSubFolder(archive, ret);
        return ret;
    }

    /**
     * @param archive
     * @param ret
     * @return
     */
    protected File appendSubFolder(Archive archive, File ret) {
        if (!getSettings().isSubpathEnabled()) {
            return ret;
        }
        if (archive.getContentView().getFileCount() < getSettings().getSubPathMinFilesTreshhold()) {
            logger.info("No Subfolder because Root contains only " + archive.getContentView().getFileCount() + " files");
            return ret;
        }
        if (archive.getContentView().getDirectoryCount() < getSettings().getSubPathMinFoldersTreshhold()) {
            logger.info("No Subfolder because Root contains only " + archive.getContentView().getDirectoryCount() + " folders");
            return ret;
        }
        if (archive.getContentView().getDirectoryCount() + archive.getContentView().getFileCount() < getSettings().getSubPathMinFilesOrFoldersTreshhold()) {
            logger.info("No Subfolder because Root contains only " + (archive.getContentView().getDirectoryCount() + archive.getContentView().getFileCount()) + " files and folders");
            return ret;
        }
        String sub = getSettings().getSubPath();
        if (!StringUtils.isEmpty(sub)) {
            sub = archive.getFactory().createExtractSubPath(sub, archive);
            if (!StringUtils.isEmpty(sub)) {
                sub = sub.trim();
                ret = new File(ret, sub);
            }
        }
        return ret;
    }

    public Set<IExtraction> getExtractors() {
        return extractors;
    }

    @Override
    public MenuItemData updateMenuModel(ContextMenuManager manager, MenuContainerRoot mr) {
        if (manager instanceof MenuManagerMainToolbar) {
            return updateMainToolbar(mr);
        } else if (manager instanceof MenuManagerMainmenu) {
            return updateMainMenu(mr);
        } else if (manager instanceof MenuManagerDownloadTableContext || manager instanceof MenuManagerLinkgrabberTableContext) {
            int addonLinkIndex = 0;
            for (int i = 0; i < mr.getItems().size(); i++) {
                if (mr.getItems().get(i) instanceof MoreMenuContainer || mr.getItems().get(i) instanceof LinkGrabberMoreSubMenu) {
                    addonLinkIndex = i;
                    break;
                }
            }
            final ArchivesSubMenu root = new ArchivesSubMenu();
            if (manager instanceof MenuManagerDownloadTableContext) {
                root.add(new MenuItemData(new ActionData(ExtractArchiveNowAction.class)));
                root.add(new MenuItemData(new ActionData(AbortExtractionAction.class)));
            }
            root.add(new MenuItemData(new ActionData(ValidateArchivesAction.class)));
            root.add(new SeparatorData());
            root.add(new MenuItemData(new ActionData(AutoExtractEnabledToggleAction.class)));
            root.add(new MenuItemData(new ActionData(SetExtractToAction.class)));
            root.add(new MenuItemData(new ActionData(SetExtractPasswordAction.class)));
            final CleanupSubMenu cleanup = new CleanupSubMenu();
            cleanup.add(new MenuItemData(new ActionData(CleanupAutoDeleteFilesEnabledToggleAction.class)));
            cleanup.add(new MenuItemData(new ActionData(CleanupAutoDeleteLinksEnabledToggleAction.class)));
            root.add(cleanup);
            if (addonLinkIndex != -1) {
                mr.getItems().add(addonLinkIndex, root);
            } else {
                mr.getItems().add(root);
            }
            return null;
        }
        return null;
    }

    private MenuItemData updateMainMenu(MenuContainerRoot mr) {
        ExtensionsMenuContainer container = new ExtensionsMenuContainer();
        container.add(ExtractAction.class);
        return container;
    }

    private MenuItemData updateMainToolbar(MenuContainerRoot mr) {
        OptionalContainer opt = new OptionalContainer(false);
        opt.add(ExtractAction.class);
        return opt;
    }

    @Override
    public List<DownloadLink> onAskToRemovePackage(Object asker, FilePackage pkg, List<DownloadLink> children) {
        return onAskToRemoveChildren(asker, children);
    }

    @Override
    public List<DownloadLink> onAskToRemoveChildren(final Object asker, final List<DownloadLink> children) {
        final ArrayList<DownloadLink> ret = new ArrayList<DownloadLink>(children.size());
        for (final DownloadLink dlink : children) {
            final ExtractionController ec = getExtractionController(dlink);
            if (ec != null) {
                if (asker instanceof ExtractionController && asker == ec) {
                    ret.add(dlink);
                    continue;
                }
                logger.info("Link (" + dlink.toString() + ") is in active Archive do not remove: " + ec.getArchive().getArchiveID());
            } else {
                ret.add(dlink);
            }
        }
        return ret;
    }

    @Override
    public void onNewFolder(Object caller, File folder) {
    }
}