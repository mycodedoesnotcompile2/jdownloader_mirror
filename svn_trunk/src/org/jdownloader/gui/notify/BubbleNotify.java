package org.jdownloader.gui.notify;

import java.awt.Point;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.swing.JFrame;

import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.windowmanager.WindowManager;
import org.appwork.utils.swing.windowmanager.WindowManager.WindowExtendedState;
import org.jdownloader.gui.notify.captcha.CESBubbleSupport;
import org.jdownloader.gui.notify.captcha.CaptchaBubbleSupport;
import org.jdownloader.gui.notify.downloads.StartDownloadsBubbleSupport;
import org.jdownloader.gui.notify.downloads.StartStopPauseBubbleSupport;
import org.jdownloader.gui.notify.gui.AbstractNotifyWindow;
import org.jdownloader.gui.notify.gui.Balloner;
import org.jdownloader.gui.notify.gui.BubbleNotifyConfig.Anchor;
import org.jdownloader.gui.notify.gui.BubbleNotifyConfigPanel;
import org.jdownloader.gui.notify.gui.CFG_BUBBLE;
import org.jdownloader.gui.notify.linkcrawler.LinkCrawlerBubbleSupport;
import org.jdownloader.gui.notify.reconnect.ReconnectBubbleSupport;

import jd.gui.swing.jdgui.JDGui;
import jd.plugins.PluginsC;

public class BubbleNotify {
    private static final BubbleNotify INSTANCE = new BubbleNotify();

    public static interface AbstractNotifyWindowFactory {
        public AbstractNotifyWindow<?> buildAbstractNotifyWindow();
    }

    /**
     * get the only existing instance of BubbleNotify. This is a singleton
     *
     * @return
     */
    public static BubbleNotify getInstance() {
        return BubbleNotify.INSTANCE;
    }

    private final Balloner ballooner;

    /**
     * Create a new instance of BubbleNotify. This is a singleton class. Access the only existing instance by using {@link #getInstance()}.
     */
    private BubbleNotify() {
        if (org.appwork.utils.Application.isHeadless()) {
            ballooner = null;
            return;
        }
        ballooner = new Balloner(null) {
            public JFrame getOwner() {
                if (JDGui.getInstance() == null) {
                    return null;
                }
                return JDGui.getInstance().getMainFrame();
            }
        };
        GenericConfigEventListener<Object> update = new GenericConfigEventListener<Object>() {
            @Override
            public void onConfigValueModified(KeyHandler<Object> keyHandler, Object newValue) {
                ballooner.setScreenID(CFG_BUBBLE.CFG.getScreenID());
                if (CFG_BUBBLE.CFG.getAnimationStartPositionAnchor() == Anchor.SYSTEM_DEFAULT) {
                    switch (CrossSystem.getOS().getFamily()) {
                    case WINDOWS:
                        // bottom right position 10 pixel margin
                        ballooner.setStartPoint(new Point(-11, -1), Anchor.TOP_RIGHT);
                        break;
                    default:
                        // top right position 10 pixel margin
                        ballooner.setStartPoint(new Point(-11, 0), Anchor.BOTTOM_RIGHT);
                    }
                } else {
                    ballooner.setStartPoint(new Point(CFG_BUBBLE.CFG.getAnimationStartPositionX(), CFG_BUBBLE.CFG.getAnimationStartPositionY()), CFG_BUBBLE.CFG.getAnimationStartPositionAnchor());
                }
                if (CFG_BUBBLE.CFG.getAnimationEndPositionAnchor() == Anchor.SYSTEM_DEFAULT) {
                    switch (CrossSystem.getOS().getFamily()) {
                    case WINDOWS:
                        // move out to the right
                        ballooner.setEndPoint(new Point(-1, -11), Anchor.BOTTOM_LEFT);
                        break;
                    default:
                        // move out to the right
                        ballooner.setEndPoint(new Point(-1, 10), Anchor.TOP_LEFT);
                    }
                } else {
                    ballooner.setEndPoint(new Point(CFG_BUBBLE.CFG.getAnimationEndPositionX(), CFG_BUBBLE.CFG.getAnimationEndPositionY()), CFG_BUBBLE.CFG.getAnimationEndPositionAnchor());
                }
                if (CFG_BUBBLE.CFG.getFinalPositionAnchor() == Anchor.SYSTEM_DEFAULT) {
                    switch (CrossSystem.getOS().getFamily()) {
                    case WINDOWS:
                        ballooner.setAnchorPoint(new Point(-11, -11), Anchor.BOTTOM_RIGHT);
                        break;
                    default:
                        ballooner.setAnchorPoint(new Point(-11, 10), Anchor.TOP_RIGHT);
                    }
                } else {
                    ballooner.setAnchorPoint(new Point(CFG_BUBBLE.CFG.getFinalPositionX(), CFG_BUBBLE.CFG.getFinalPositionY()), CFG_BUBBLE.CFG.getFinalPositionAnchor());
                }
            }

            @Override
            public void onConfigValidatorError(KeyHandler<Object> keyHandler, Object invalidValue, ValidationException validateException) {
            }
        };
        CFG_BUBBLE.CFG._getStorageHandler().getEventSender().addListener(update);
        update.onConfigValueModified(null, null);
        types.add(new LinkCrawlerBubbleSupport());
        types.add(new UpdatesBubbleSupport());
        types.add(new ReconnectBubbleSupport());
        types.add(new CaptchaBubbleSupport());
        types.add(new StartDownloadsBubbleSupport());
        types.add(new StartStopPauseBubbleSupport());
        types.add(PluginsC.getBubbleSupportInstance());
        types.add(CESBubbleSupport.getInstance());
    }

    private boolean isBubbleNotificationEnabled() {
        if (JDGui.getInstance().isSilentModeActive() && !CFG_BUBBLE.BUBBLE_NOTIFY_ENABLED_DURING_SILENT_MODE.isEnabled()) {
            return false;
        } else {
            switch (CFG_BUBBLE.CFG.getBubbleNotifyEnabledState()) {
            case ALWAYS:
                return true;
            case NEVER:
                return false;
            case JD_ACTIVE:
                return WindowManager.getInstance().hasFocus();
            case JD_NOT_ACTIVE:
                return !WindowManager.getInstance().hasFocus();
            case TASKBAR:
                return WindowManager.getInstance().getExtendedState(JDGui.getInstance().getMainFrame()) == WindowExtendedState.ICONIFIED;
            case TRAY:
                return !JDGui.getInstance().getMainFrame().isVisible();
            case TRAY_OR_TASKBAR:
                if (WindowManager.getInstance().getExtendedState(JDGui.getInstance().getMainFrame()) == WindowExtendedState.ICONIFIED) {
                    return true;
                } else if (!JDGui.getInstance().getMainFrame().isVisible()) {
                    return true;
                } else {
                    return false;
                }
            default:
                return false;
            }
        }
    }

    public void show(final AbstractNotifyWindow no) {
        if (ballooner == null) {
            return;
        }
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                boolean added = false;
                try {
                    if (isBubbleNotificationEnabled()) {
                        ballooner.add(no);
                        added = true;
                    }
                } finally {
                    if (added == false) {
                        /*
                         * creating a window and not disposing it = mem leak! in case we do not enqueue the window, we dispose it now!
                         */
                        no.dispose();
                    }
                }
            }
        };
    }

    public void show(final AbstractNotifyWindowFactory factory) {
        if (ballooner == null) {
            return;
        } else if (factory == null) {
            return;
        }
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                boolean added = false;
                AbstractNotifyWindow<?> notifyWindow = null;
                try {
                    if (!isBubbleNotificationEnabled()) {
                        return;
                    }
                    notifyWindow = factory.buildAbstractNotifyWindow();
                    if (notifyWindow != null) {
                        ballooner.add(notifyWindow);
                        added = true;
                    }
                } finally {
                    if (added == false && notifyWindow != null) {
                        /*
                         * creating a window and not disposing it = mem leak! in case we do not enqueue the window, we dispose it now!
                         */
                        notifyWindow.dispose();
                    }
                }
            }
        };
    }

    public void hide(final AbstractNotifyWindow notify) {
        if (ballooner == null) {
            return;
        } else if (notify == null) {
            return;
        }
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                ballooner.hide(notify);
            }
        };
    }

    public void relayout() {
        if (ballooner == null) {
            return;
        }
        ballooner.relayout();
    }

    public void unregisterTypes(AbstractBubbleSupport type) {
        if (ballooner == null) {
            return;
        }
        if (types.remove(type)) {
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    if (configPanel != null) {
                        configPanel.updateTypes(getTypes());
                    }
                }
            };
        }
    }

    public void registerType(AbstractBubbleSupport type) {
        if (ballooner == null) {
            return;
        }
        types.add(type);
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                if (configPanel != null) {
                    configPanel.updateTypes(getTypes());
                }
            }
        };
    }

    private final CopyOnWriteArrayList<AbstractBubbleSupport> types = new CopyOnWriteArrayList<AbstractBubbleSupport>();
    private BubbleNotifyConfigPanel                           configPanel;

    public List<AbstractBubbleSupport> getTypes() {
        return types;
    }

    public BubbleNotifyConfigPanel getConfigPanel() {
        if (ballooner == null) {
            return null;
        }
        if (configPanel == null) {
            configPanel = new BubbleNotifyConfigPanel();
        }
        return configPanel;
    }
}
