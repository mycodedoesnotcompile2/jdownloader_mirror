/**
 *
 * ====================================================================================================================================================

 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.swing.dialog;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dialog.ModalityType;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsDevice;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.awt.event.WindowListener;
import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.WeakHashMap;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.storage.JSonStorage;
import org.appwork.swing.MigPanel;
import org.appwork.swing.RelayoutOnScreenScaleUpdate;
import org.appwork.uio.CloseReason;
import org.appwork.uio.UIOManager;
import org.appwork.uio.UserIODefinition;
import org.appwork.utils.Application;
import org.appwork.utils.BinaryLogic;
import org.appwork.utils.DebugMode;
import org.appwork.utils.NullsafeAtomicReference;
import org.appwork.utils.ImageProvider.ImageProvider;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.locale._AWU;
import org.appwork.utils.logging2.LogInterface;
import org.appwork.utils.net.Base64OutputStream;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.dimensor.DialogDimensor;
import org.appwork.utils.swing.dialog.locator.CenterOfScreenDialogLocator;
import org.appwork.utils.swing.dialog.locator.DialogLocator;
import org.appwork.utils.swing.windowmanager.WindowManager;
import org.appwork.utils.swing.windowmanager.WindowManager.FrameState;

public abstract class AbstractDialog<T> implements ActionListener, WindowListener, OKCancelCloseUserIODefinition, WindowFocusListener, ComponentListener {
    protected static int                          BUTTON_HEIGHT           = -1;
    public static DialogLocator                   DEFAULT_LOCATOR         = null;
    public static final DialogLocator             LOCATE_CENTER_OF_SCREEN = new CenterOfScreenDialogLocator();
    private static final HashMap<String, Integer> SESSION_DONTSHOW_AGAIN  = new HashMap<String, Integer>();
    public static final OwnerFinder               DEFAULT_OWNER_FINDER    = new OwnerFinder() {
                                                                              @Override
                                                                              public Window findDialogOwner(final AbstractDialog<?> dialogModel, final WindowStack windowStack) {
                                                                                  Window ret;
                                                                                  for (int i = windowStack.size() - 1; i >= 0; i++) {
                                                                                      ret = windowStack.get(i);
                                                                                      if (ret instanceof Frame || ret instanceof java.awt.Dialog) {
                                                                                          return ret;
                                                                                      }
                                                                                  }
                                                                                  return null;
                                                                              }
                                                                          };
    private static OwnerFinder                    OWNER_FINDER            = AbstractDialog.DEFAULT_OWNER_FINDER;
    public static WindowZHandler                  ZHANDLER                = new BasicZHandler();

    public static void setZHandler(WindowZHandler zHANDLER) {
        ZHANDLER = zHANDLER;
    }

    private static final WeakHashMap<Object, WindowStack> STACK_MAP = new WeakHashMap<Object, WindowStack>();
    /**
     * @param desiredRootFrame
     * @return
     */
    private static final Object                           NULL_KEY  = new Object();

    public static int getButtonHeight() {
        return AbstractDialog.BUTTON_HEIGHT;
    }

    public static DialogLocator getDefaultLocator() {
        return AbstractDialog.DEFAULT_LOCATOR;
    }

    protected LogInterface getLogger() {
        return org.appwork.loggingv3.LogV3.I().getDefaultLogger();
    }

    /**
     * @return
     */
    public static Window getDefaultRoot() {
        final WindowStack stack = AbstractDialog.getWindowStackByRoot(null);
        for (Window w : stack) {
            if (w instanceof Frame || w instanceof java.awt.Dialog) {
                return w;
            }
        }
        return null;
    }

    /**
     * @return
     */
    public static OwnerFinder getGlobalOwnerFinder() {
        return AbstractDialog.OWNER_FINDER;
    }

    public void componentResized(ComponentEvent e) {
        final DialogDimensor dim = getDimensor();
        if (dim != null) {
            dim.onClose(AbstractDialog.this, e);
        }
    }

    /**
     * Invoked when the component's position changes.
     */
    public void componentMoved(ComponentEvent e) {
        final DialogLocator loc = getLocator();
        if (loc != null) {
            loc.onClose(AbstractDialog.this, e);
        }
    }

    /**
     * Invoked when the component has been made visible.
     */
    public void componentShown(ComponentEvent e) {
    }

    /**
     * Invoked when the component has been made invisible.
     */
    public void componentHidden(ComponentEvent e) {
    }

    public static Integer getSessionDontShowAgainValue(final String key) {
        synchronized (AbstractDialog.SESSION_DONTSHOW_AGAIN) {
            final Integer ret = AbstractDialog.SESSION_DONTSHOW_AGAIN.get(key);
            if (ret == null) {
                return -1;
            }
            return ret;
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.uio.UserIODefinition#isRemoteAPIEnabled()
     */
    @Override
    public boolean isRemoteAPIEnabled() {
        return false;
    }

    private static WindowStack getWindowStackByRoot(final Window desiredRootFrame) {
        Object key = desiredRootFrame;
        if (key == null || !desiredRootFrame.isVisible()) {
            key = AbstractDialog.NULL_KEY;
        }
        WindowStack ret = AbstractDialog.STACK_MAP.get(key);
        if (ret == null) {
            ret = new WindowStack(desiredRootFrame);
            AbstractDialog.STACK_MAP.put(key, ret);
        }
        return ret;
    }

    public static void resetDialogInformations() {
        try {
            synchronized (AbstractDialog.SESSION_DONTSHOW_AGAIN) {
                AbstractDialog.SESSION_DONTSHOW_AGAIN.clear();
            }
            JSonStorage.getPlainStorage("Dialogs").clear();
        } catch (final Exception e) {
            org.appwork.loggingv3.LogV3.log(e);
        }
    }

    /**
     * @param i
     */
    public static void setButtonHeight(final int height) {
        AbstractDialog.BUTTON_HEIGHT = height;
    }

    public static void setDefaultLocator(final DialogLocator dEFAULT_LOCATOR) {
        AbstractDialog.DEFAULT_LOCATOR = dEFAULT_LOCATOR;
    }

    /**
     * @param owner
     * @return
     */
    public final static <T extends Window> T ensureFrameDialogOrNull(T owner) {
        if (owner == null) {
            return null;
        } else if (owner instanceof java.awt.Frame || owner instanceof java.awt.Dialog) {
            RelayoutOnScreenScaleUpdate.init(owner);
            return owner;
        } else {
            org.appwork.loggingv3.LogV3.log(new Exception("Warning! Bad Owner Frame Detected - fallback to null: " + printTypeHirarchy(owner.getClass())));
            return null;
        }
    }

    private final static String printTypeHirarchy(Class<?> class1) {
        if (class1 == null) {
            return "";
        } else {
            return class1.getName() + " << " + printTypeHirarchy(class1.getSuperclass());
        }
    }

    /**
     * @param frame
     */
    public static void setDefaultRoot(final Window root) {
        final Window defaultRoot = ensureFrameDialogOrNull(root);
        if (root != null && root != defaultRoot) {
            throw new IllegalStateException(root + " is not a Frame or Dialog:" + printTypeHirarchy(root.getClass()));
        } else {
            AbstractDialog.getWindowStackByRoot(null).reset(defaultRoot);
        }
    }

    public static void setGlobalOwnerFinder(final OwnerFinder finder) {
        AbstractDialog.OWNER_FINDER = finder == null ? AbstractDialog.DEFAULT_OWNER_FINDER : finder;
    }

    protected AbstractAction[]   actions                = null;
    protected JButton            cancelButton;
    private final String         cancelButtonText;
    private boolean              countdownPausable      = true;
    protected FocusListener      defaultButtonFocusListener;
    protected DefaultButtonPanel defaultButtons;
    protected InternDialog<T>    dialog;
    private DialogDimensor       dimensor;
    protected boolean            disposed               = false;
    protected boolean            doNotShowAgainSelected = false;
    protected JCheckBox          dontshowagain;
    private boolean              dummyInit              = false;
    protected int                flagMask;
    private Icon                 icon;
    protected JLabel             iconLabel;
    private volatile boolean     initialized            = false;
    private DialogLocator        locator;
    protected JButton            okButton;

    public JButton getCancelButton() {
        return cancelButton;
    }

    public JButton getOkButton() {
        return okButton;
    }

    protected String                                       okButtonText;
    private Point                                          orgLocationOnScreen;
    protected JComponent                                   panel;
    protected Dimension                                    preferredSize;
    protected int                                          returnBitMask = 0;
    private int                                            timeout       = 0;
    /**
     * Timer Thread to count down the {@link #counter}
     */
    protected NullsafeAtomicReference<AbstractTimerThread> timer         = new NullsafeAtomicReference<AbstractTimerThread>(null);
    /**
     * Label to display the timervalue
     */
    protected JLabel                                       timerLbl;
    private String                                         title;
    private DisposeCallBack                                disposeCallBack;
    private boolean                                        callerIsEDT   = false;
    protected JComponent                                   focusButton;
    // document modal:
    // if there are several window stacks, the dialog blocks only it's own
    // windowstack.
    private ModalityType                                   modalityType  = ModalityType.DOCUMENT_MODAL;
    private volatile RuntimeException                      layoutException;

    protected RuntimeException getLayoutException() {
        return layoutException;
    }

    public void setModalityType(ModalityType modalityType) {
        this.modalityType = modalityType;
    }

    public AbstractDialog(final int flag, final String title, final Icon icon, final String okOption, final String cancelOption) {
        super();
        this.title = title;
        this.flagMask = flag;
        this.icon = BinaryLogic.containsAll(flag, Dialog.STYLE_HIDE_ICON) ? null : icon;
        this.okButtonText = okOption == null ? _AWU.T.ABSTRACTDIALOG_BUTTON_OK() : okOption;
        this.cancelButtonText = cancelOption == null ? _AWU.T.ABSTRACTDIALOG_BUTTON_CANCEL() : cancelOption;
    }

    /**
     * this function will init and show the dialog
     */
    protected void _init() {
        WindowStack windowStack = null;
        InternDialog<T> dialog = null;
        try {
            this.layoutDialog();
            if (BinaryLogic.containsAll(this.flagMask, UIOManager.LOGIC_COUNTDOWN)) {
                this.timerLbl.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(final MouseEvent e) {
                        AbstractDialog.this.cancel();
                        AbstractDialog.this.timerLbl.removeMouseListener(this);
                    }
                });
                this.timerLbl.setToolTipText(_AWU.T.TIMERDIALOG_TOOLTIP_TIMERLABEL());
                this.timerLbl.setIcon(DialogIcon.DIALOG_CANCEL.get(16));
            }
            this.setTitle(this.title);
            if (this.evaluateDontShowAgainFlag()) {
                return;
            }
            // final Container parent = getDialog().getParent();
            //
            // if (parent == null || !parent.isShowing()) {
            // // final Window main = getRootFrame();
            // // if (main != null) {
            // // main.addWindowFocusListener(new WindowFocusListener() {
            // //
            // // @Override
            // // public void windowGainedFocus(final WindowEvent e) {
            // // SwingUtils.toFront(getDialog());
            // // main.removeWindowFocusListener(this);
            // //
            // // }
            // //
            // // @Override
            // // public void windowLostFocus(final WindowEvent e) {
            // //
            // // }
            // // });
            // // }
            // // // getDialog().setAlwaysOnTop(true);
            // }
            // Layout manager
            // Dispose dialog on close
            this.getDialog().setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
            this.getDialog().addWindowListener(this);
            this.defaultButtonFocusListener = new FocusListener() {
                @Override
                public void focusGained(final FocusEvent e) {
                    final JRootPane root = SwingUtilities.getRootPane(e.getComponent());
                    if (root != null && e.getComponent() instanceof JButton) {
                        root.setDefaultButton((JButton) e.getComponent());
                    }
                }

                @Override
                public void focusLost(final FocusEvent e) {
                    final JRootPane root = SwingUtilities.getRootPane(e.getComponent());
                    if (root != null) {
                        root.setDefaultButton(null);
                    }
                }
            };
            // create panel for the dialog's buttons
            this.okButton = this.createOkButton();
            this.cancelButton = new JButton(this.cancelButtonText);
            this.cancelButton.addFocusListener(this.defaultButtonFocusListener);
            this.okButton.addFocusListener(this.defaultButtonFocusListener);
            this.defaultButtons = this.getDefaultButtonPanel();
            /*
             * We set the focus on the ok button. if no ok button is shown, we set the focus on cancel button
             */
            JButton focus = null;
            // add listeners here
            this.okButton.addActionListener(this);
            this.cancelButton.addActionListener(this);
            // add icon if available
            if (this.icon != null) {
                this.getDialog().setLayout(new MigLayout("ins 5,wrap 2", "[][grow,fill]", "[grow,fill][]"));
                this.getDialog().add(this.getIconComponent(), this.getIconConstraints());
            } else {
                this.getDialog().setLayout(new MigLayout("ins 5,wrap 1", "[grow,fill]", "[grow,fill][]"));
            }
            // Layout the dialog content and add it to the contentpane
            this.panel = this.layoutDialogContent();
            this.getDialog().add(this.panel, "");
            // add the countdown timer
            final MigPanel bottom = this.createBottomPanel();
            bottom.setOpaque(false);
            bottom.add(this.timerLbl);
            if (BinaryLogic.containsAll(this.flagMask, Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN)) {
                this.initDoNotShowAgainCheckbox(bottom);
            } else {
                bottom.add(Box.createHorizontalGlue());
            }
            bottom.add(this.defaultButtons);
            if ((this.flagMask & UIOManager.BUTTONS_HIDE_OK) == 0) {
                // Set OK as defaultbutton
                this.getDialog().getRootPane().setDefaultButton(this.okButton);
                this.okButton.addHierarchyListener(new HierarchyListener() {
                    public void hierarchyChanged(final HierarchyEvent e) {
                        if ((e.getChangeFlags() & HierarchyEvent.PARENT_CHANGED) != 0) {
                            final JButton defaultButton = (JButton) e.getComponent();
                            final JRootPane root = SwingUtilities.getRootPane(defaultButton);
                            if (root != null) {
                                root.setDefaultButton(defaultButton);
                            }
                        }
                    }
                });
                focus = this.okButton;
                this.defaultButtons.addOKButton(this.okButton);
            }
            if (!BinaryLogic.containsAll(this.flagMask, UIOManager.BUTTONS_HIDE_CANCEL)) {
                this.defaultButtons.addCancelButton(this.cancelButton);
                if (BinaryLogic.containsAll(this.flagMask, UIOManager.BUTTONS_HIDE_OK)) {
                    this.getDialog().getRootPane().setDefaultButton(this.cancelButton);
                    // focus is on cancel if OK is hidden
                    focus = this.cancelButton;
                }
            }
            this.addButtons(this.defaultButtons);
            initTimerLabel();
            this.getDialog().add(bottom, "spanx,growx,pushx");
            // pack dialog
            this.getDialog().invalidate();
            // this.setMinimumSize(this.getPreferredSize());
            this.getDialog().setResizable(this.isResizable());
            // minimum size foir a dialog
            // // Dimension screenDim =
            // Toolkit.getDefaultToolkit().getScreenSize();
            // this.setMaximumSize(Toolkit.getDefaultToolkit().getScreenSize());
            // if (this.getDesiredSize() != null) {
            // this.setSize(this.getDesiredSize());
            // }
            // some dialogs may have autowrapping textfields. these textfields
            // need a few resizing rounds to find it's final dimension
            final DialogDimensor dimensor = getDimensor();
            final Dimension preferredSize;
            if (this.preferredSize != null) {
                preferredSize = this.preferredSize;
            } else {
                preferredSize = dimensor != null ? dimensor.getDimension(AbstractDialog.this) : null;
            }
            if (preferredSize == null && CrossSystem.isWindows()) {
                AbstractDialog.this.getDialog().getContentPane().addComponentListener(new ComponentListener() {
                    @Override
                    public void componentHidden(final ComponentEvent e) {
                    }

                    @Override
                    public void componentMoved(final ComponentEvent e) {
                    }

                    @Override
                    public void componentResized(final ComponentEvent e) {
                        final Dimension size = AbstractDialog.this.getDialog().getSize();
                        AbstractDialog.this.getDialog().pack();
                        final Dimension after = AbstractDialog.this.getDialog().getSize();
                        if (after.equals(size)) {
                            AbstractDialog.this.getDialog().getContentPane().removeComponentListener(this);
                        }
                    }

                    @Override
                    public void componentShown(final ComponentEvent e) {
                    }
                });
            }
            if (preferredSize != null) {
                this.getDialog().setPreferredSize(preferredSize);
            }
            this.pack();
            // register an escape listener to cancel the dialog
            this.focusButton = focus;
            this.registerEscape();
            this.packed();
            // find location AFTER dimensor - the locator needs the dimensions in order to find "Center of X" Locations
            Point location = null;
            final DialogLocator locator = getLocator();
            if (locator != null) {
                try {
                    location = locator.getLocationOnScreen(this);
                } catch (final Exception e) {
                    getLogger().log(e);
                }
            }
            if (location == null) {
                try {
                    location = AbstractDialog.LOCATE_CENTER_OF_SCREEN.getLocationOnScreen(this);
                } catch (final Exception e) {
                    getLogger().log(e);
                }
            }
            if (location != null) {
                this.getDialog().setLocation(location);
            }
            /*
             * workaround a javabug that forces the parentframe to stay always on top
             */
            // Disabled on 14.06.2013:
            // This peace of code causes the parent to come on top even if we do
            // not want or need it
            // In our case, we do not want the captcha dialogs causing the
            // mainframe to get on top.
            // i think that this piece of code is a workaround for always on top
            // bugs we had years ago.
            //
            // if (getDialog().getParent() != null && !CrossSystem.isMac()) {
            // ((Window) getDialog().getParent()).setAlwaysOnTop(true);
            // ((Window) getDialog().getParent()).setAlwaysOnTop(false);
            // }
            this.getDialog().addComponentListener(new ComponentAdapter() {
                private boolean workaroundInstalled = false;

                /*
                 * (non-Javadoc)
                 *
                 * @see java.awt.event.ComponentAdapter#componentMoved(java.awt.event.ComponentEvent)
                 */
                @Override
                public void componentMoved(ComponentEvent e) {
                    if (orgLocationOnScreen == null) {
                        try {
                            AbstractDialog.this.orgLocationOnScreen = AbstractDialog.this.getDialog().getLocationOnScreen();
                        } catch (java.awt.IllegalComponentStateException e1) {
                            // https://projects.appwork.org/issues/247
                            // java.awt.IllegalComponentStateException: component must be showing on the screen to determine its location
                        }
                    }
                    if (orgLocationOnScreen != null && workaroundInstalled) {
                        getDialog().removeComponentListener(this);
                    }
                }

                @Override
                public void componentShown(final ComponentEvent e) {
                    try {
                        AbstractDialog.this.orgLocationOnScreen = AbstractDialog.this.getDialog().getLocationOnScreen();
                    } catch (java.awt.IllegalComponentStateException e1) {
                        // https://projects.appwork.org/issues/247
                        // java.awt.IllegalComponentStateException: component must be showing on the screen to determine its location
                    }
                    workaroundInstalled = true;
                    if (CrossSystem.isUnix()) {
                        AbstractDialog.this.getDialog().getContentPane().addComponentListener(new ComponentAdapter() {
                            @Override
                            public void componentResized(final ComponentEvent e) {
                                final Dimension size = AbstractDialog.this.getDialog().getSize();
                                AbstractDialog.this.getDialog().pack();
                                final Dimension after = AbstractDialog.this.getDialog().getSize();
                                if (after.equals(size)) {
                                    AbstractDialog.this.getDialog().getContentPane().removeComponentListener(this);
                                }
                            }
                        });
                    } else if (CrossSystem.isMac()) {
                        AbstractDialog.this.getDialog().pack();
                    }
                    if (orgLocationOnScreen != null && workaroundInstalled) {
                        getDialog().removeComponentListener(this);
                    }
                }
            });
            dialog = this.getDialog();
            dialog.addWindowFocusListener(this);
            windowStack = AbstractDialog.getWindowStackByRoot(this.getDesiredRootFrame());
            windowStack.add(dialog);
            // System.out.println("Window Stack Before " + windowStack.size());
            for (final Window w : windowStack) {
                if (w == null) {
                    // System.out.println("Window null");
                } else {
                    // System.out.println(w.getName() + " - " + w);
                }
            }
            try {
                Rectangle dialogBounds = dialog.getBounds();
                // GraphicsDevice screen = SwingUtils.getScreenByBounds(dialogBounds);
                boolean fullyVisible = SwingUtils.isRectangleFullyDisplayableOnScreens(dialogBounds);
                if (!fullyVisible) {
                    GraphicsDevice screenWithMajorIntersection = SwingUtils.getScreenByBounds(dialogBounds);
                    Rectangle usableBounds = SwingUtils.getUsableScreenBounds(screenWithMajorIntersection);
                    if (locator == null) {
                        Point old = dialog.getLocation();
                        dialog.setLocation(Math.max(old.x, usableBounds.x), Math.max(old.y, usableBounds.y));
                    } else {
                        // the location has already been fixed in the locator
                    }
                    Point newLoc = dialog.getLocation();
                    Dimension pref = dialog.getPreferredSize();
                    Dimension newDim = new Dimension(Math.min(pref.width, usableBounds.x + usableBounds.width - newLoc.x), Math.min(pref.height, usableBounds.y + usableBounds.height - newLoc.y));
                    LogV3.info(this, "Fixed Dimensions for dialog from %s to %s", dialogBounds, newDim);
                    dialog.setPreferredSize(newDim);
                    dialog.pack();
                }
            } catch (Exception e) {
                // try catch as long as we are sure that this cannot throw anything
            }
            this.setVisible(true);
            // if the dt has been interrupted,s setVisible will return even for
            // modal dialogs
            // however the dialog will stay open. Make sure to close it here
            // dialog gets closed
            // 17.11.2011 I did not comment this - may be debug code while
            // finding the problem with dialogs with closed parent...s
            // this code causes a dialog which gets disposed without setting
            // return mask to appear again.
            // //System.out.println("Unlocked " +
            // this.getDialog().isDisplayable());
            //
            // if (this.returnBitMask == 0) {
            // this.setVisible(true);
            // org.appwork.loggingv3.LogV3.fine("Answer: Parent Closed ");
            // this.returnBitMask |= Dialog.RETURN_CLOSED;
            // this.setVisible(false);
            //
            // this.dispose();
            // }
        } catch (RuntimeException e) {
            this.layoutException = e;
            throw e;
        } finally {
            if (this.getDialog().getModalityType() != ModalityType.MODELESS) {
                this.dispose();
            } else {
                if (windowStack != null && dialog != null) {
                    final int i = windowStack.lastIndexOf(dialog);
                    if (i >= 0) {
                        windowStack.remove(i);
                        // System.out.println("Window Stack After " +
                        // windowStack.size());
                        for (final Window w : windowStack) {
                            if (w == null) {
                                // System.out.println("Window null");
                            } else {
                                // System.out.println(w.getName() + " - " + w);
                            }
                        }
                    }
                }
            }
        }
        /*
         * workaround a javabug that forces the parentframe to stay always on top
         */
        // Disabled on 14.06.2013:
        // This peace of code causes the parent to come on top even if we do not
        // want or need it
        // In our case, we do not want the captcha dialogs causing the mainframe
        // to get on top.
        // i think that this piece of code is a workaround for always on top
        // bugs we had years ago.
        // if (getDialog().getParent() != null && !CrossSystem.isMac()) {
        // ((Window) getDialog().getParent()).setAlwaysOnTop(true);
        // ((Window) getDialog().getParent()).setAlwaysOnTop(false);
        // }
    }

    protected void initTimerLabel() {
        if (BinaryLogic.containsAll(this.flagMask, UIOManager.LOGIC_COUNTDOWN)) {
            // show timer
            this.initTimer();
        } else {
            this.timerLbl.setText(null);
        }
    }

    public void actionPerformed(final ActionEvent e) {
        if (e.getSource() == this.okButton) {
            getLogger().fine("Answer: Button<OK:" + this.okButton.getText() + ">");
            this.setReturnmask(true);
        } else if (e.getSource() == this.cancelButton) {
            getLogger().fine("Answer: Button<CANCEL:" + this.cancelButton.getText() + ">");
            this.setReturnmask(false);
        }
        this.dispose();
    }

    /**
     * Overwrite this method to add additional buttons
     */
    protected void addButtons(final JPanel buttonBar) {
    }

    /**
     * interrupts the timer countdown
     */
    public void cancel() {
        if (!this.isCountdownPausable()) {
            return;
        }
        this.stopTimer();
    }

    /**
     * called when user closes the window
     *
     * @return <code>true</code>, if and only if the dialog should be closeable
     **/
    public boolean closeAllowed() {
        return true;
    }

    /**
     * @return
     */
    protected DefaultButtonPanel createBottomButtonPanel() {
        if (AbstractDialog.BUTTON_HEIGHT <= 0) {
            return createButtonPanelImpl("ins 0", "[]", "0[grow,fill]0");
        } else {
            return createButtonPanelImpl("ins 0", "[]", "0[grow,fill," + AbstractDialog.BUTTON_HEIGHT + "!]0");
        }
    }

    /**
     * @param string
     * @param string2
     * @param string3
     * @return
     */
    protected DefaultButtonPanel createButtonPanelImpl(String ins, String columns, String rows) {
        return new DefaultButtonPanel(ins, columns, rows);
    }

    /**
     * @return
     */
    protected MigPanel createBottomPanel() {
        return new MigPanel("ins 0", "[]20[grow,fill][]", "[]");
    }

    protected JButton createOkButton() {
        return new JButton(getOKButtonText());
    }

    protected abstract T createReturnValue();

    /**
     * This method has to be called to display the dialog. make sure that all settings have beens et before, becvause this call very likly
     * display a dialog that blocks the rest of the gui until it is closed
     */
    public void displayDialog() {
        if (this.initialized) {
            getLogger().info("Already Initialized. Tried to re-display a dialog?");
            return;
        }
        getLogger().info("Display Dialog: " + this.getClass().getName() + ": " + this);
        this.initialized = true;
        this._init();
    }

    public void dispose() {
        if (this.dummyInit && this.dialog == null) {
            return;
        }
        if (!this.initialized) {
            throw new IllegalStateException("Dialog has not been initialized yet. call displayDialog()");
        }
        DebugMode.breakIf(getReturnmask() == 0, "YOu should set a returnmask first");
        AbstractDialog.this.stopTimer();
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                final WindowStack windowStack = AbstractDialog.getWindowStackByRoot(AbstractDialog.this.getDesiredRootFrame());
                final int i = windowStack.lastIndexOf(AbstractDialog.this.getDialog());
                if (i >= 0) {
                    windowStack.remove(i);
                    // System.out.println("Window Stack After " +
                    // windowStack.size());
                    for (final Window w : windowStack) {
                        if (w == null) {
                            // System.out.println("Window null");
                        } else {
                            // System.out.println(w.getName() + " - " + w);
                        }
                    }
                    if (windowStack.size() > 0) {
                        Window lastActive = windowStack.get(windowStack.size() - 1);
                        lastActive.requestFocus();
                    }
                }
                if (AbstractDialog.this.isDisposed()) {
                    return;
                }
                AbstractDialog.this.setDisposed(true);
                if (AbstractDialog.this.getDialog().isVisible()) {
                    final DialogLocator locator = getLocator();
                    if (locator != null) {
                        try {
                            locator.onClose(AbstractDialog.this, new WindowEvent(getDialog(), WindowEvent.WINDOW_CLOSING));
                        } catch (final Exception e) {
                            getLogger().log(e);
                        }
                    }
                    try {
                        final DialogDimensor dimensor = getDimensor();
                        if (dimensor != null) {
                            dimensor.onClose(AbstractDialog.this, new WindowEvent(getDialog(), WindowEvent.WINDOW_CLOSING));
                        }
                    } catch (final Exception e) {
                        getLogger().log(e);
                    }
                }
                // try to avoid java.lang.IllegalArgumentException: Window must
                // not be zero for linux.
                // 1. set Invisible before disposing the frame
                AbstractDialog.this.getDialog().setVisible(false);
                // 2. Dispose
                AbstractDialog.this.getDialog().realDispose();
                // 3. set disposed to true afterwards
                AbstractDialog.this.setDisposed(true);
            }
        };
    }

    /**
     * @return
     */
    public boolean evaluateDontShowAgainFlag() {
        if (this.isDontShowAgainFlagEabled()) {
            final String key = this.getDontShowAgainKey();
            if (key != null) {
                // bypass if key is null. this enables us to show don't show
                // again checkboxes, but handle the result extern.
                try {
                    final int i = BinaryLogic.containsAll(this.flagMask, UIOManager.LOGIC_DONT_SHOW_AGAIN_DELETE_ON_EXIT) ? AbstractDialog.getSessionDontShowAgainValue(key) : JSonStorage.getPlainStorage("Dialogs").get(key, -1);
                    if (i >= 0) {
                        // filter saved return value
                        int ret = i & (Dialog.RETURN_OK | Dialog.RETURN_CANCEL);
                        // add flags
                        ret |= Dialog.RETURN_DONT_SHOW_AGAIN | Dialog.RETURN_SKIPPED_BY_DONT_SHOW;
                        /*
                         * if LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL or LOGIC_DONT_SHOW_AGAIN_IGNORES_OK are used, we check here if we should
                         * handle the dont show again feature
                         */
                        if (BinaryLogic.containsAll(this.flagMask, UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL) && BinaryLogic.containsAll(ret, Dialog.RETURN_CANCEL)) {
                            return false;
                        }
                        if (BinaryLogic.containsAll(this.flagMask, UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_OK) && BinaryLogic.containsAll(ret, Dialog.RETURN_OK)) {
                            return false;
                        }
                        if (this.isDeveloperMode() && this.isDisposed() && this.returnBitMask != ret) {
                            throw new IllegalStateException("Dialog already disposed");
                        }
                        this.returnBitMask = ret;
                        return true;
                    }
                } catch (final Exception e) {
                    getLogger().log(e);
                }
            }
        }
        return false;
    }

    /**
     * @param e
     */
    public void fillReturnMask(final DialogClosedException e) {
        if (this.returnBitMask < 0) {
            this.returnBitMask = 0;
        }
        if (e.isCausedByClosed()) {
            this.returnBitMask |= Dialog.RETURN_CLOSED;
        }
        if (e.isCausedbyESC()) {
            this.returnBitMask |= Dialog.RETURN_CANCEL;
        }
        if (e.isCausedByInterrupt()) {
            this.returnBitMask |= Dialog.RETURN_INTERRUPT;
        }
        if (e.isCausedByTimeout()) {
            this.returnBitMask |= Dialog.RETURN_TIMEOUT;
        }
    }

    /**
     * Fakes an init of the dialog. we need this if we want to work with the model only.
     */
    public void forceDummyInit() {
        if (Application.isHeadless()) {
            getLogger().info("Force Dummy Init");
            AbstractDialog.this.initialized = true;
            AbstractDialog.this.dummyInit = true;
        } else {
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    getLogger().info("Force Dummy Init");
                    AbstractDialog.this.initialized = true;
                    AbstractDialog.this.dummyInit = true;
                }
            }.waitForEDT();
        }
    }

    /**
     * @return
     */
    protected Color getBackground() {
        return this.getDialog().getBackground();
    }

    public String getCancelButtonText() {
        return this.cancelButtonText;
    }

    @Override
    public CloseReason getCloseReason() {
        final int rm = this.getReturnmask();
        if (rm == 0) {
            // dialog got disposed without setting the returnmask - for example by closing the dialog's parent
            throw new IllegalStateException("Dialog has not been closed yet");
        } else if (BinaryLogic.containsSome(rm, Dialog.RETURN_TIMEOUT)) {
            return CloseReason.TIMEOUT;
        } else if (BinaryLogic.containsSome(rm, Dialog.RETURN_INTERRUPT)) {
            return CloseReason.INTERRUPT;
        } else if (BinaryLogic.containsSome(rm, Dialog.RETURN_CLOSED)) {
            return CloseReason.CLOSE;
        } else if (BinaryLogic.containsSome(rm, Dialog.RETURN_CANCEL)) {
            return CloseReason.CANCEL;
        } else if (BinaryLogic.containsSome(rm, Dialog.RETURN_OK)) {
            return CloseReason.OK;
        } else {
            throw new WTFException("Returnmask:" + rm);
        }
    }

    /**
     * @return the timeout a dialog actually should display in ms
     */
    public long getCountdown() {
        return this.getTimeout() > 0 ? this.getTimeout() : Dialog.getInstance().getDefaultTimeout();
    }

    /**
     * @return
     */
    protected DefaultButtonPanel getDefaultButtonPanel() {
        final DefaultButtonPanel ret = this.createBottomButtonPanel();
        if (this.actions != null) {
            for (final AbstractAction a : this.actions) {
                ret.addAction(a).addFocusListener(this.defaultButtonFocusListener);
            }
        }
        return ret;
    }

    /**
     * @return
     */
    protected Window getDesiredRootFrame() {
        return null;
    }

    public InternDialog<T> getDialog() {
        if (this.dialog == null) {
            throw new NullPointerException("Call #org.appwork.utils.swing.dialog.AbstractDialog.displayDialog() first");
        } else {
            return this.dialog;
        }
    }

    public DialogDimensor getDimensor() {
        return this.dimensor;
    }

    /**
     * Create the key to save the don't showmagain state in database. should be overwritten in same dialogs. by default, the dialogs get
     * differed by their title and their classname
     *
     * @return
     */
    public String getDontShowAgainKey() {
        return "ABSTRACTDIALOG_DONT_SHOW_AGAIN_" + this.getClass().getSimpleName() + "_" + this.toString();
    }

    /**
     * @return
     */
    protected String getDontShowAgainLabelText() {
        return _AWU.T.ABSTRACTDIALOG_STYLE_SHOW_DO_NOT_DISPLAY_AGAIN();
    }

    // /**
    // * should be overwritten and return a Dimension of the dialog should have
    // a
    // * special size
    // *
    // * @return
    // */
    // protected Dimension getDesiredSize() {
    //
    // return null;
    // }
    public int getFlags() {
        return this.flagMask;
    }

    public Icon getIcon() {
        return this.icon;
    }

    /**
     * @return
     */
    protected JComponent getIconComponent() {
        this.iconLabel = new JLabel(this.icon);
        // iconLabel.setVerticalAlignment(JLabel.TOP);
        return this.iconLabel;
    }

    /**
     * @return
     */
    protected String getIconConstraints() {
        return "gapright 10,gaptop 2";
    }

    public String getIconDataUrl() {
        if (this.getIcon() == null) {
            return null;
        }
        Base64OutputStream b64os = null;
        ByteArrayOutputStream bos = null;
        try {
            bos = new ByteArrayOutputStream();
            b64os = new Base64OutputStream(bos);
            ImageProvider.writeImage(IconIO.convertIconToBufferedImage(this.getIcon()), "png", b64os);
            b64os.flush(true);
            final String ret = "png;base64," + bos.toString("UTF-8");
            return ret;
        } catch (final Exception e) {
            e.printStackTrace();
            return null;
        } finally {
            try {
                b64os.close();
            } catch (final Throwable e) {
            }
            try {
                bos.close();
            } catch (final Throwable e) {
            }
        }
    }

    /**
     * @return
     */
    public List<? extends Image> getIconList() {
        return null;
    }

    public DialogLocator getLocator() {
        DialogLocator ret = this.locator;
        if (ret == null) {
            ret = AbstractDialog.DEFAULT_LOCATOR;
            if (ret != null) {
                return ret;
            } else {
                return AbstractDialog.LOCATE_CENTER_OF_SCREEN;
            }
        }
        return ret;
    }

    /**
     * @return
     */
    public ModalityType getModalityType() {
        return modalityType;
    }

    public String getOKButtonText() {
        return this.okButtonText;
    };

    /**
     * @return
     */
    public Window getOwner() {
        return AbstractDialog.getGlobalOwnerFinder().findDialogOwner(this, AbstractDialog.getWindowStackByRoot(this.getDesiredRootFrame()));
    }

    private int preferredHeight = -1;
    private int preferredWidth  = -1;

    /**
     * override this if you want to set a special height
     *
     * @return
     */
    protected int getPreferredHeight() {
        return preferredHeight;
    }

    public AbstractDialog<T> setPreferredHeight(int i) {
        preferredHeight = i;
        return this;
    }

    /**
     * @param i
     * @return
     */
    public AbstractDialog<T> setPreferredWidth(int i) {
        preferredWidth = i;
        return this;
    }

    /**
     * @return
     */
    public Dimension getPreferredSize() {
        final Dimension rawPreferredSize = this.getRawPreferredSize();
        int width = this.getPreferredWidth();
        int height = this.getPreferredHeight();
        if (width <= 0) {
            width = rawPreferredSize.width;
        }
        if (height <= 0) {
            height = rawPreferredSize.height;
        }
        if (getDialog().isPreferredSizeSet()) {
            // allow larger dialogs when preferred size is set!
            height = Math.max(rawPreferredSize.height, height);
            width = Math.max(rawPreferredSize.width, width);
        }
        try {
            // TODO:this ignores insets and multimonitor setups
            final Dimension ret = new Dimension(Math.min(Toolkit.getDefaultToolkit().getScreenSize().width, width), Math.min(Toolkit.getDefaultToolkit().getScreenSize().height, height));
            return ret;
        } catch (final Throwable e) {
            return rawPreferredSize;
        }
    }

    /**
     * overwride this to set a special width
     *
     * @return
     */
    protected int getPreferredWidth() {
        return preferredWidth;
    }

    /**
     * @return
     */
    public Dimension getRawPreferredSize() {
        return this.getDialog().getRawPreferredSize();
    }

    /**
     * Return the returnbitmask
     *
     * @return
     */
    public int getReturnmask() {
        if (!this.initialized) {
            throw new IllegalStateException("Dialog has not been initialized yet. call displayDialog()");
        } else {
            return this.returnBitMask;
        }
    }

    public T getReturnValue() {
        if (!this.initialized) {
            throw new IllegalStateException("Dialog has not been initialized yet. call displayDialog()");
        } else {
            return this.createReturnValue();
        }
    }

    public int getTimeout() {
        return this.timeout;
    }

    protected NullsafeAtomicReference<AbstractTimerThread> getTimer() {
        return this.timer;
    }

    /**
     * @return
     */
    public String getTitle() {
        try {
            if (this.dialog == null) {
                return this.title;
            } else {
                return this.getDialog().getTitle();
            }
        } catch (final NullPointerException e) {
            // not initialized yet
            return this.title;
        }
    }

    /**
     * @return
     */
    protected FrameState getWindowStateOnVisible() {
        return AbstractDialog.ZHANDLER.getWindowStateOnVisible(this);
    }

    /**
     *
     * @return if the dialog has been moved by the user
     */
    public boolean hasBeenMoved() {
        try {
            return this.orgLocationOnScreen != null && dialog != null && !this.getDialog().getLocationOnScreen().equals(this.orgLocationOnScreen);
        } catch (java.awt.IllegalComponentStateException e) {
            // java.awt.IllegalComponentStateException: component must be showing on the screen to determine its location
            return false;
        }
    }

    protected void initDoNotShowAgainCheckbox(final MigPanel bottom) {
        this.dontshowagain = new JCheckBox(this.getDontShowAgainLabelText());
        this.dontshowagain.setHorizontalAlignment(SwingConstants.TRAILING);
        this.dontshowagain.setHorizontalTextPosition(SwingConstants.LEADING);
        this.dontshowagain.setSelected(this.doNotShowAgainSelected);
        bottom.add(this.dontshowagain, "alignx right");
    }

    protected JComponent lastFocusRequestedJComponent = null;

    /**
     * @param focus
     */
    protected void initFocus(final JComponent focus) {
        final Component focusOwner = AbstractDialog.this.getDialog().getFocusOwner();
        if (focusOwner != null) {
            // dialog component has already focus...
            return;
        }
        final JComponent lastFocusRequestedJComponent = this.lastFocusRequestedJComponent;
        if (focus == null) {
            this.lastFocusRequestedJComponent = null;
            return;
        } else if (lastFocusRequestedJComponent == focus) {
            return;
        } else if (focus.requestFocusInWindow()) {
            this.lastFocusRequestedJComponent = focus;
        }
    }

    protected void initTimer() {
        final AbstractTimerThread thread;
        final AbstractTimerThread oldThread = this.getTimer().getAndSet(thread = new AbstractTimerThread(this));
        if (oldThread != null) {
            oldThread.interrupt();
        }
        thread.start();
    }

    /**
     * Closes the thread. Causes a cancel and setting the interrupted flag
     */
    public void interrupt() {
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                if (!AbstractDialog.this.isInitialized()) {
                    return;
                }
                if (AbstractDialog.this.isDisposed() && AbstractDialog.this.returnBitMask != (Dialog.RETURN_CLOSED | Dialog.RETURN_INTERRUPT) && AbstractDialog.this.isDeveloperMode()) {
                    throw new IllegalStateException("Dialog already disposed");
                }
                AbstractDialog.this.dispose();
                AbstractDialog.this.returnBitMask = Dialog.RETURN_CLOSED | Dialog.RETURN_INTERRUPT;
            }
        };
    }

    public boolean isCallerIsEDT() {
        return this.callerIsEDT;
    }

    /**
     * @return
     *
     */
    public boolean isCountdownFlagEnabled() {
        return BinaryLogic.containsAll(this.flagMask, UIOManager.LOGIC_COUNTDOWN);
    }

    public boolean isCountdownPausable() {
        return this.countdownPausable;
    }

    /**
     * @return
     */
    protected boolean isDeveloperMode() {
        // dev mode in IDE
        return !Application.isJared(AbstractDialog.class);
    }

    public boolean isDisposed() {
        return this.disposed;
    }

    /**
     * @return
     */
    public boolean isDontShowAgainFlagEabled() {
        return BinaryLogic.containsAll(this.flagMask, Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN);
    }

    public boolean isDontShowAgainSelected() {
        if (this.isHiddenByDontShowAgain() || this.dontshowagain != null && this.dontshowagain.isSelected() && this.dontshowagain.isEnabled()) {
            return true;
        } else {
            return false;
        }
    }

    public boolean isHiddenByDontShowAgain() {
        if (this.dontshowagain != null && this.dontshowagain.isSelected() && this.dontshowagain.isEnabled()) {
            return false;
        }
        final String key = this.getDontShowAgainKey();
        if (key == null) {
            return false;
        }
        final int i = BinaryLogic.containsAll(this.flagMask, UIOManager.LOGIC_DONT_SHOW_AGAIN_DELETE_ON_EXIT) ? AbstractDialog.getSessionDontShowAgainValue(this.getDontShowAgainKey()) : JSonStorage.getPlainStorage("Dialogs").get(this.getDontShowAgainKey(), -1);
        return i >= 0;
    }

    /**
     * @return the initialized
     */
    public boolean isInitialized() {
        return this.initialized;
    }

    /**
     * override to change default resizable flag
     *
     * @return
     */
    protected boolean isResizable() {
        // by default dialogs should be resizeble - at least for windows.
        // size calculation is almost impossible for nonresizable dialogs. they
        // are always a bit bigger than getDimension tells us
        return true;
    }

    /**
     * @return
     */
    protected boolean isVisible() {
        if (this.dialog == null) {
            return false;
        } else {
            return this.getDialog().isVisible();
        }
    }

    protected void layoutDialog() {
        Dialog.getInstance().initLaf();
        ModalityType modality = this.getModalityType();
        if (this.isCallerIsEDT()) {
            if (modality != ModalityType.APPLICATION_MODAL) {
                LogV3.warning("Modeless Dialog from EDT -> Not possible. Reset to Application_modal");
                modality = ModalityType.APPLICATION_MODAL;
            }
        }
        this.dialog = new InternDialog<T>(this, modality) {
            @Override
            public void dispose() {
                try {
                    AbstractDialog.this.setDisposed(true);
                    AbstractDialog.this.dispose();
                } finally {
                    super.realDispose();
                }
            }
        };
        if (getDimensor() != null || getLocator() != null) {
            dialog.addComponentListener(this);
        }
        if (this.preferredSize != null) {
            this.dialog.setPreferredSize(this.preferredSize);
        }
        this.timerLbl = new JLabel(TimeFormatter.formatMilliSeconds(this.getCountdown(), 0));
        this.timerLbl.setEnabled(this.isCountdownPausable());
    }

    /**
     * This method has to be overwritten to implement custom content
     *
     * @return musst return a JComponent
     */
    abstract public JComponent layoutDialogContent();

    public void onSetVisible(final boolean b) {
        if (!b && this.getDialog().isVisible()) {
            final DialogLocator locator = getLocator();
            if (locator != null) {
                try {
                    locator.onClose(AbstractDialog.this, null);
                } catch (final Exception e) {
                    getLogger().log(e);
                }
            }
            final DialogDimensor dimensor = getDimensor();
            if (dimensor != null) {
                try {
                    dimensor.onClose(AbstractDialog.this, null);
                } catch (final Exception e) {
                    getLogger().log(e);
                }
            }
        }
    }

    /**
     * Handle timeout
     */
    public void onTimeout() {
        this.setReturnmask(false);
        if (this.isDeveloperMode() && this.isDisposed()) {
            throw new IllegalStateException("Dialog is already Disposed");
        }
        this.returnBitMask |= Dialog.RETURN_TIMEOUT;
        this.dispose();
    }

    public void pack() {
        if (!this.getDialog().isMinimumSizeSet()) {
            final Dimension rawPreferredSize = getRawPreferredSize();
            final Dimension preferredSize = getDialog().getPreferredSize();
            if (rawPreferredSize != null && preferredSize != null && (rawPreferredSize.height < preferredSize.height && rawPreferredSize.width < preferredSize.width)) {
                this.getDialog().setMinimumSize(rawPreferredSize);
            }
        }
        this.getDialog().pack();
    }

    /**
     * may be overwritten to set focus to special components etc.
     */
    protected void packed() {
    }

    protected JComponent getFocusButton() {
        return focusButton;
    }

    protected void registerEscape() {
        final JComponent focusButton = getFocusButton();
        if (focusButton != null) {
            final KeyStroke ks = KeyStroke.getKeyStroke("ESCAPE");
            focusButton.getInputMap().put(ks, "ESCAPE");
            focusButton.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(ks, "ESCAPE");
            focusButton.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(ks, "ESCAPE");
            focusButton.getActionMap().put("ESCAPE", new AbstractAction() {
                private static final long serialVersionUID = -6666144330707394562L;

                public void actionPerformed(final ActionEvent e) {
                    if (AbstractDialog.this.isDisposed()) {
                        return;
                    }
                    getLogger().fine("Answer: Key<ESCAPE>");
                    AbstractDialog.this.setReturnmask(false);
                    AbstractDialog.this.returnBitMask |= Dialog.RETURN_ESC;
                    AbstractDialog.this.dispose();
                }
            });
        }
    }

    protected void registerEscape(final JComponent contentpane) {
        this.focusButton = contentpane;
        this.registerEscape();
    }

    /**
     * resets the dummyinit to continue working with the dialog instance after using {@link #forceDummyInit()}
     */
    public void resetDummyInit() {
        if (org.appwork.utils.Application.isHeadless()) {
            getLogger().info("Reset Dummy Info");
            AbstractDialog.this.initialized = false;
            AbstractDialog.this.dummyInit = false;
        } else {
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    getLogger().info("Reset Dummy Info");
                    AbstractDialog.this.initialized = false;
                    AbstractDialog.this.dummyInit = false;
                }
            }.waitForEDT();
        }
    }

    public void resetTimer() {
        final AbstractTimerThread lTimer = this.getTimer().get();
        if (lTimer != null) {
            lTimer.reset();
        }
    }

    protected void setAlwaysOnTop(final boolean b) {
        this.getDialog().setAlwaysOnTop(b);
    }

    /**
     * @param b
     */
    public void setCallerIsEDT(final boolean b) {
        this.callerIsEDT = b;
    }

    public void setCloseReason(final CloseReason closeReason) {
        switch (closeReason) {
        case OK:
            this.returnBitMask = Dialog.RETURN_OK;
            break;
        case CANCEL:
            this.returnBitMask = Dialog.RETURN_CANCEL;
            break;
        case CLOSE:
            this.returnBitMask = Dialog.RETURN_CLOSED;
            break;
        case TIMEOUT:
            this.returnBitMask = Dialog.RETURN_CLOSED | Dialog.RETURN_TIMEOUT;
            break;
        case INTERRUPT:
            this.returnBitMask = Dialog.RETURN_CLOSED | Dialog.RETURN_INTERRUPT;
            break;
        default:
            if (DebugMode.TRUE_IN_IDE_ELSE_FALSE) {
                throw new WTFException("Unhandled CloseReason:" + closeReason);
            }
            break;
        }
    }

    public void setCountdownPausable(final boolean b) {
        this.countdownPausable = b;
        final Thread ltimer = this.timer.get();
        if (ltimer != null && ltimer.isAlive()) {
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    AbstractDialog.this.timerLbl.setEnabled(b);
                }
            };
        }
    }

    /**
     * @deprecated use #setTimeout instead
     * @param countdownTime
     */
    @Deprecated
    public void setCountdownTime(final int countdownTimeInSeconds) {
        this.timeout = countdownTimeInSeconds * 1000;
    }

    protected void setDefaultCloseOperation(final int doNothingOnClose) {
        this.getDialog().setDefaultCloseOperation(doNothingOnClose);
    }

    public void setDimensor(final DialogDimensor dimensor) {
        this.dimensor = dimensor;
    }

    /**
     * @param b
     */
    protected void setDisposed(final boolean b) {
        try {
            if (this.disposeCallBack != null) {
                this.disposeCallBack.dialogDisposed(AbstractDialog.this);
            }
        } finally {
            this.disposed = b;
        }
    }

    /**
     * @param disposeCallBack
     */
    public void setDisposedCallback(final DisposeCallBack disposeCallBack) {
        this.disposeCallBack = disposeCallBack;
    }

    /**
     * @param b
     */
    public void setDoNotShowAgainSelected(final boolean b) {
        if (!BinaryLogic.containsAll(this.flagMask, Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN)) {
            throw new IllegalStateException("You have to set the Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN flag to use this method");
        } else {
            this.doNotShowAgainSelected = b;
        }
    }

    public void setIcon(final Icon icon) {
        this.icon = icon;
        if (this.iconLabel != null) {
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    AbstractDialog.this.iconLabel.setIcon(icon);
                }
            };
        }
    }

    /**
     *
     */
    public void setInterrupted() {
        if (this.isDeveloperMode() && this.isDisposed()) {
            throw new IllegalStateException("Dialog already disposed");
        } else {
            this.returnBitMask |= Dialog.RETURN_INTERRUPT;
        }
    }

    /**
     * Add Additional BUttons on the left side of ok and cancel button. You can add a "tag" property to the action in ordner to help the
     * layouter,
     *
     * <pre>
     * abstractActions[0].putValue(&quot;tag&quot;, &quot;ok&quot;)
     * </pre>
     *
     * @param abstractActions
     *            list
     */
    public void setLeftActions(final AbstractAction... abstractActions) {
        this.actions = abstractActions;
    }

    /**
     * @param locateCenterOfScreen
     */
    public void setLocator(final DialogLocator locator) {
        this.locator = locator;
    }

    protected void setMinimumSize(final Dimension dimension) {
        this.getDialog().setMinimumSize(dimension);
    }

    /**
     * @param dimension
     */
    public void setPreferredSize(final Dimension dimension) {
        this.preferredSize = dimension;
        if (this.dialog != null) {
            this.getDialog().setPreferredSize(dimension);
        }
    }

    protected void setResizable(final boolean b) {
        this.getDialog().setResizable(b);
    }

    /**
     * Sets the returnvalue and saves the don't show again states to the database
     *
     * @param b
     */
    protected void setReturnmask(final boolean b) {
        int ret = b ? Dialog.RETURN_OK : Dialog.RETURN_CANCEL;
        if (BinaryLogic.containsAll(this.flagMask, Dialog.STYLE_SHOW_DO_NOT_DISPLAY_AGAIN)) {
            boolean isDontShowAgainSelected = this.dontshowagain != null && this.dontshowagain.isSelected() && this.dontshowagain.isEnabled();
            ret = writeDontShowAgainAnswer(b, isDontShowAgainSelected);
        }
        if (ret == this.returnBitMask) {
            return;
        }
        if (this.isDeveloperMode() && this.isDisposed()) {
            throw new IllegalStateException("Dialog already disposed");
        }
        this.returnBitMask = ret;
    }

    public int writeDontShowAgainAnswer(final boolean clickedOK, boolean isDontShowAgainSelected) {
        int ret = clickedOK ? Dialog.RETURN_OK : Dialog.RETURN_CANCEL;
        if (isDontShowAgainSelected) {
            ret |= Dialog.RETURN_DONT_SHOW_AGAIN;
            if (BinaryLogic.containsAll(this.flagMask, UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_CANCEL) && !clickedOK) {
                return ret;
            }
            if (BinaryLogic.containsAll(this.flagMask, UIOManager.LOGIC_DONT_SHOW_AGAIN_IGNORES_OK) && clickedOK) {
                return ret;
            }
            try {
                final String key = this.getDontShowAgainKey();
                if (key != null) {
                    if (BinaryLogic.containsAll(this.flagMask, UIOManager.LOGIC_DONT_SHOW_AGAIN_DELETE_ON_EXIT)) {
                        synchronized (AbstractDialog.SESSION_DONTSHOW_AGAIN) {
                            AbstractDialog.SESSION_DONTSHOW_AGAIN.put(this.getDontShowAgainKey(), ret);
                        }
                    } else {
                        JSonStorage.getPlainStorage("Dialogs").put(this.getDontShowAgainKey(), ret);
                    }
                }
            } catch (final Exception e) {
                getLogger().log(e);
            }
        }
        return ret;
    }

    /**
     * Set countdown time on Milliseconds!
     *
     * @param countdownTimeInMs
     */
    public void setTimeout(final int countdownTimeInMs) {
        this.timeout = countdownTimeInMs;
    }

    /**
     * @param title2
     */
    public void setTitle(final String title) {
        this.title = title;
        if (this.dialog != null) {
            this.getDialog().setTitle(title);
        }
    }

    /**
     * @param b
     */
    public void setVisible(final boolean b) {
        // this.onSetVisible(b);
        new EDTRunner() {
            @Override
            protected void runInEDT() {
                // getDialog().setVisible(b);
                WindowManager.getInstance().setVisible(AbstractDialog.this.getDialog(), b, AbstractDialog.this.getWindowStateOnVisible());
            }
        };
    }

    public UserIODefinition show() {
        final UserIODefinition ret = UIOManager.I().show(UserIODefinition.class, this);
        return ret;
    }

    public void stopTimer() {
        final Thread ltimer = this.getTimer().getAndSet(null);
        if (ltimer != null) {
            ltimer.interrupt();
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    AbstractDialog.this.timerLbl.setEnabled(false);
                }
            };
        }
    }

    /**
     * @throws DialogClosedException
     * @throws DialogCanceledException
     *
     */
    public void throwCloseExceptions() throws DialogClosedException, DialogCanceledException {
        final int mask = this.getReturnmask();
        if (BinaryLogic.containsSome(mask, Dialog.RETURN_CLOSED)) {
            throw new DialogClosedException(mask);
        } else if (BinaryLogic.containsSome(mask, Dialog.RETURN_CANCEL)) {
            throw new DialogCanceledException(mask);
        }
    }

    /**
     * Returns an id of the dialog based on it's title;
     */
    @Override
    public String toString() {
        return ("dialog-" + this.getTitle()).replaceAll("\\W", "_");
    }

    public void windowActivated(final WindowEvent arg0) {
    }

    public void windowClosed(final WindowEvent arg0) {
    }

    public void windowClosing(final WindowEvent arg0) {
        if (this.closeAllowed()) {
            getLogger().fine("Answer: Button<[X]>");
            if (this.isDeveloperMode() && this.isDisposed()) {
                throw new IllegalStateException("Dialog already disposed");
            }
            this.returnBitMask |= Dialog.RETURN_CLOSED;
            this.dispose();
        } else {
            getLogger().fine("(Answer: Tried [X] bot not allowed)");
        }
    }

    public void windowDeactivated(final WindowEvent arg0) {
    }

    public void windowDeiconified(final WindowEvent arg0) {
    }

    @Override
    public void windowGainedFocus(final WindowEvent e) {
        this.initFocus(this.getFocusButton());
    }

    public void windowIconified(final WindowEvent arg0) {
    }

    @Override
    public void windowLostFocus(final WindowEvent e) {
    }

    public void windowOpened(final WindowEvent arg0) {
    }

    /**
     * @param currentTimeout
     * @return
     */
    public String formatCountdown(long currentTimeout) {
        return TimeFormatter.formatMilliSeconds(currentTimeout, 0);
    }

    /**
     * @param currentTimeout
     * @return
     */
    public boolean isExpired(long currentTimeout) {
        return currentTimeout < 0;
    }
}
