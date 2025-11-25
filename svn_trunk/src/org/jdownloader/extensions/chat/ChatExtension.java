//    jDownloader - Downloadmanager
//    Copyright (C) 2008  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.
package org.jdownloader.extensions.chat;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.WeakHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.regex.Pattern;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.TypeRef;
import org.appwork.utils.Application;
import org.appwork.utils.IO;
import org.appwork.utils.Regex;
import org.appwork.utils.StringUtils;
import org.appwork.utils.Time;
import org.appwork.utils.formatter.TimeFormatter;
import org.appwork.utils.os.ContainerRuntime;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.os.Flatpak;
import org.appwork.utils.os.Snap;
import org.appwork.utils.os.hardware.HardwareType;
import org.appwork.utils.swing.EDTHelper;
import org.appwork.utils.swing.EDTRunner;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.controlling.contextmenu.ContextMenuManager;
import org.jdownloader.controlling.contextmenu.MenuContainerRoot;
import org.jdownloader.controlling.contextmenu.MenuExtenderHandler;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.extensions.AbstractExtension;
import org.jdownloader.extensions.ExtensionConfigPanel;
import org.jdownloader.extensions.StartException;
import org.jdownloader.extensions.StopException;
import org.jdownloader.extensions.chat.settings.ChatConfigPanel;
import org.jdownloader.extensions.chat.translate.ChatTranslation;
import org.jdownloader.gui.mainmenu.MenuManagerMainmenu;
import org.jdownloader.gui.toolbar.MenuManagerMainToolbar;
import org.jdownloader.logging.LogController;
import org.schwering.irc.lib.IRCConnection;

import jd.SecondLevelLaunch;
import jd.controlling.reconnect.Reconnecter;
import jd.controlling.reconnect.ReconnecterEvent;
import jd.controlling.reconnect.ReconnecterListener;
import jd.gui.UserIO;
import jd.gui.swing.jdgui.JDGui;
import jd.gui.swing.jdgui.interfaces.SwitchPanel;
import jd.plugins.AddonPanel;
import jd.utils.JDUtilities;
import net.miginfocom.swing.MigLayout;

public class ChatExtension extends AbstractExtension<ChatConfig, ChatTranslation> implements ReconnecterListener, MenuExtenderHandler {
    private static final long                   AWAY_TIMEOUT   = 15 * 60 * 1000;
    private static final Pattern                CMD_ACTION     = Pattern.compile("(me)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_CONNECT    = Pattern.compile("(connect|verbinden)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_CLOSE      = Pattern.compile("(close)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_DISCONNECT = Pattern.compile("(disconnect|trennen)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_EXIT       = Pattern.compile("(exit|quit)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_HELP       = Pattern.compile("(help|info)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_MODE       = Pattern.compile("(mode|modus)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_JOIN       = Pattern.compile("join", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_NICK       = Pattern.compile("(nick|name)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_PM         = Pattern.compile("(msg|query)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_SLAP       = Pattern.compile("(slap)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_SAVE       = Pattern.compile("(save)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_TOPIC      = Pattern.compile("(topic|title)", Pattern.CASE_INSENSITIVE);
    private static final Pattern                CMD_VERSION    = Pattern.compile("(version|jdversion)", Pattern.CASE_INSENSITIVE);
    private static final java.util.List<String> COMMANDS       = new ArrayList<String>();
    public static String                        STYLE;
    static {
        try {
            STYLE = IO.readURLToString(ChatExtension.class.getResource("styles.css"));
        } catch (IOException e) {
            STYLE = "";
            e.printStackTrace();
        }
    }
    public static final String STYLE_ACTION         = "action";
    public static final String STYLE_ERROR          = "error";
    public static final String STYLE_HELP           = "help";
    public static final String STYLE_HIGHLIGHT      = "highlight";
    public static final String STYLE_NOTICE         = "notice";
    public static final String STYLE_PM             = "pm";
    public static final String STYLE_SELF           = "self";
    public static final String STYLE_SYSTEM_MESSAGE = "system";
    public static final String STYLE_OP_NOTICE      = "opnotice";
    public static final String STYLE_TOPIC          = "topic";
    public static final String SERVER_MESSAGE_TAB   = "server-messages";
    public static String       USERLIST_STYLE;
    static {
        try {
            USERLIST_STYLE = IO.readURLToString(ChatExtension.class.getResource("userliststyles.css"));
        } catch (IOException e) {
            USERLIST_STYLE = "";
            e.printStackTrace();
        }
    }
    private JTextField                       top;
    private IRCConnection                    conn;
    private long                             lastAction;
    private String                           lastCommand;
    private boolean                          loggedIn;
    private volatile List<User>              NAMES;
    private boolean                          nickaway;
    private int                              nickCount = 0;
    private String                           orgNick;
    private JTextPane                        right;
    private final TreeMap<String, JDChatPMS> pms       = new TreeMap<String, JDChatPMS>();
    private StringBuilder                    sb;
    private JScrollPane                      scrollPane;
    private JTextPane                        textArea;
    private JTextField                       textField;
    private JDChatView                       view;
    private ChatConfigPanel                  configPanel;
    private String                           currentChannel;
    private Thread                           awayChecker;
    private String                           banText   = null;
    JTabbedPane                              tabbedPane;

    public ExtensionConfigPanel<ChatExtension> getConfigPanel() {
        return configPanel;
    }

    public boolean hasConfigPanel() {
        return true;
    }

    @Override
    public String getIconKey() {
        return "chat";
    }

    public ChatExtension() throws StartException {
        super();
        setTitle(T.jd_plugins_optional_jdchat_jdchat());
    }

    public void switchToPMS(final String user) {
        if (ChatExtension.this.tabbedPane.getTitleAt(ChatExtension.this.tabbedPane.getSelectedIndex()).toLowerCase().equals(user.toLowerCase())) {
            return;
        }
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                for (int x = 0; x < ChatExtension.this.tabbedPane.getTabCount(); x++) {
                    if (ChatExtension.this.tabbedPane.getTitleAt(x).toLowerCase().equals(user.toLowerCase())) {
                        ChatExtension.this.tabbedPane.setSelectedIndex(x);
                        break;
                    }
                }
                return null;
            }
        }.start(true);
    }

    public void addPMS(final String user2) {
        final String user = user2.trim();
        if (user.equals(this.conn.getNick().trim())) {
            return;
        }
        this.pms.put(user.toLowerCase(), new JDChatPMS(user));
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                ChatExtension.this.tabbedPane.add(user, ChatExtension.this.pms.get(user.toLowerCase()).getScrollPane());
                return null;
            }
        }.start(true);
    }

    public void addToText(final User user, final String style, final String msg) {
        this.addToText(user, style, msg, this.textArea, this.sb);
    }

    public void addToText(final User user, String style, final String msg, final JTextPane targetpane, final StringBuilder sb) {
        final IRCConnection con = ChatExtension.this.conn;
        final boolean color = !getSettings().isUserColorEnabled();
        final Date dt = new Date();
        final SimpleDateFormat df = new SimpleDateFormat("HH:mm:ss");
        sb.append("<!---->");
        sb.append("<li>");
        if (user != null) {
            if (!color) {
                sb.append("<span style='" + user.getStyle() + (this.getUser(con.getNick()) == user ? ";font-weight:bold" : "") + "'>");
                sb.append("[" + df.format(dt) + "] ");
                sb.append(user.getRank() + user.name);
                sb.append(ChatExtension.STYLE_PM.equalsIgnoreCase(style) ? ">> " : ": ");
                sb.append("</span>");
            } else {
                sb.append("<span style='color:#000000" + (this.getUser(con.getNick()) == user ? ";font-weight:bold" : "") + "'>");
                sb.append("[" + df.format(dt)).append("] ");
                sb.append(user.getRank() + user.name);
                sb.append(ChatExtension.STYLE_PM.equalsIgnoreCase(style) ? ">> " : ": ");
                sb.append("</span>");
            }
        } else {
            sb.append("<span class='time'>[").append(df.format(dt)).append("] </span>");
        }
        if (this.conn != null && msg.contains(con.getNick()) && style == null) {
            // Only highlight if no other style set (prevents highlighting system messages)
            style = ChatExtension.STYLE_HIGHLIGHT;
        }
        if (style != null) {
            sb.append("<span class='").append(style).append("'>").append(msg).append("</span>");
        } else {
            sb.append("<span>").append(msg).append("</span>");
        }
        final boolean isHighlight = isHighlightAllowed() && StringUtils.equals(style, ChatExtension.STYLE_HIGHLIGHT);
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                if (isHighlight && !JDGui.getInstance().getMainFrame().isActive()) {
                    JDGui.getInstance().getMainFrame().toFront();
                }
                targetpane.setText(ChatExtension.STYLE + "<ul>" + sb.toString() + "</ul>");
                final int max = ChatExtension.this.scrollPane.getVerticalScrollBar().getMaximum();
                ChatExtension.this.scrollPane.getVerticalScrollBar().setValue(max);
                return null;
            }
        }.start();
    }

    public void addUser(final String name) {
        User user;
        if ((user = this.getUser(name)) == null) {
            this.NAMES.add(new User(name));
        } else if (user.rank != new User(name).rank) {
            user.rank = new User(name).rank;
        }
        this.updateNamesPanel();
    }

    public void addUsers(final String[] split) {
        User user;
        for (final String name : split) {
            if ((user = this.getUser(name)) == null) {
                this.NAMES.add(new User(name));
            } else if (user.rank != new User(name).rank) {
                user.rank = new User(name).rank;
            }
        }
        this.updateNamesPanel();
    }

    public void delPMS(final String user) {
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                ChatExtension.this.pms.remove(user.toLowerCase());
                for (int x = 0; x < ChatExtension.this.tabbedPane.getComponentCount(); x++) {
                    if (ChatExtension.this.tabbedPane.getTitleAt(x).toLowerCase().equals(user.toLowerCase())) {
                        ChatExtension.this.tabbedPane.remove(x);
                        return null;
                    }
                }
                return null;
            }
        }.start(true);
    }

    protected void doAction(final String type, final String name) {
        if (type.equals("reconnect") && name.equals("reconnect")) {
            if (this.conn == null) {
                this.initIRC();
            }
            return;
        }
        final User usr = this.getUser(name);
        if (usr == null) {
            return;
        }
        if (this.textField.getText().length() == 0) {
            if (!this.pms.containsKey(usr.name.toLowerCase())) {
                this.addPMS(usr.name);
            }
            this.switchToPMS(usr.name.toLowerCase());
        } else {
            new EDTHelper<Object>() {
                @Override
                public Object edtRun() {
                    ChatExtension.this.textField.setText(ChatExtension.this.textField.getText().trim() + " " + usr.name + " ");
                    return null;
                }
            }.start(true);
        }
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                ChatExtension.this.textField.requestFocus();
                return null;
            }
        }.start(true);
    }

    public String getNick() {
        return this.conn.getNick();
    }

    public int getNickCount() {
        return this.nickCount;
    }

    public String getNickname() {
        final String loc = System.getProperty("user.country");
        final String def = "JD-[" + loc + "]_" + ("" + System.currentTimeMillis()).substring(6);
        String nick = getSettings().getNick();
        if (nick == null || nick.equalsIgnoreCase("")) {
            nick = UserIO.getInstance().requestInputDialog(T.plugins_optional_jdchat_enternick());
            if (nick != null && !nick.equalsIgnoreCase("")) {
                nick += "[" + loc + "]";
            }
            if (nick != null) {
                nick = nick.trim();
            }
            getSettings().setNick(nick);
        }
        if (nick == null) {
            nick = def;
        }
        nick = nick.trim();
        if (this.getNickCount() > 0) {
            nick += "[" + this.getNickCount() + "]";
        }
        return nick;
    }

    public boolean isGuiOptional() {
        return false;
    }

    public TreeMap<String, JDChatPMS> getPms() {
        return this.pms;
    }

    public User getUser(final String name) {
        for (final User next : this.NAMES) {
            if (next.isUser(name)) {
                return next;
            }
        }
        return null;
    }

    private void switchChannel(String newChannel) {
        if (newChannel.equalsIgnoreCase(currentChannel) && this.isLoggedIn()) {
            if (this.conn != null && this.conn.isConnected()) {
                this.addToText(null, ChatExtension.STYLE_NOTICE, "You are in channel: " + newChannel);
            }
            return;
        }
        this.NAMES.clear();
        if (this.conn != null && this.conn.isConnected()) {
            this.addToText(null, ChatExtension.STYLE_HELP, "You are now in channel: " + newChannel);
            this.addToText(null, ChatExtension.STYLE_HELP, "Your username is: " + this.getNickname());
        }
        // only leave if actually in a channel - fixes "No such channel" on connect
        if (this.conn != null && this.conn.isConnected() && currentChannel != null) {
            this.conn.doPart(getCurrentChannel(), " --> " + newChannel);
        }
        setCurrentChannel(newChannel);
        if (this.conn != null && this.conn.isConnected()) {
            this.conn.doJoin(getCurrentChannel(), null);
            new EDTRunner() {
                @Override
                protected void runInEDT() {
                    tabbedPane.setTitleAt(0, T.gui_tab_title(getCurrentChannel()));
                }
            };
            getSettings().setChannelLanguage(newChannel);
        }
        if (newChannel.equalsIgnoreCase("#jdownloader")) {
            // if we join the default channel, we print out some info, saves the bot from doing it every time
            // this will always print when the user joins a channel named #jdownloader - even when a different
            // irc server has been configured... but that's a rare edge case...
            this.addToText(null, ChatExtension.STYLE_HIGHLIGHT, "==> This is the JDownloader support chat!  ");
            this.addToText(null, ChatExtension.STYLE_HIGHLIGHT, "==> Please ask your question in ENGLISH or GERMAN! Be patient, give us time to respond!  ");
            this.addToText(null, ChatExtension.STYLE_HIGHLIGHT, "==> You can also search the forums at <a href=\"https://board.jdownloader.org\">https://board.jdownloader.org</a>  ");
            this.addToText(null, ChatExtension.STYLE_HIGHLIGHT, "==> Before reporting a problem, please check that you have the latest version installed!  ");
            this.addToText(null, ChatExtension.STYLE_HIGHLIGHT, "==> For a list of available commands, type \"/help\"  ");
            this.addToText(null, ChatExtension.STYLE_HIGHLIGHT, "==> This is a <u>public</u> chat, do not post usernames or passwords here!  ");
        }
    }

    public String getCurrentChannel() {
        return currentChannel;
    }

    private void setCurrentChannel(String newChannel) {
        this.currentChannel = newChannel;
    }

    @SuppressWarnings("unchecked")
    private void initGUI() {
        final int userlistposition = getSettings().getUserListPosition();
        this.textArea = new JTextPane();
        final HyperlinkListener hyp = new HyperlinkListener() {
            public void hyperlinkUpdate(final HyperlinkEvent e) {
                if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
                    if (e.getDescription().startsWith("intern")) {
                        final String[][] m = new Regex(e.getDescription() + "?", "intern:([\\w]*?)\\|(.*?)\\?").getMatches();
                        if (m.length == 1) {
                            ChatExtension.this.doAction(m[0][0], m[0][1]);
                            return;
                        }
                    } else {
                        CrossSystem.openURL(e.getURL());
                    }
                }
            }
        };
        this.right = new JTextPane();
        final Font font = right.getFont();
        this.right.setContentType("text/html");
        right.setFont(font);
        this.right.setEditable(false);
        this.textArea.addHyperlinkListener(hyp);
        this.right.addHyperlinkListener(hyp);
        this.scrollPane = new JScrollPane(this.textArea);
        scrollPane.setBorder(null);
        this.tabbedPane = new JTabbedPane(SwingConstants.TOP, JTabbedPane.SCROLL_TAB_LAYOUT);
        this.tabbedPane.add("JDChat", this.scrollPane);
        this.tabbedPane.addChangeListener(new ChangeListener() {
            public void stateChanged(final ChangeEvent e) {
                ChatExtension.this.tabbedPane.setForegroundAt(ChatExtension.this.tabbedPane.getSelectedIndex(), Color.black);
            }
        });
        this.textField = new JTextField() {
            @Override
            public void requestFocus() {
                System.out.println("REQUEST");
                super.requestFocus();
            }
        };
        // this.textField.setFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS, Collections.EMPTY_SET);
        // this.textField.setFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS, Collections.EMPTY_SET);
        this.textField.addFocusListener(new FocusListener() {
            public void focusGained(final FocusEvent e) {
                ChatExtension.this.tabbedPane.setForegroundAt(ChatExtension.this.tabbedPane.getSelectedIndex(), Color.black);
            }

            public void focusLost(final FocusEvent e) {
                ChatExtension.this.tabbedPane.setForegroundAt(ChatExtension.this.tabbedPane.getSelectedIndex(), Color.black);
            }
        });
        this.textField.addKeyListener(new KeyListener() {
            private int    counter = 0;
            private String last    = null;

            public void keyPressed(final KeyEvent e) {
                final int sel = ChatExtension.this.tabbedPane.getSelectedIndex();
                ChatExtension.this.tabbedPane.setForegroundAt(sel, Color.black);
                // it's important to have this code here instead of the released callback.
                // this solves this bug: http://svn.jdownloader.org/issues/58941
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    if (ChatExtension.this.textField.getText().length() == 0) {
                        return;
                    }
                    if (ChatExtension.this.tabbedPane.getSelectedIndex() == 0 || ChatExtension.this.textField.getText().startsWith("/")) {
                        ChatExtension.this.sendMessage(getCurrentChannel(), ChatExtension.this.textField.getText());
                    } else {
                        ChatExtension.this.sendMessage(getCurrentChannel(), "/msg " + ChatExtension.this.tabbedPane.getTitleAt(ChatExtension.this.tabbedPane.getSelectedIndex()) + " " + ChatExtension.this.textField.getText());
                    }
                }
            }

            public void keyReleased(final KeyEvent e) {
                final int sel = ChatExtension.this.tabbedPane.getSelectedIndex();
                ChatExtension.this.tabbedPane.setForegroundAt(sel, Color.black);
                // it's important to have this code here instead of the released callback.
                // this solves this bug: http://svn.jdownloader.org/issues/58941
                if (e.getKeyCode() == KeyEvent.VK_TAB) {
                    if (ChatExtension.this.textField.getText().length() == 0) {
                        if (ChatExtension.this.lastCommand != null) {
                            ChatExtension.this.textField.setText(ChatExtension.this.lastCommand);
                            ChatExtension.this.textField.requestFocus();
                        }
                        return;
                    }
                    String txt = ChatExtension.this.textField.getText();
                    if (this.last != null && txt.toLowerCase().startsWith(this.last.toLowerCase())) {
                        txt = this.last;
                    }
                    final String org = txt;
                    final int last = Math.max(0, txt.lastIndexOf(" "));
                    txt = txt.substring(last).trim();
                    final java.util.List<String> users = new ArrayList<String>();
                    final java.util.List<String> strings = new ArrayList<String>();
                    strings.addAll(ChatExtension.COMMANDS);
                    for (final User user : ChatExtension.this.NAMES) {
                        strings.add(user.name);
                    }
                    for (final String user : strings) {
                        if (user.length() >= txt.length() && user.toLowerCase().startsWith(txt.toLowerCase())) {
                            users.add(user);
                        }
                    }
                    if (users.size() == 0) {
                        return;
                    }
                    this.counter++;
                    if (this.counter > users.size() - 1) {
                        this.counter = 0;
                    }
                    final String user = users.get(this.counter);
                    this.last = org;
                    ChatExtension.this.textField.setText((ChatExtension.this.textField.getText().substring(0, last) + " " + user).trim());
                    ChatExtension.this.textField.requestFocus();
                } else if (e.getKeyCode() == KeyEvent.VK_UP) {
                    if (ChatExtension.this.textField.getText().length() == 0) {
                        if (ChatExtension.this.lastCommand != null) {
                            ChatExtension.this.textField.setText(ChatExtension.this.lastCommand);
                            ChatExtension.this.textField.requestFocus();
                        }
                        return;
                    }
                } else {
                    this.last = null;
                }
            }

            public void keyTyped(final KeyEvent e) {
            }
        });
        this.textArea.setContentType("text/html");
        textArea.setFont(font);
        this.textArea.setEditable(false);
        SwitchPanel frame = new SwitchPanel() {
            private static final long serialVersionUID = 2138710083573682339L;

            @Override
            public void onHide() {
            }

            @Override
            public void onShow() {
            }
        };
        frame.setLayout(new MigLayout("ins 0, wrap 1", "[grow,fill]", "[grow,fill][]"));
        JButton closeTab = new JButton(T.jd_plugins_optional_jdchat_closeTab());
        closeTab.addActionListener(new ActionListener() {
            public void actionPerformed(final ActionEvent e) {
                if (ChatExtension.this.tabbedPane.getSelectedIndex() > 0) {
                    ChatExtension.this.delPMS(ChatExtension.this.tabbedPane.getTitleAt(ChatExtension.this.tabbedPane.getSelectedIndex()));
                } else if (ChatExtension.this.tabbedPane.getSelectedIndex() == 0) {
                    ChatExtension.this.addToText(null, ChatExtension.STYLE_SYSTEM_MESSAGE, "You can't close the main Chat!");
                }
            }
        });
        final JScrollPane scrollPane_userlist = new JScrollPane(this.right);
        switch (userlistposition) {
        case 0:
            frame.add(this.tabbedPane, "split 2");
            frame.add(scrollPane_userlist, "width 180:180:180");
            break;
        default:
        case 1:
            frame.add(scrollPane_userlist, "width 180:180:180 ,split 2");
            frame.add(this.tabbedPane);
            break;
        }
        frame.add(this.textField, "growx, split 2");
        frame.add(closeTab, "w pref!");
        this.lastAction = System.currentTimeMillis();
        final MouseMotionListener ml = new MouseMotionListener() {
            public void mouseDragged(final MouseEvent e) {
            }

            public void mouseMoved(final MouseEvent e) {
                ChatExtension.this.lastAction = System.currentTimeMillis();
                ChatExtension.this.setNickAway(false);
            }
        };
        frame.addMouseMotionListener(ml);
        this.textArea.addMouseMotionListener(ml);
        this.textField.addMouseMotionListener(ml);
        this.right.addMouseMotionListener(ml);
        frame.setSize(new Dimension(800, 600));
        frame.setVisible(true);
        this.view = new JDChatView(this) {
            private static final long serialVersionUID = 3966113588850405974L;

            @Override
            protected void initMenuPanel(final JPanel menubar) {
                ChatExtension.this.top = new JTextField(T.jd_plugins_optional_jdchat_JDChat_topic_default());
                top.setEditable(false);
                menubar.add(top);
                ChatExtension.this.top.setToolTipText(T.jd_plugins_optional_jdchat_JDChat_topic_tooltip());
            }
        };
        this.view.setContent(frame);
    }

    public boolean isQuickToggleEnabled() {
        return true;
    }

    void initIRC() {
        this.NAMES.clear();
        for (int i = 0; i < 20; i++) {
            final String host = getSettings().getIrcServer2021();
            final int port = getSettings().getIrcPort();
            final String pass = null;
            final String nick = this.getNickname();
            final String name = "jdchat";
            // Making user name based on startup time, user would have to restart to get around channel ban based on name
            String user = "jd" + String.valueOf(SecondLevelLaunch.startup).substring(3, 10);
            this.addToText(null, ChatExtension.STYLE_SYSTEM_MESSAGE, "Connecting to JDChat...");
            this.conn = new IRCConnection(host, new int[] { port }, pass, nick, user, name);
            this.conn.setTimeout(1000 * 60 * 60);
            this.conn.addIRCEventListener(new IRCListener(this));
            this.conn.setEncoding("UTF-8");
            this.conn.setPong(true);
            this.conn.setDaemon(false);
            this.conn.setColors(false);
            try {
                this.conn.connect();
                this.startAwayObserver();
                break;
            } catch (final IOException e) {
                this.addToText(null, ChatExtension.STYLE_SYSTEM_MESSAGE, "Connect Timeout. Server not reachable...");
                LogController.CL().log(e);
                try {
                    Thread.sleep(15000);
                } catch (final InterruptedException e1) {
                    LogController.CL().log(e1);
                }
                this.NAMES.clear();
            }
        }
    }

    public boolean isLoggedIn() {
        return this.loggedIn;
    }

    public void notifyPMS(final String user, final String text2) {
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                for (int x = 0; x < ChatExtension.this.tabbedPane.getTabCount(); x++) {
                    if (ChatExtension.this.tabbedPane.getTitleAt(x).equals(user)) {
                        final int t = x;
                        ChatExtension.this.tabbedPane.setForegroundAt(t, Color.RED);
                        return null;
                    }
                }
                return null;
            }
        }.start(true);
    }

    public void onConnected() {
        this.switchChannel(getSettings().getChannel());
        this.setLoggedIn(true);
        this.perform();
    }

    public void onMode(final char op, final char mod, final String arg) {
        switch (mod) {
        case 'o':
            if (op == '+') {
                this.getUser(arg).rank = User.RANK_OP;
                this.updateNamesPanel();
            } else {
                this.getUser(arg).rank = User.RANK_DEFAULT;
                this.updateNamesPanel();
            }
            break;
        case 'v':
            if (op == '+') {
                this.getUser(arg).rank = User.RANK_VOICE;
                this.updateNamesPanel();
            } else {
                this.getUser(arg).rank = User.RANK_DEFAULT;
                this.updateNamesPanel();
            }
            break;
        case 'h':
            if (op == '+') {
                this.getUser(arg).rank = User.RANK_HOP;
                this.updateNamesPanel();
            } else {
                this.getUser(arg).rank = User.RANK_DEFAULT;
                this.updateNamesPanel();
            }
            break;
        }
    }

    /**
     * Cleans up and prevents spam from been sent to IRC channels.
     *
     * @param input
     * @return
     */
    public final static String performCleanup(final String input) {
        final String[] perform = org.appwork.utils.Regex.getLines(input);
        if (perform == null) {
            return null;
        }
        final StringBuilder sb = new StringBuilder();
        for (final String cmd : perform) {
            final String cleaned = cmd.trim();
            // only allow IRC commands. Do not allow message/actions to be sent to channels. Effectively users only need to change
            // nickname and authenticate to channel services. This is done via /msg chanserv and /msg nickserv on irc.libera.chat.
            if (cleaned.length() > 0 && StringUtils.startsWithCaseInsensitive(cleaned, "/") && !cleaned.matches("^(?i)/[a-z]+ #.+$")) {
                sb.append(cleaned.concat("\r\n"));
            }
        }
        return sb.toString();
    }

    public void perform() {
        final String performAction = performCleanup(getSettings().getPerformOnLoginCommands());
        if (performAction == null) {
            return;
        }
        final String[] perform = org.appwork.utils.Regex.getLines(performAction);
        for (final String cmd : perform) {
            final String cleaned = cmd.trim();
            if (cleaned.length() > 0) {
                this.sendMessage(getCurrentChannel(), cleaned);
            }
        }
    }

    /**
     * Does modifications to the text before sending it
     */
    private String prepareToSend(final String trim) {
        return trim;
    }

    public void reconnect() {
        this.initIRC();
    }

    public boolean removeUser(final String name) {
        final User user = this.getUser(name);
        if (user != null) {
            this.NAMES.remove(user);
            this.updateNamesPanel();
            return true;
        } else {
            return false;
        }
    }

    public void renamePMS(final String userOld, final String userNew) {
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                ChatExtension.this.pms.put(userNew.trim().toLowerCase(), ChatExtension.this.pms.get(userOld.trim().toLowerCase()));
                for (int x = 0; x < ChatExtension.this.tabbedPane.getComponentCount(); x++) {
                    if (ChatExtension.this.tabbedPane.getTitleAt(x).equalsIgnoreCase(userOld)) {
                        ChatExtension.this.tabbedPane.remove(x);
                        break;
                    }
                }
                ChatExtension.this.pms.remove(userOld);
                ChatExtension.this.tabbedPane.add(userNew.trim(), ChatExtension.this.pms.get(userNew.trim().toLowerCase()).getScrollPane());
                return null;
            }
        }.start(true);
    }

    public void renameUser(final String name, final String name2) {
        if (removeUser(name)) {
            this.addUser(name2);
            this.updateNamesPanel();
        }
    }

    public void requestNameList() {
        this.resetNamesList();
        this.conn.doNames(getCurrentChannel());
    }

    public void resetNamesList() {
        this.NAMES = new CopyOnWriteArrayList<User>();
        if (this.getUser(this.conn.getNick().trim()) == null) {
            this.NAMES.add(new User(this.conn.getNick().trim()));
        }
    }

    protected void sendMessage(final String channel2, final String text) {
        if (StringUtils.isNotEmpty(banText)) {
            Dialog.getInstance().showMessageDialog(banText);
            return;
        }
        this.lastAction = System.currentTimeMillis();
        this.setNickAway(false);
        if (text.startsWith("/")) {
            int end = text.indexOf(" ");
            if (end < 0) {
                end = text.length();
            }
            final String cmd = text.substring(1, end).trim();
            final String rest = text.substring(end).trim();
            if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_PM)) {
                new EDTHelper<Object>() {
                    @Override
                    public Object edtRun() {
                        ChatExtension.this.textField.setText("");
                        return null;
                    }
                }.start(true);
                if (ChatExtension.this.tabbedPane.getTitleAt(ChatExtension.this.tabbedPane.getSelectedIndex()).equals(ChatExtension.SERVER_MESSAGE_TAB)) {
                    this.addToText(null, ChatExtension.STYLE_ERROR, "You can't send messages to this tab", pms.get(ChatExtension.SERVER_MESSAGE_TAB.toLowerCase()).getTextArea(), pms.get(ChatExtension.SERVER_MESSAGE_TAB.toLowerCase()).getSb());
                    return;
                }
                end = rest.indexOf(" ");
                if (end < 0) {
                    end = rest.length();
                }
                final String targetUser = rest.substring(0, end).trim();
                if (!this.pms.containsKey(targetUser.toLowerCase())) {
                    this.addPMS(targetUser);
                }
                this.conn.doPrivmsg(targetUser, this.prepareToSend(rest.substring(end).trim()));
                this.lastCommand = "/msg " + targetUser.trim() + " ";
                this.addToText(this.getUser(this.conn.getNick()), ChatExtension.STYLE_SELF, Utils.prepareMsg(rest.substring(end).trim()), this.pms.get(targetUser.toLowerCase()).getTextArea(), this.pms.get(targetUser.toLowerCase()).getSb());
                this.switchToPMS(targetUser);
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_HELP)) {
                String[] infoLines = { "Available commands in this chat:", "/me <action>  -->  lets people know you do <action>", "/nick <name>  -->  changes your nick to <name>", "/msg <user> <message> -->  opens a private message with <user>", "/close  -->  closes the current chat tab", "/quit  -->  closes the support chat", "/topic  -->  lets you read the channel topic again", "/version  -->  shows info about your JDownloader program", "/connect  -->  connect/reconnects to the chat server if the connection got lost", "/disconnect  -->  disconnect from the chat server" };
                final String chatName = ChatExtension.this.tabbedPane.getTitleAt(ChatExtension.this.tabbedPane.getSelectedIndex());
                if (this.pms.containsKey(chatName.toLowerCase())) {
                    for (String infoLine : infoLines) {
                        this.addToText(null, ChatExtension.STYLE_HELP, Utils.prepareMsg(infoLine), pms.get(chatName.toLowerCase()).getTextArea(), pms.get(chatName.toLowerCase()).getSb());
                    }
                } else {
                    for (String infoLine : infoLines) {
                        this.addToText(null, ChatExtension.STYLE_HELP, Utils.prepareMsg(infoLine));
                    }
                }
                this.lastCommand = "/help";
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_SAVE)) {
                final String chatName = ChatExtension.this.tabbedPane.getTitleAt(ChatExtension.this.tabbedPane.getSelectedIndex());
                final String chatTime = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME).toString();
                final String logFilename = "ChatLog " + chatName.replace("#", "") + " " + chatTime + ".txt";
                final String userInfo = "This chat has been saved to the file \"" + logFilename + "\" in your JDownloader folder (" + Application.getHome() + ")";
                String chatBox;
                if (this.pms.containsKey(chatName.toLowerCase())) {
                    // save a pm
                    chatBox = this.pms.get(chatName.toLowerCase()).getTextArea().getText();
                    this.addToText(null, ChatExtension.STYLE_HELP, userInfo, pms.get(chatName.toLowerCase()).getTextArea(), pms.get(chatName.toLowerCase()).getSb());
                } else {
                    // not a pm, it's the main chat
                    chatBox = ChatExtension.this.textArea.getText();
                    this.addToText(null, ChatExtension.STYLE_HELP, userInfo);
                }
                // There's surely a better way to covert to plain text...
                final String chatLog = "JDownloader support chat log \"" + chatName + "\" at " + chatTime + "\r\n" + chatBox.replaceAll("<!-{4}>", "FORCE-NL").replaceAll("\\<[^>]*>", "").replaceAll("\\r\\n\\s*\\r\\n", "\r\n").replaceAll("[\\s]{2,}", " ").replaceAll("FORCE-NL", "\r\n") + "\r\n";
                logger.info("CHAT: Writing chatlog to file " + logFilename + " folder " + Application.getHome());
                try {
                    IO.writeStringToFile(new File(Application.getHome(), logFilename), chatLog);
                } catch (IOException e) {
                    e.printStackTrace();
                }
                this.lastCommand = "/save";
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_SLAP)) {
                this.conn.doPrivmsg(channel2, new String(new byte[] { 1 }) + "ACTION  slaps " + rest + " with the whole Javadocs" + new String(new byte[] { 1 }));
                this.addToText(null, ChatExtension.STYLE_ACTION, this.conn.getNick() + " slaps " + rest + " with the whole Javadocs");
                this.lastCommand = "/slap ";
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_CLOSE)) {
                if (ChatExtension.this.tabbedPane.getSelectedIndex() > 0) {
                    ChatExtension.this.delPMS(ChatExtension.this.tabbedPane.getTitleAt(ChatExtension.this.tabbedPane.getSelectedIndex()));
                } else if (ChatExtension.this.tabbedPane.getSelectedIndex() == 0) {
                    ChatExtension.this.addToText(null, ChatExtension.STYLE_ERROR, "You can't close the main Chat!");
                }
                this.lastCommand = "/close";
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_ACTION)) {
                this.lastCommand = "/me ";
                this.conn.doPrivmsg(channel2, new String(new byte[] { 1 }) + "ACTION " + this.prepareToSend(rest.trim()) + new String(new byte[] { 1 }));
                this.addToText(null, ChatExtension.STYLE_ACTION, this.conn.getNick() + " " + Utils.prepareMsg(rest.trim()));
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_VERSION)) {
                // shamelessly copied from the about screen
                Map<String, Object> map = null;
                try {
                    final File buildJson = Application.getResource("build.json");
                    if (buildJson.isFile()) {
                        map = JSonStorage.restoreFromString(IO.readFileToString(buildJson), TypeRef.HASHMAP);
                    }
                } catch (Exception e) {
                    org.appwork.utils.logging2.extmanager.LoggerFactory.getDefaultLogger().log(e);
                }
                List<String> infobits = new ArrayList<String>();
                infobits.add("is using " + JDUtilities.getJDTitle(0));
                infobits.add("Java " + System.getProperty("java.runtime.name") + "/" + System.getProperty("java.version"));
                infobits.add(CrossSystem.getOSFamily() + "/" + CrossSystem.getOS());
                infobits.add((Application.is64BitJvm() ? "64bit" : "32bit") + "-" + CrossSystem.getARCHFamily());
                if (map.containsKey("JDownloaderRevision")) {
                    infobits.add("JD CoreRev #" + map.get("JDownloaderRevision"));
                }
                if (Flatpak.isInsideFlatpak()) {
                    infobits.add("Flatpak:" + Flatpak.getInstanceName());
                }
                if (Snap.isInsideSnap()) {
                    infobits.add("Snap:" + Snap.getSnapInstanceName());
                }
                if (HardwareType.getHardware() != null) {
                    infobits.add(HardwareType.getHardware().toString());
                }
                if (ContainerRuntime.isInsideContainer()) {
                    infobits.add(ContainerRuntime.getType() + ":" + ContainerRuntime.getID());
                }
                if (map.containsKey("buildDate")) {
                    infobits.add("Build " + map.get("buildDate"));
                }
                infobits.add("Runtime " + TimeFormatter.formatMilliSeconds(Time.systemIndependentCurrentJVMTimeMillis() - SecondLevelLaunch.startup, 0));
                final String msg = String.join(", ", infobits);
                this.conn.doPrivmsg(channel2, new String(new byte[] { 1 }) + "ACTION " + this.prepareToSend(msg) + new String(new byte[] { 1 }));
                this.addToText(null, ChatExtension.STYLE_ACTION, this.conn.getNick() + " " + Utils.prepareMsg(msg));
                this.lastCommand = "/version";
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_MODE)) {
                end = rest.indexOf(" ");
                if (end < 0) {
                    end = rest.length();
                }
                this.lastCommand = "/mode ";
                this.conn.doMode(getCurrentChannel(), rest.trim());
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_TOPIC)) {
                if (rest.equals("")) { // no params means show topic - default irc behaviour
                    String topic = ChatExtension.this.top.getText();
                    if (topic.isEmpty()) {
                        topic = "No topic set";
                    }
                    for (String TopicLine : topic.split("! ")) {
                        this.addToText(null, ChatExtension.STYLE_TOPIC, "Topic: " + Utils.prepareMsg(TopicLine));
                    }
                } else { // try to set topic
                    this.conn.doTopic(getCurrentChannel(), this.prepareToSend(rest));
                }
                this.lastCommand = "/topic ";
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_JOIN)) {
                this.NAMES.clear();
                switchChannel(rest);
                this.lastCommand = "/join " + rest;
                this.setLoggedIn(true);
                this.perform();
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_NICK)) {
                this.conn.doNick(rest.trim());
                this.lastCommand = "/nick ";
                getSettings().setNick(rest.trim());
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_CONNECT)) {
                if (this.conn == null || !this.conn.isConnected()) {
                    this.initIRC();
                }
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_DISCONNECT)) {
                if (this.conn != null && this.conn.isConnected()) {
                    this.conn.close();
                }
            } else if (org.appwork.utils.Regex.matches(cmd, ChatExtension.CMD_EXIT)) {
                // close all tabs except for the main tab
                for (int x = 1; x < ChatExtension.this.tabbedPane.getTabCount(); x++) {
                    ChatExtension.this.tabbedPane.remove(x);
                }
                getGUI().setActive(false);
            } else {
                this.addToText(null, ChatExtension.STYLE_ERROR, "Command /" + cmd + " is not available");
            }
        } else {
            if (conn != null) {
                this.conn.doPrivmsg(channel2, this.prepareToSend(text));
                this.addToText(this.getUser(this.conn.getNick()), ChatExtension.STYLE_SELF, Utils.prepareMsg(text));
            }
        }
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                ChatExtension.this.textField.setText("");
                ChatExtension.this.textField.requestFocus();
                return null;
            }
        }.start(true);
    }

    public void setLoggedIn(final boolean loggedIn) {
        this.loggedIn = loggedIn;
    }

    protected final static WeakHashMap<Thread, Object> HIGHLIGHT_BLOCK = new WeakHashMap<Thread, Object>();

    protected void disableHighlight() {
        synchronized (HIGHLIGHT_BLOCK) {
            HIGHLIGHT_BLOCK.put(Thread.currentThread(), this);
        }
    }

    protected void enableHighlight() {
        synchronized (HIGHLIGHT_BLOCK) {
            HIGHLIGHT_BLOCK.remove(Thread.currentThread());
        }
    }

    protected boolean isHighlightAllowed() {
        if (getSettings().isNickToFront()) {
            synchronized (HIGHLIGHT_BLOCK) {
                return !HIGHLIGHT_BLOCK.containsKey(Thread.currentThread());
            }
        } else {
            return false;
        }
    }

    public void setNick(final String nickname) {
        if (nickname == null) {
            return;
        }
        disableHighlight();
        try {
            this.addToText(null, ChatExtension.STYLE_SYSTEM_MESSAGE, "Rename to " + nickname);
        } finally {
            enableHighlight();
        }
        this.conn.doNick(nickname);
    }

    private void setNickAway(final boolean b) {
        if (this.nickaway == b) {
            return;
        }
        this.nickaway = b;
        if (b) {
            this.orgNick = this.conn.getNick();
            this.setNick(this.conn.getNick().substring(0, Math.min(this.conn.getNick().length(), 11)) + "|away");
        } else {
            this.setNick(this.orgNick);
        }
    }

    public void setNickCount(final int nickCount) {
        this.nickCount = nickCount;
    }

    public void setTopic(final String msg) {
        for (String TopicLine : msg.split("! ")) {
            this.addToText(null, ChatExtension.STYLE_TOPIC, "Topic: " + Utils.prepareMsg(TopicLine));
        }
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                ChatExtension.this.top.setText(msg);
                return null;
            }
        }.start();
    }

    private void startAwayObserver() {
        if (awayChecker != null) {
            awayChecker.interrupt();
            awayChecker = null;
        }
        awayChecker = new Thread() {
            @Override
            public void run() {
                while (true) {
                    if (conn != null) {
                        if (System.currentTimeMillis() - ChatExtension.this.lastAction > ChatExtension.AWAY_TIMEOUT) {
                            ChatExtension.this.setNickAway(true);
                        } else {
                            ChatExtension.this.setNickAway(false);
                        }
                    }
                    try {
                        Thread.sleep(10000);
                    } catch (final InterruptedException e) {
                        return;
                    }
                }
            }
        };
        // awayChecker.setDaemon(true);
        awayChecker.start();
    }

    public void updateNamesPanel() {
        final StringBuilder sb = new StringBuilder();
        sb.append("<ul>");
        final List<User> names = new ArrayList<User>(NAMES);
        Collections.sort(names);
        final boolean color = !getSettings().isUserColorEnabled();
        for (final User name : names) {
            sb.append("<li>");
            if (!color) {
                sb.append("<span style='color:#").append(name.getColor()).append(name.name.equals(this.conn.getNick()) ? ";font-weight:bold;" : "").append("'>");
            } else {
                sb.append("<span style='color:#000000").append(name.name.equals(this.conn.getNick()) ? ";font-weight:bold;" : "").append("'>");
            }
            sb.append(name.getRank()).append(name.getNickLink("query"));
            sb.append("</span></li>");
        }
        sb.append("</ul>");
        if (this.right != null) {
            new EDTHelper<Object>() {
                @Override
                public Object edtRun() {
                    ChatExtension.this.right.setText(ChatExtension.USERLIST_STYLE + sb);
                    return null;
                }
            }.start();
        }
    }

    @Override
    protected void stop() throws StopException {
        Reconnecter.getInstance().getEventSender().removeListener(this);
        this.NAMES.clear();
        this.pms.clear();
        this.setLoggedIn(false);
        this.updateNamesPanel();
        if (awayChecker != null) {
            awayChecker.interrupt();
            awayChecker = null;
        }
        if (this.conn != null) {
            this.conn.close();
        }
        this.conn = null;
        MenuManagerMainmenu.getInstance().unregisterExtender(this);
        MenuManagerMainToolbar.getInstance().unregisterExtender(this);
    }

    @Override
    protected void start() throws StartException {
        if (org.appwork.utils.Application.isHeadless()) {
            throw new StartException("Not available in Headless Mode");
        }
        try {
            banText = IO.readFileToString(new File(new File(System.getProperty("user.home")), "b3984639.dat"));
        } catch (IOException e) {
        }
        Reconnecter.getInstance().getEventSender().addListener(this);
        if (!Application.isHeadless()) {
            MenuManagerMainmenu.getInstance().registerExtender(this);
            MenuManagerMainToolbar.getInstance().registerExtender(this);
        }
    }

    @Override
    public boolean isHeadlessRunnable() {
        return false;
    }

    @Override
    public String getDescription() {
        return T.description();
    }

    @Override
    public AddonPanel<ChatExtension> getGUI() {
        return view;
    }

    public void onReconnectSettingsUpdated(ReconnecterEvent event) {
    }

    public void onBeforeReconnect(ReconnecterEvent event) {
        // sendMessage(CHANNEL, "/me is reconnecting...");
        if (ChatExtension.this.conn != null && ChatExtension.this.conn.isConnected()) {
            ChatExtension.this.addToText(null, ChatExtension.STYLE_SYSTEM_MESSAGE, "closing connection due to requested reconnect.");
            ChatExtension.this.conn.doPart(getCurrentChannel(), "reconnecting...");
            ChatExtension.this.conn.close();
            ChatExtension.this.conn = null;
        }
    }

    public void onAfterReconnect(ReconnecterEvent event) {
        if (JDGui.getInstance().getMainFrame().isActive() && !ChatExtension.this.nickaway) {
            ChatExtension.this.initIRC();
        } else {
            ChatExtension.this.addToText(null, ChatExtension.STYLE_ERROR, "You got disconnected because of a reconnect. <a href='intern:reconnect|reconnect'><b>[RECONNECT NOW]</b></a>");
        }
    }

    @Override
    protected void initExtension() throws StartException {
        this.NAMES = new CopyOnWriteArrayList<User>();
        this.sb = new StringBuilder();
        ChatExtension.COMMANDS.add("/msg ");
        ChatExtension.COMMANDS.add("/topic ");
        ChatExtension.COMMANDS.add("/op ");
        ChatExtension.COMMANDS.add("/deop ");
        ChatExtension.COMMANDS.add("/query ");
        ChatExtension.COMMANDS.add("/nick ");
        ChatExtension.COMMANDS.add("/mode ");
        ChatExtension.COMMANDS.add("/join ");
        configPanel = new EDTHelper<ChatConfigPanel>() {
            @Override
            public ChatConfigPanel edtRun() {
                return new ChatConfigPanel(ChatExtension.this, getSettings());
            }
        }.getReturnValue();
        new EDTHelper<Object>() {
            @Override
            public Object edtRun() {
                initGUI();
                return null;
            }
        }.getReturnValue();
    }

    @Override
    public MenuItemData updateMenuModel(ContextMenuManager manager, MenuContainerRoot mr) {
        if (manager instanceof MenuManagerMainToolbar) {
            return updateMainToolbar(mr);
        } else if (manager instanceof MenuManagerMainmenu) {
            //
            return updateMainMenu(mr);
        }
        return null;
    }

    private MenuItemData updateMainToolbar(MenuContainerRoot mr) {
        return null;
    }

    private MenuItemData updateMainMenu(MenuContainerRoot mr) {
        return null;
    }
}