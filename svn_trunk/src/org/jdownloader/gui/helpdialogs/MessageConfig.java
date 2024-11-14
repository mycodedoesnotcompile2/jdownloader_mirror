package org.jdownloader.gui.helpdialogs;

import java.awt.Point;

import javax.swing.Icon;

public class MessageConfig {
    private Point point = null;

    public MessageConfig setPoint(Point point) {
        this.point = point;
        return this;
    }

    public MessageConfig setDontShowAgainKey(String dontShowAgainKey) {
        this.dontShowAgainKey = dontShowAgainKey;
        return this;
    }

    private String  dontShowAgainKey = null;
    private int     flags            = 0;
    private String  title;
    private String  msg;
    private Icon    icon;
    private Boolean expandToBottom   = null;
    private Boolean expandToRight    = null;

    public MessageConfig() {
    }

    public MessageConfig(int flags, String title, String msg, Icon icon) {
        this.title = title;
        this.msg = msg;
        this.icon = icon;
    }

    public MessageConfig(Point point, String dontShowAgainKey, int flags, String title, String msg, Icon icon) {
        this.point = point;
        this.dontShowAgainKey = dontShowAgainKey;
        this.flags = flags;
        this.title = title;
        this.msg = msg;
        this.icon = icon;
    }

    public Point getPoint() {
        return point;
    }

    public String getDontShowAgainKey() {
        return dontShowAgainKey;
    }

    public int getFlags() {
        return flags;
    }

    public MessageConfig setFlags(int flags) {
        this.flags = flags;
        return this;
    }

    public String getTitle() {
        return title;
    }

    public MessageConfig setTitle(String title) {
        this.title = title;
        return this;
    }

    public String getMsg() {
        return msg;
    }

    public MessageConfig setMsg(String msg) {
        this.msg = msg;
        return this;
    }

    public Icon getIcon() {
        return icon;
    }

    public MessageConfig setIcon(Icon icon) {
        this.icon = icon;
        return this;
    }

    public Boolean getExpandToBottom() {
        return expandToBottom;
    }

    public MessageConfig setExpandToBottom(Boolean expandToBottom) {
        this.expandToBottom = expandToBottom;
        return this;
    }

    public Boolean getExpandToRight() {
        return expandToRight;
    }

    public MessageConfig setExpandToRight(Boolean expandToRight) {
        this.expandToRight = expandToRight;
        return this;
    }
}
