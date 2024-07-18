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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.storage.flexijson;

public class KeyValueElement {
    private String               key;
    /**
     * Path is optional and may get set by the parser. By default, the path is NOT set due to performance reasons. override the extendPath
     * Method in the parser
     */
    public final Object          path;
    public final FlexiJSonObject parent;

    public String getKey() {
        return key;
    }

    public KeyValueElement(FlexiJSonObject parent, Object path, String key, FlexiJSonNode value) {
        super();
        this.key = key;
        this.value = value;
        this.path = path;
        this.parent = parent;
    }

    public KeyValueElement(FlexiJSonObject parent, String key, FlexiJSonNode value) {
        this(parent, null, key, value);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (commentsBeforeKey != null) {
            for (FlexiCommentJsonNode c : commentsBeforeKey) {
                sb.append(c);
            }
        }
        sb.append(key);
        if (commentsAfterKey != null) {
            for (FlexiCommentJsonNode c : commentsAfterKey) {
                sb.append(c);
            }
        }
        sb.append("=");
        sb.append(value);
        return sb.toString();
    }

    protected void setKey(String key) {
        this.key = key;
    }

    public FlexiJSonNode getValue() {
        return value;
    }

    public void setValue(FlexiJSonNode value) {
        this.value = value;
    }

    private FlexiJSonNode     value;
    private FlexiJSonComments commentsBeforeKey;

    public FlexiJSonComments getCommentsBeforeKey() {
        return commentsBeforeKey;
    }

    public void setCommentsBeforeKey(FlexiJSonComments commentsBeforeKey) {
        this.commentsBeforeKey = commentsBeforeKey;
        if (commentsBeforeKey != null) {
            // we probably do not have the value yet and will set the parent later
            commentsBeforeKey.setParent(value);
            for (FlexiCommentJsonNode c : commentsBeforeKey) {
                c.setLocation(FlexiCommentJsonNode.AttachLocation.BEFORE_KEY);
            }
        }
    }

    public FlexiJSonComments getCommentsAfterKey() {
        return commentsAfterKey;
    }

    public void setCommentsAfterKey(FlexiJSonComments commentsAfterKey) {
        this.commentsAfterKey = commentsAfterKey;
        if (commentsAfterKey != null) {
            // we probably do not have the value yet and will set the parent later
            commentsAfterKey.setParent(value);
            for (FlexiCommentJsonNode c : commentsAfterKey) {
                c.setLocation(FlexiCommentJsonNode.AttachLocation.AFTER_KEY);
            }
        }
    }

    private FlexiJSonComments commentsAfterKey;
    private int               order = 0;

    public int getOrder() {
        return order;
    }

    /**
     * @param commentsBeforeKey2
     * @param append
     *            TODO
     */
    public void addCommentsBeforeKey(FlexiJSonComments commentsBeforeKey2, boolean append) {
        if (commentsBeforeKey2 == null) {
            return;
        }
        // we probably do not have the value yet and will set the parent later
        commentsBeforeKey2.setParent(value);
        for (FlexiCommentJsonNode c : commentsBeforeKey2) {
            c.setLocation(FlexiCommentJsonNode.AttachLocation.BEFORE_KEY);
        }
        if (commentsBeforeKey == null) {
            commentsBeforeKey = commentsBeforeKey2;
        } else {
            if (append) {
                commentsBeforeKey.addAll(commentsBeforeKey2);
            } else {
                commentsBeforeKey.addAll(0, commentsBeforeKey2);
            }
        }
    }

    public void addCommentsAfterKey(FlexiJSonComments c) {
        if (c == null) {
            return;
        }
        // we probably do not have the value yet and will set the parent later
        c.setParent(value);
        for (FlexiCommentJsonNode comment : c) {
            comment.setLocation(FlexiCommentJsonNode.AttachLocation.AFTER_KEY);
        }
        if (commentsAfterKey == null) {
            commentsAfterKey = c;
        } else {
            commentsAfterKey.addAll(c);
        }
    }

    public boolean hasComments() {
        if (commentsAfterKey != null && commentsAfterKey.size() > 0) {
            return true;
        } else if (commentsBeforeKey != null && commentsBeforeKey.size() > 0) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param value2
     */
    public void setOrder(int order) {
        this.order = order;
    }
}