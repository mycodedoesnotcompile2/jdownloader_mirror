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
package org.appwork.storage.flexijson;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Set;

import org.appwork.storage.flexijson.mapper.FlexiJSonMapper;
import org.appwork.storage.flexijson.mapper.FlexiMapperTags;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder;
import org.appwork.storage.flexijson.stringify.FlexiJSonStringBuilder.JSONBuilderOutputStream;

/**
 * @author thomas
 *
 */
public class FlexiJSonComments extends ArrayList<FlexiCommentJsonNode> implements FlexiJSonNode {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public FlexiJSonComments() {
        super(1);
    }

    public FlexiJSonComments(Collection<FlexiCommentJsonNode> comments) {
        // do not put comments in the costructor - this would not set the parents
        super();
        addAll(comments);
    }

    /**
     * @param flexiComment
     */
    public FlexiJSonComments(FlexiCommentJsonNode... comments) {
        // do not put comments in the costructor - this would not set the parents
        super();
        addAll(Arrays.asList(comments));
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.ArrayList#add(java.lang.Object)
     */
    @Override
    public boolean add(FlexiCommentJsonNode e) {
        e.setParent(this);
        return super.add(e);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.ArrayList#add(int, java.lang.Object)
     */
    @Override
    public void add(int index, FlexiCommentJsonNode element) {
        element.setParent(this);
        super.add(index, element);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.ArrayList#addAll(java.util.Collection)
     */
    @Override
    public boolean addAll(Collection<? extends FlexiCommentJsonNode> c) {
        if (c == null) {
            return false;
        }
        for (FlexiCommentJsonNode e : c) {
            e.setParent(this);
        }
        return super.addAll(c);
    }

    public boolean addAll(int index, Collection<? extends FlexiCommentJsonNode> c) {
        if (c == null) {
            return false;
        }
        for (FlexiCommentJsonNode e : c) {
            e.setParent(this);
        }
        return super.addAll(index, c);
    }

    @Override
    public String toString() {
        return new FlexiJSonStringBuilder().toJSONString(this);
    }

    @Override
    public void setCommentsAfter(FlexiJSonComments commentsAfter) {
    }

    @Override
    public FlexiJSonComments getCommentsAfter() {
        return null;
    }

    @Override
    public FlexiJSonNode resolvePath(JSPath parent) {
        return null;
    }

    @Override
    public void setCommentsBefore(FlexiJSonComments commentsBefore) {
    }

    @Override
    public FlexiJSonComments getCommentsBefore() {
        return null;
    }

    @Override
    public void addCommentsBefore(FlexiJSonComments comments) {
    }

    @Override
    public void addCommentsAfter(FlexiJSonComments comments) {
    }

    private FlexiJSonNode parent;

    @Override
    public void setParent(FlexiJSonNode parent) {
        this.parent = parent;
    }

    @Override
    public FlexiJSonNode getParent() {
        return parent;
    }

    public boolean hasComments() {
        return false;
    }

    @Override
    public void writeToStream(FlexiJSonStringBuilder stringifier, JSONBuilderOutputStream out, int layer, LinkedList<String> path) throws IOException {
        stringifier.appendComments(this, out, layer, path);
    }

    private HashSet<FlexiMapperTags> tags;

    @Override
    public void tag(FlexiMapperTags tag) {
        if (tags == null) {
            tags = new HashSet<FlexiMapperTags>();
        }
        if (tag != null) {
            tags.add(tag);
        }
    }

    @Override
    public Set<FlexiMapperTags> getTags() {
        return tags;
    }

    /**
     * @param mapper
     * @param mergeDifferentTypes
     *
     */
    public void merge(FlexiJSonMapper mapper, boolean mergeDifferentTypes) {
        ArrayList<FlexiCommentJsonNode> newList = new ArrayList<FlexiCommentJsonNode>();
        StringBuilder sb = new StringBuilder();
        FlexiCommentJsonNode last = null;
        HashSet<FlexiMapperTags> tags = new HashSet<FlexiMapperTags>();
        for (FlexiCommentJsonNode c : this) {
            if (c instanceof FlexiComment) {
                if ((last != null && !mergeDifferentTypes && ((FlexiComment) c).getType() != ((FlexiComment) last).getType())) {
                    if (sb.length() > 0) {
                        FlexiComment com = mapper.createFlexiJsonComment(sb.toString(), null, ((FlexiComment) last).getType());
                        for (FlexiMapperTags t : tags) {
                            com.tag(t);
                        }
                        com.setLocation(last.getLocation());
                        com.setParent(this);
                        newList.add(com);
                        sb.setLength(0);
                        tags.clear();
                    }
                }
                if (sb.length() > 0) {
                    sb.append("\r\n");
                }
                sb.append(((FlexiComment) c).getText());
                if (c.getTags() != null) {
                    tags.addAll(c.getTags());
                }
                last = c;
            } else {
                if (sb.length() > 0) {
                    FlexiComment com = mapper.createFlexiJsonComment(sb.toString(), FlexiMapperTags.UNKNOWN, ((FlexiComment) last).getType());
                    com.setLocation(last.getLocation());
                    com.setParent(this);
                    newList.add(com);
                    sb.setLength(0);
                }
                newList.add(c);
                last = null;
            }
        }
        if (sb.length() > 0) {
            FlexiComment com = mapper.createFlexiJsonComment(sb.toString(), null, ((FlexiComment) last).getType());
            for (FlexiMapperTags t : tags) {
                com.tag(t);
            }
            com.setLocation(last.getLocation());
            com.setParent(this);
            newList.add(com);
            sb.setLength(0);
        }
        clear();
        addAll(newList);
    }

    /**
     *
     */
    public void cleanUpDupes() {
        HashSet<String> dupe = new HashSet<String>();
        FlexiCommentJsonNode next;
        for (Iterator<FlexiCommentJsonNode> it = this.iterator(); it.hasNext();) {
            next = it.next();
            if (next instanceof FlexiComment) {
                if (!dupe.add(((FlexiComment) next).getText().replaceAll("[\r\n\\s]+", ""))) {
                    it.remove();
                }
            }
        }
    }

    @Override
    public boolean remove(FlexiJSonNode node) {
        return remove((Object) node);
    }
}
