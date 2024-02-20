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
package org.appwork.swing.components;

import java.awt.Component;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.util.Comparator;
import java.util.TreeSet;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.appwork.utils.Application;
import org.appwork.utils.CompareUtils;
import org.appwork.utils.ImageProvider.ImageProvider;

public class ExtMergedIcon implements Icon, IDIcon {
    protected class Entry {

        public final Icon      icon;
        public final int       x;
        public final int       y;
        public final Composite composite;
        public final int       z;
        public final Shape     clip;

        public Entry(final Icon icon, final int x, final int y, final int z, final Composite c, Shape clip) {
            this.icon = icon;
            this.x = x;
            this.y = y;
            this.composite = c;
            this.z = z;
            this.clip = clip;
        }

    }

    private int                    cropedWidth  = -1;
    private int                    cropedHeight = -1;

    protected final TreeSet<Entry> entries      = new TreeSet<Entry>(new Comparator<Entry>() {

                                                    @Override
                                                    public int compare(final Entry o1, final Entry o2) {
                                                        return CompareUtils.compareInt(o1.z, o2.z);
                                                    }
                                                });

    private int                    width        = 0;
    private int                    height       = 0;
    private ImageIcon              internalIcon;

    private boolean                caching;
    protected IconIdentifier       internalID;

    public ExtMergedIcon() {

    }

    /**
     * @param abstractIcon
     */
    public ExtMergedIcon(final Icon icon) {
        this(icon, 0, 0, 0, null);
    }

    /**
     * @param abstractIcon
     * @param i
     * @param j
     */
    public ExtMergedIcon(final Icon icon, final int x, final int y) {
        this.addEntry(new Entry(icon, x, y, 0, null, null));
    }

    public ExtMergedIcon(final Icon icon, final int x, final int y, final int z, final Composite c) {
        this.addEntry(new Entry(icon, x, y, z, c, null));
    }

    public ExtMergedIcon add(final Icon icon) {
        synchronized (this.entries) {
            return this.add(icon, 0, 0, this.entries.size(), null);
        }
    }

    /**
     * @param abstractIcon
     * @param i
     * @param j
     * @return
     */
    public ExtMergedIcon add(final Icon icon, final int x, final int y) {
        synchronized (this.entries) {
            return this.add(icon, x, y, this.entries.size(), null);
        }

    }

    public ExtMergedIcon add(final Icon icon, final int x, final int y, final int z, final Composite c, Shape clip) {
        this.addEntry(new Entry(icon, x, y, z, c, clip));
        return this;
    }

    public ExtMergedIcon add(final Icon icon, final int x, final int y, final int z, final Composite c) {
        this.addEntry(new Entry(icon, x, y, z, c, null));
        return this;
    }

    private void addEntry(final Entry entry) {
        if (this.internalIcon != null) {
            throw new IllegalStateException("internalIcon is set");
        }
        this.width = Math.max(this.width, entry.x + entry.icon.getIconWidth());
        this.height = Math.max(this.height, entry.y + entry.icon.getIconHeight());
        idIconCheck(entry);
        this.entries.add(entry);

    }

    protected void idIconCheck(final Entry entry) {
        if (!(entry.icon instanceof IDIcon)) {
            if (!Application.isJared(null)) {
                new Exception(this.getClass() + ": Warning. Not an  IDIcon").printStackTrace();
            }
        }
    }

    public void cache() {
        this.caching = true;
        try {
            this.internalIcon = ImageProvider.toImageIcon(this);
            this.internalID = this.getIdentifier();
            this.entries.clear();
        } finally {
            this.caching = false;
        }
    }


    public ExtMergedIcon crop(final int width, final int height) {
        this.cropedWidth = width;
        this.cropedHeight = height;

        return this;
    }

    @Override
    public int getIconHeight() {
        if (this.cropedHeight > 0) {
            return this.cropedHeight;
        }
        return this.height;
    }

    @Override
    public int getIconWidth() {
        if (this.cropedWidth > 0) {
            return this.cropedWidth;
        }
        return this.width;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {

        final Graphics2D g2 = (Graphics2D) g;

        if (this.internalIcon == null && !this.caching) {
            this.cache();

        }

        if (this.internalIcon != null) {
            g2.drawImage(this.internalIcon.getImage(), x, y, null);
            // internalIcon.paintIcon(c, g2, x, y);
            return;
        }
        final Shape oldClip = g2.getClip();
        // Rectangle rec = new Rectangle( );
        g2.setClip(x, y, this.getIconWidth(), this.getIconHeight());
        Shape defClip = g2.getClip();
        for (final Entry e : this.entries) {
            if (e.clip != null) {
                g2.setClip(e.clip);
            }
            paintSingleIcon(c, x, y, g2, e);
            g2.setClip(defClip);
        }
        g2.setClip(oldClip);
    }

    protected void paintSingleIcon(final Component c, final int x, final int y, final Graphics2D g2, final Entry e) {
        final Composite com = g2.getComposite();
        try {
            if (e.composite != null) {
                g2.setComposite(e.composite);
            }
            e.icon.paintIcon(c, g2, x + e.x, y + e.y);
        } finally {
            if (com != null) {
                g2.setComposite(com);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.appwork.swing.components.IdentifierInterface#toIdentifier()
     */
    @Override
    public IconIdentifier getIdentifier() {

        if (this.internalID != null) {
            return this.internalID;
        }
        IconIdentifier t = new IconIdentifier("Merge");
        if (this.cropedHeight > 0) {
            t.addProperty("cropedHeight", this.cropedHeight);
        }
        if (this.cropedWidth > 0) {
            t.addProperty("cropedWidth", this.cropedWidth);
        }
        t.addProperty("height", this.getIconHeight());
        t.addProperty("width", this.getIconWidth());

        for (Entry e : this.entries) {
            if (e.icon instanceof IDIcon) {
                IconIdentifier id = ((IDIcon) e.icon).getIdentifier();
                if (e.x != 0) {
                    id.addProperty("x", e.x);
                }
                if (e.y != 0) {
                    id.addProperty("y", e.y);
                }
                id.addProperty("height", e.icon.getIconHeight());
                id.addProperty("width", e.icon.getIconWidth());
                t.add(id);
                //

            } else {
                t.add(new IconIdentifier("unknown", e.icon.toString()));

            }
        }

        return t;
    }

}