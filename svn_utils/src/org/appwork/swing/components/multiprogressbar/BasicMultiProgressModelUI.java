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
package org.appwork.swing.components.multiprogressbar;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JComponent;
import javax.swing.plaf.ComponentUI;

import org.appwork.utils.ColorUtils;

/**
 * @author Thomas
 * 
 */
public class BasicMultiProgressModelUI extends MultiProgressBarUI implements MultiProgressListener {

    public BasicMultiProgressModelUI() {
        super();

    }

    public static ComponentUI createUI(final JComponent x) {
        return new BasicMultiProgressModelUI();
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        Graphics2D g2 = (Graphics2D) g;
        MultiProgressBar bar = (MultiProgressBar) c;
        Range[] ranges = bar.getModel().getRanges();
        long max = bar.getModel().getMaximum();
        if (max == 0) return;
        if (ranges == null || ranges.length == 0) return;
        int w = bar.getWidth();
        int h = bar.getHeight();
        int to = 0;
        int from = 0;
        for (Range r : ranges) {

            Color color = r.getColor() != null ? r.getColor() : bar.getForeground();

            g2.setPaint(new GradientPaint(w / 2, h, color, w / 2, 0, ColorUtils.getAlphaInstance(color, 40)));

            to = (int) (((r.getTo() - r.getFrom()) * w) / max)+1;
            from = (int) ((r.getFrom() * w) / max);
            g2.fillRect(from, 0, to, h);
            g2.setColor(color);
            g2.drawLine(from, 0, from, h);
        }

    }

    private MultiProgressBar bar;

    private void installListeners() {
        bar.getEventSender().addListener(this);

    }

    @Override
    public void installUI(final JComponent c) {
        this.bar = (MultiProgressBar) c;
        this.installListeners();

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.appwork.swing.components.multiprogressbar.MultiProgressListener#onChanged
     * ()
     */
    @Override
    public void onChanged() {
        bar.repaint();
    }
}
