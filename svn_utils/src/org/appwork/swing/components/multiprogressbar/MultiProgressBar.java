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

import javax.swing.JComponent;

public class MultiProgressBar extends JComponent {
    /**
     * @see #getUIClassID
     */
    private static final String        UI_CLASS_ID = "MultiProgressBarUI";
    private MultiProgressModel         model;
    private MultiProgressModelListener listener;
    private MultiProgressEventSender   eventSender;
    public MultiProgressEventSender getEventSender() {
        return eventSender;
    }

    private MultiProgressEvent         changeEvent;

    public MultiProgressBar(long max) {
        this(new MultiProgressModel(max));

    }

    /**
     * Resets the UI property to a value from the current look and feel.
     * 
     * @see JComponent#updateUI
     */
    @Override
    public void updateUI() {
        // final CircleProgressBarUI newUI = (CircleProgressBarUI)
        // UIManager.getUI(this);
        this.setUI(new BasicMultiProgressModelUI());
    }

    public synchronized void setModel(final MultiProgressModel model) {
        if (this.model == model) { return; }
        if (this.model != null) {
            model.getEventSender().removeListener(this.listener);
        }
        this.listener = new MultiProgressModelListener() {

            @Override
            public void onChanged() {
                eventSender.fireEvent(changeEvent);
            }

        };
        model.getEventSender().addListener(this.listener);
        this.model = model;

        this.repaint();

    }

    public void setUI(final MultiProgressBarUI ui) {
        super.setUI(ui);

    }

    /**
     * @param multiProgressModel
     */
    public MultiProgressBar(MultiProgressModel multiProgressModel) {
        eventSender = new MultiProgressEventSender();
        changeEvent = new MultiProgressEvent(this);
        setModel(multiProgressModel);
        this.updateUI();
    }

    /**
     * 
     */
    public MultiProgressModel getModel() {
        return model;
    }
}
