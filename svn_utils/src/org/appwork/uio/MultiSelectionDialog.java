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
package org.appwork.uio;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JTextPane;
import javax.swing.ListCellRenderer;

import org.appwork.storage.JSonStorage;
import org.appwork.utils.swing.dialog.AbstractDialog;

import net.miginfocom.swing.MigLayout;

/**
 * An {@link AbstractDialog} which allows for selecting multiple options from
 * given list.
 * 
 */

public class MultiSelectionDialog extends AbstractDialog<int[]> implements MultiSelectionDialogInterface {

    private String           message;
    private ListCellRenderer renderer;
    private Object[]         options;
    private JTextPane        textpane;
    private JList            listComponent;

    public MultiSelectionDialog(int flag, String title, String question, Object[] options, Icon icon, String okText, String cancelText, ListCellRenderer renderer) {
        super(flag, title, icon, okText, cancelText);
        this.message = question;
        this.renderer = renderer;
        this.options = options;
    }

    @Override
    protected int[] createReturnValue() {
        return this.getSelectedIndices();
    }

    @Override
    public JComponent layoutDialogContent() {
        final JPanel contentpane = new JPanel(new MigLayout("ins 0,wrap 1", "[fill,grow]"));
        this.textpane = new JTextPane();
        this.textpane.setBorder(null);
        this.textpane.setBackground(null);
        this.textpane.setOpaque(false);
        this.textpane.putClientProperty("Synthetica.opaque", Boolean.FALSE);
        this.textpane.setText(this.message);
        this.textpane.setEditable(false);

        contentpane.add(this.textpane);

        this.listComponent = this.createListComponent();
        contentpane.add(this.listComponent, "pushy,growy, width n:n:450,height 100::");

        return contentpane;
    }

    private JList createListComponent() {
        final JList ret = new JList(options);
        final ListCellRenderer rend = this.getRenderer(ret.getCellRenderer());
        if (rend != null) {
            ret.setCellRenderer(rend);
        }
        return ret;
    }

    public int[] getSelectedIndices() {
        return this.listComponent.getSelectedIndices();
    }

    protected ListCellRenderer getRenderer(final ListCellRenderer orgRenderer) {
        return this.renderer;
    }

    @Override
    public String getMessage() {
        return message;
    }

    @Override
    public String[] getLabels() {
        String[] ret = new String[options.length];
        for (int i = 0; i < ret.length; i++) {
            ret[i] = JSonStorage.toString(options[i]);

        }
        return ret;
    }

}
