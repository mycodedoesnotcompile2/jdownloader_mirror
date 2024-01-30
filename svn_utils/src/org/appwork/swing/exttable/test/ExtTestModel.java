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
package org.appwork.swing.exttable.test;

import java.io.File;

import javax.swing.Icon;

import org.appwork.swing.exttable.ExtTableIcon;
import org.appwork.swing.exttable.ExtTableModel;
import org.appwork.swing.exttable.columns.ExtFileBrowser;
import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.appwork.utils.swing.dialog.ExtFileChooserDialog;
import org.appwork.utils.swing.dialog.FileChooserSelectionMode;
import org.appwork.utils.swing.dialog.FileChooserType;

/**
 * @author thomas
 *
 */
public class ExtTestModel extends ExtTableModel<TextObject> {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * @param id
     */
    public ExtTestModel() {
        super(ExtTestModel.class.getName() + "_" + 1);
        for (int i = 0; i < 100; i++) {
            this.addElement(new TextObject("a" + i, "b" + i, i + "c"));
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.utils.swing.table.ExtTableModel#initColumns()
     */
    @Override
    protected void initColumns() {      
        addColumn(new ExtFileBrowser<TextObject>("Browse me") {
            /**
             *
             */
            private static final long serialVersionUID = -7233073890074043200L;

            @Override
            public File getFile(TextObject o) {                
                return o.getFile();
            }

            @Override
            protected void setFile(TextObject object, File newFile) {
                object.setFile(newFile);
            }

            @Override
            public File browse(TextObject object) {
                ExtFileChooserDialog d = new ExtFileChooserDialog(0, "Choose file", null, null);
                d.setFileSelectionMode(FileChooserSelectionMode.FILES_AND_DIRECTORIES);
                d.setType(FileChooserType.OPEN_DIALOG);
                d.setMultiSelection(false);
                try {
                    Dialog.I().showDialog(d);
                    return d.getSelectedFile();
                } catch (DialogNoAnswerException e) {
                    e.printStackTrace();
                    return null;
                }

            }
        });
        this.addColumn(new ExtTextColumn<TextObject>("EDIT ME") {
            private static final long serialVersionUID = 1L;

            @Override
            public int getDefaultWidth() {
                return 40;
            }

            @Override
            public boolean isEnabled(TextObject obj) {
                return false;
            }

            @Override
            public boolean isEditable(final TextObject obj) {
                return true;
            }

            @Override
            protected Icon getIcon(final TextObject value) {
                return ExtTableIcon.TABLE_FINDMENU.get(14);
            }

            @Override
            public String getStringValue(final TextObject value) {
                return value.getA();
            }
        });
        this.addColumn(new ExtTextColumn<TextObject>("col 2") {
            private static final long serialVersionUID = 1L;

            @Override
            public int getDefaultWidth() {
				return 80;
            }

            @Override
            public String getStringValue(final TextObject value) {
                return value.getB();
            }
        });
        this.addColumn(new ExtTextColumn<TextObject>("col 3") {
            private static final long serialVersionUID = 1L;

            @Override
            public int getDefaultWidth() {
				return 120;
            }

            @Override
            public String getStringValue(final TextObject value) {
                return value.getC();
            }

            @Override
            protected boolean isDefaultResizable() {
				return false;
            }
        });
        this.addColumn(new ExtTextColumn<TextObject>("col 4") {
            private static final long serialVersionUID = 1L;

            @Override
            public int getDefaultWidth() {
				return 200;
            }

            @Override
            public String getStringValue(final TextObject value) {
                return value.getC() + value.getA();
            }

            @Override
            public boolean isDefaultVisible() {
                return false;
            }
        });
    }
}
