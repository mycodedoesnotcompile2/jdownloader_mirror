package jd.controlling.downloadcontroller;

import javax.swing.JTextField;

import org.appwork.swing.MigPanel;

import jd.plugins.ParsedFilename;

/**
 * Reusable filename display used by the download controller dialogs: an (optionally editable) name field next to a read-only extension
 * field. The name/extension split is derived via {@link ParsedFilename}; when there is no extension only the name field is shown. Whether
 * the name field is editable (read-only) is controlled via the constructor, so the same component renders both read-only display filenames
 * and a user-editable custom filename.
 */
class FileNameField extends MigPanel {
    private final JTextField nameField;
    private final JTextField extensionField;
    private final String     extension;

    FileNameField(final String filename, final boolean nameEditable) {
        this(new ParsedFilename(filename), nameEditable);
    }

    FileNameField(final ParsedFilename filename, final boolean nameEditable) {
        super("ins 0", "[grow,fill][]", "[]");
        this.extension = filename.getExtensionAdvanced();
        this.nameField = new JTextField(filename.getFilenameWithoutExtensionAdvanced());
        this.nameField.setEditable(nameEditable);
        add(this.nameField, "growx, pushx, wmin 100");
        if (this.extension != null && this.extension.length() > 0) {
            /* Extension is shown in its own field and is always read-only. */
            this.extensionField = new JTextField(this.extension);
            this.extensionField.setEditable(false);
            /*
             * Long names otherwise squeeze the extension field down to near-zero width. Keep it between 4 and 10 characters wide
             * (approximated from the font's character width).
             */
            final java.awt.FontMetrics fm = this.extensionField.getFontMetrics(this.extensionField.getFont());
            final int charWidth = fm.charWidth('m');
            final int horizontalPadding = 8;
            add(this.extensionField, "wmin " + (charWidth * 4 + horizontalPadding) + ", wmax " + (charWidth * 10 + horizontalPadding));
        } else {
            this.extensionField = null;
        }
    }

    JTextField getNameField() {
        return this.nameField;
    }
}
