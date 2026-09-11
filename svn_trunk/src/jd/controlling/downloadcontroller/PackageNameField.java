package jd.controlling.downloadcontroller;

import javax.swing.JTextField;

import org.appwork.swing.MigPanel;

/**
 * Read-only package name display used by the download controller dialogs. Similar to {@link FileNameField} but without the name/extension
 * split - a package name is shown as a single read-only field.
 */
public class PackageNameField extends MigPanel {
    private final JTextField nameField;

    public PackageNameField(final String packagename) {
        super("ins 0", "[grow,fill]", "[]");
        this.nameField = new JTextField(packagename);
        this.nameField.setEditable(false);
        add(this.nameField, "growx, pushx, wmin 100");
    }

    public JTextField getNameField() {
        return this.nameField;
    }
}
