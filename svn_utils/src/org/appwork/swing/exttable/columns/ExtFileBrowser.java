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
package org.appwork.swing.exttable.columns;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JTextField;

import org.appwork.swing.MigPanel;
import org.appwork.utils.swing.SwingUtils;

/**
 * @author Thomas
 * 
 */
public abstract class ExtFileBrowser<T> extends ExtComponentColumn<T> implements ActionListener {
    /**
     * 
     */
    private static final long serialVersionUID = -6538541922869223568L;
    protected MigPanel renderer;
    private JTextField rendererLabel;
    private JButton    rendererButton;
    protected MigPanel editor;
    private JButton    editorButton;
    private JTextField editorLabel;
    private T          editObject;

    private Color      bg;
    private Color      fg;

    @Override
    public void actionPerformed(ActionEvent e) {
        getModel().setSelectedObject(editObject);
        File newFile = browse(editObject);
        setFile(editObject, newFile);
        cancelCellEditing();

    }

    /**
     * @param editObject2
     * @param newFile
     */
    abstract protected void setFile(T object, File newFile);

    /**
     * @param editObject2
     * @return
     */
    abstract public File browse(T object);

    public ExtFileBrowser(String name) {
        super(name);

        rendererButton = new JButton("Browse");

        // renderer
        this.rendererLabel = new JTextField();
        rendererLabel.setEditable(false);
        SwingUtils.setOpaque(rendererLabel, false);
        rendererLabel.setBorder(null);
        this.renderer = new MigPanel("ins 0", "[grow,fill]0[]", "[grow,fill]") {
            /**
             * 
             */
            private static final long serialVersionUID = -2099450346231216292L;

            public void setForeground(Color fg) {
                super.setForeground(fg);
                rendererLabel.setForeground(fg);

            }

            public void setBackground(Color bg) {
                super.setBackground(bg);
            }

            public void setVisible(boolean aFlag) {
                rendererLabel.setVisible(aFlag);
                rendererButton.setVisible(aFlag);
            }

        };
        renderer.setOpaque(false);
        renderer.add(rendererLabel);
        renderer.add(rendererButton);
        // editor

        bg = rendererLabel.getBackground();
        fg = rendererLabel.getForeground();

        editorButton = new JButton("Browse");

        editorButton.addActionListener(this);
        editorLabel = new JTextField();
        editorLabel.setEditable(false);
        SwingUtils.setOpaque(editorLabel, false);
        editorLabel.addMouseListener(new MouseAdapter() {

            @Override
            public void mouseClicked(MouseEvent e) {
                getModel().setSelectedObject(editObject);
            }
        });
        editorLabel.setBorder(null);
        this.editor = new MigPanel("ins 0", "[grow,fill]0[]", "[grow,fill]") {
            /**
             * 
             */
            private static final long serialVersionUID = -8432598231289793141L;

            public void setForeground(Color fg) {
                super.setForeground(fg);
                editorLabel.setForeground(fg);

            }

            public void setVisible(boolean aFlag) {
                editorLabel.setVisible(aFlag);
                editorButton.setVisible(aFlag);
            }

            public void setBackground(Color bg) {
                super.setBackground(bg);
            }

        };
        editor.setOpaque(false);
        editor.add(editorLabel);
        editor.add(editorButton);

    }

    @Override
    protected JComponent getInternalEditorComponent(T value, boolean isSelected, int row, int column) {
        // TODO Auto-generated method stub
        return editor;
    }

    @Override
    protected JComponent getInternalRendererComponent(T value, boolean isSelected, boolean hasFocus, int row, int column) {
        return renderer;
    }

    @Override
    public void configureEditorComponent(T value, boolean isSelected, int row, int column) {
     
            editObject = value;
            editorLabel.setText(getFile(value) + "");
            editorLabel.setCaretPosition(0);
        

    }

    @Override
    public void configureRendererComponent(T value, boolean isSelected, boolean hasFocus, int row, int column) {
        rendererLabel.setText(getFile(value) + "");

    }

    /**
     * @param value
     * @return
     */
    abstract public File getFile(T value);

    @Override
    public void resetEditor() {
        editor.setForeground(fg);
        editor.setBackground(bg);
        editor.setOpaque(false);
    }

    @Override
    public void resetRenderer() {
        renderer.setForeground(fg);
        renderer.setBackground(bg);
        renderer.setOpaque(false);
    }

}
