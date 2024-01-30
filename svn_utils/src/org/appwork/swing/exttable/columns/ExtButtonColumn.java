package org.appwork.swing.exttable.columns;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;

import org.appwork.swing.MigPanel;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.renderer.RendererMigPanel;

public abstract class ExtButtonColumn<T> extends ExtComponentColumn<T> implements ActionListener {
    protected JButton  rendererButton;
    protected MigPanel renderer;
    protected JButton  editorButton;
    protected MigPanel editor;
    protected T        clickedElement;
    private int        clickedRow;

    public T getClickedElement() {
        return clickedElement;
    }

    {
        this.rendererButton = new JButton("Install");
        // renderer
        this.renderer = new RendererMigPanel("ins 0", "1[grow,fill]1", "1[]1") {
            @Override
            public void setVisible(final boolean aFlag) {
                ExtButtonColumn.this.rendererButton.setVisible(aFlag);
            }

            @Override
            public void setBounds(final int x, final int y, final int width, final int height) {
                super.setBounds(x, y, width, height);
            }
        };
        this.renderer.add(this.rendererButton);
        // editor
        // no RendererMigPanel... this would swallow some important events
        this.editorButton = new JButton("Browse");
        this.editor = new MigPanel("ins 0", "1[grow,fill]1", "1[]1") {
            private static final long serialVersionUID = -8432598231289793141L;

            @Override
            public void setVisible(final boolean aFlag) {
                ExtButtonColumn.this.editorButton.setVisible(aFlag);
            }
        };
        this.editor.add(this.editorButton);
        this.editorButton.addActionListener(this);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        getModel().fireTableDataChanged();
        final T el = clickedElement;
        // start Editing. fireTableDataChanged will stop cell editing. This must be done with invoke later
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                startEditing(el);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see javax.swing.AbstractCellEditor#stopCellEditing()
     */
    @Override
    public boolean stopCellEditing() {        
        return super.stopCellEditing();
    }

    public ExtButtonColumn(final String name) {
        super(name);
    }

    @Override
    public void resetEditor() {
        this.editorButton.setEnabled(true);
        // his.editor.setBackground(null);
        SwingUtils.setOpaque(this.editor, false);
        SwingUtils.setOpaque(this.editorButton, true);
    }

    @Override
    public void resetRenderer() {
        this.rendererButton.setEnabled(true);
        SwingUtils.setOpaque(this.renderer, false);
        SwingUtils.setOpaque(this.rendererButton, true);
    }

    @Override
    protected JComponent getInternalEditorComponent(final T value, final boolean isSelected, final int row, final int column) {
        this.clickedElement = value;
        clickedRow = row;
        return this.editor;
    }

    @Override
    protected JComponent getInternalRendererComponent(final T value, final boolean isSelected, final boolean hasFocus, final int row, final int column) {
        return this.renderer;
    }
}