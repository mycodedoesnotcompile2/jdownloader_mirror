package org.jdownloader.controlling.contextmenu.gui;

import java.awt.Component;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FilenameFilter;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPopupMenu;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.appwork.resources.ThemeContext;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.ExtButton;
import org.appwork.swing.components.ExtTextField;
import org.appwork.utils.swing.dialog.AbstractDialog;
import org.appwork.utils.swing.dialog.Dialog;
import org.jdownloader.actions.AppAction;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.images.NewTheme;

public class NewSubMenuDialog extends AbstractDialog<Object> {
    private ExtTextField txt;
    protected Icon       actionIcon;
    private ExtButton    iconField;
    protected String     iconUrl = "help";

    public NewSubMenuDialog() {
        super(Dialog.STYLE_HIDE_ICON, _GUI.T.NewSubMenuDialog_NewSubMenuDialog_title(), null, null, null);
    }

    @Override
    protected Object createReturnValue() {
        return null;
    }

    @Override
    public JComponent layoutDialogContent() {
        MigPanel p = new MigPanel("ins 0", "[][grow,fill]", "[24!,fill]");
        txt = new ExtTextField();
        txt.setHelpText(_GUI.T.NewSubMenuDialog_layoutDialogContent_name_());
        iconField = new ExtButton(new AppAction() {
            {
                setSmallIcon(NewTheme.I().getIcon(iconUrl, 20));
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                final JPopupMenu p = new JPopupMenu();
                File imagesDir;
                imagesDir = NewTheme.I().getImagesDirectory();
                String[] names = imagesDir.list(new FilenameFilter() {
                    public boolean accept(File dir, String name) {
                        if (name.endsWith(".png") || name.endsWith(".svg")) {
                            Image image = NewTheme.I().getImage(name.substring(0, name.length() - 4), -1, -1, new ThemeContext().withoutCache());
                            if (Math.max(image.getWidth(null), image.getHeight(null)) >= 32) {
                                return true;
                            }
                        }
                        return false;
                    }
                });
                final JList list = new JList(names);
                list.setLayoutOrientation(JList.HORIZONTAL_WRAP);
                final ListCellRenderer org = list.getCellRenderer();
                list.setCellRenderer(new ListCellRenderer() {
                    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
                        String key = value.toString().substring(0, value.toString().length() - 4);
                        JLabel ret = (JLabel) org.getListCellRendererComponent(list, "", index, isSelected, cellHasFocus);
                        ret.setIcon(NewTheme.I().getIcon(key, 20));
                        return ret;
                    }
                });
                list.setFixedCellHeight(22);
                list.setFixedCellWidth(22);
                list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                list.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
                    public void valueChanged(ListSelectionEvent e) {
                        String v = list.getSelectedValue().toString();
                        v = v.substring(0, v.length() - 4);
                        iconUrl = v;
                        setSmallIcon(NewTheme.I().getIcon(iconUrl, 20));
                        iconField.setIcon(NewTheme.I().getIcon(iconUrl, 20));
                        p.setVisible(false);
                    }
                });
                p.add(list);
                p.show(iconField, 0, iconField.getHeight());
            }
        });
        p.add(iconField, "width 24!,height 24!");
        p.add(txt);
        return p;
    }

    public String getName() {
        return txt.getText();
    }

    public String getIconKey() {
        return iconUrl;
    }
}
