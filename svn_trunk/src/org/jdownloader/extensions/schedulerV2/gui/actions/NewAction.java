package org.jdownloader.extensions.schedulerV2.gui.actions;

import java.awt.event.ActionEvent;

import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogNoAnswerException;
import org.jdownloader.extensions.schedulerV2.gui.AddScheduleEntryDialog;
import org.jdownloader.extensions.schedulerV2.gui.SchedulerTable;
import org.jdownloader.extensions.schedulerV2.model.ScheduleEntry;
import org.jdownloader.gui.views.components.AbstractAddAction;

public class NewAction extends AbstractAddAction {
    /**
     * 
     */
    private static final long    serialVersionUID = 1L;
    private final SchedulerTable table;

    public NewAction(SchedulerTable table) {
        super();
        this.table = table;
    }

    public void actionPerformed(ActionEvent e) {
        final AddScheduleEntryDialog dialog = new AddScheduleEntryDialog();

        try {
            ScheduleEntry entry = Dialog.getInstance().showDialog(dialog);
            if (entry != null) {
                table.getExtension().addScheduleEntry(entry);
            }
        } catch (DialogNoAnswerException e2) {
            e2.printStackTrace();
        }
    }

}
