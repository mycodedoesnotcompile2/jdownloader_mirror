package jd.controlling.downloadcontroller;

import org.appwork.uio.In;
import org.appwork.uio.Out;
import org.appwork.utils.swing.dialog.OKCancelCloseUserIODefinition;

import jd.controlling.downloadcontroller.IfFilenameTooLongDialog.IfFilenameTooLongAction;

public interface IfFilenameTooLongDialogInterface extends OKCancelCloseUserIODefinition {
    @In
    public IfFilenameTooLongAction getAction();

    @Out
    public String getFilePath();

    @Out
    public String getPackagename();

    @Out
    public String getPackageID();

    @Out
    public String getHost();

    @Out
    public String getNewFilename();

    @In
    public boolean isRememberForPackageSelected();

    /** True when the chosen action should be remembered for the rest of the current session ("Don't ask again during this session"). */
    @In
    public boolean isDontAskAgainThisSessionSelected();
}
