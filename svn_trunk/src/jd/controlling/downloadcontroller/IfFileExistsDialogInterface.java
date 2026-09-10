package jd.controlling.downloadcontroller;

import org.appwork.uio.In;
import org.appwork.uio.Out;
import org.appwork.utils.swing.dialog.OKCancelCloseUserIODefinition;
import org.jdownloader.settings.GeneralSettings.OnSkipDueToAlreadyExistsAction;
import org.jdownloader.settings.IfFileExistsAction;

public interface IfFileExistsDialogInterface extends OKCancelCloseUserIODefinition {
    @In
    public IfFileExistsAction getAction();

    @Out
    public String getFilePath();

    @Out
    public String getPackagename();

    @Out
    public String getPackageID();

    @Out
    public String getHost();

    /** The new filename the user chose for the rename case (auto suggestion or custom); null when no rename was selected. */
    @Out
    public String getNewFilename();

    /** True when the chosen action should be remembered for all remaining items of the package (per-package "remember" checkbox). */
    @In
    public boolean isRememberForPackageSelected();

    /** True when the chosen action should be remembered for the rest of the current session ("Don't ask again during this session"). */
    @In
    public boolean isDontAskAgainThisSessionSelected();

    /**
     * The "on skip due to already exists" action chosen in the dialog (dropdown next to the skip option); null when the user did not choose
     * the skip option, in which case the caller falls back to the global config value.
     */
    @In
    public OnSkipDueToAlreadyExistsAction getOnSkipDueToAlreadyExistsAction();

}
