package org.jdownloader.gui.views.downloads.action;

import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.extensions.extraction.translate.T;
import org.jdownloader.translate._JDT;

public class ResetSettings {
    private DeleteMode            deleteMode            = DeleteMode.AUTO;
    private DisabledItemsBehavior disabledItemsBehavior = DisabledItemsBehavior.AUTO;

    public static enum DeleteMode implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto/Global default";
            }
        },
        MOVE_TO_TRASH {
            @Override
            public String getLabel() {
                return _JDT.T.DeleteOption_recycle();
            }
        },
        MOVE_TO_TRASH_WITH_FINAL_DELETE_AS_FALLBACK {
            @Override
            public String getLabel() {
                return "Move to trash if possible, if not delete permanently";
            }
        },
        DELETE {
            @Override
            public String getLabel() {
                return T.T.final_delete();
            }
        };
    }

    public static enum DisabledItemsBehavior implements LabelInterface {
        AUTO {
            @Override
            public String getLabel() {
                return "Auto/Global default";
            }
        },
        IGNORE {
            @Override
            public String getLabel() {
                return "Ignore disabled items";
            }
        },
        ENABLE_AND_RESET {
            @Override
            public String getLabel() {
                return "Enable & Reset disabled items";
            }
        };
    }

    public ResetSettings setDeleteMode(DeleteMode deleteMode) {
        this.deleteMode = deleteMode;
        return this;
    }

    public ResetSettings setDisabledItemsBehavior(DisabledItemsBehavior disabledItemsBehavior) {
        this.disabledItemsBehavior = disabledItemsBehavior;
        return this;
    }

    public DeleteMode getDeleteMode() {
        return deleteMode;
    }

    public DisabledItemsBehavior getDisabledItemsBehavior() {
        return disabledItemsBehavior;
    }
}