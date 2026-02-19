package org.jdownloader.extensions.antistandby;

import org.appwork.storage.config.JsonConfig;
import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.storage.config.handler.EnumSetKeyHandler;
import org.appwork.storage.config.handler.StorageHandler;
import org.appwork.utils.Application;

public class CFG_ANTISTANDBY {

    private static final AntiStandbyConfig                 CFG              = JsonConfig.create(Application.getResource("cfg/" + AntiStandbyExtension.class.getName()), AntiStandbyConfig.class);
    private static final StorageHandler<AntiStandbyConfig> SH               = (StorageHandler<AntiStandbyConfig>) CFG._getStorageHandler();

    public static final BooleanKeyHandler                  FRESH_INSTALL    = SH.getKeyHandler("FreshInstall", BooleanKeyHandler.class);

    public static final BooleanKeyHandler                  GUI_ENABLED      = SH.getKeyHandler("GuiEnabled", BooleanKeyHandler.class);

    public static final BooleanKeyHandler                  ENABLED          = SH.getKeyHandler("Enabled", BooleanKeyHandler.class);

    public static final EnumSetKeyHandler                  CONDITION        = SH.getKeyHandler("Condition", EnumSetKeyHandler.class);

    public static final BooleanKeyHandler                  DISPLAY_REQUIRED = SH.getKeyHandler("DisplayRequired", BooleanKeyHandler.class);
}