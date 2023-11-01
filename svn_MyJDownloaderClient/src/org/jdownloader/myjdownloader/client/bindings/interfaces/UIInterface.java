package org.jdownloader.myjdownloader.client.bindings.interfaces;

import org.jdownloader.myjdownloader.client.bindings.ApiDoc;
import org.jdownloader.myjdownloader.client.bindings.ClientApiNameSpace;
import org.jdownloader.myjdownloader.client.bindings.MenuStructure;
import org.jdownloader.myjdownloader.client.exceptions.device.InternalServerErrorException;

@ClientApiNameSpace("ui")
public interface UIInterface extends Linkable {
    public static enum Context {
        /** Linkgrabber rightlick */
        @ApiDoc("Linkgrabber Rightclick Context") LGC,
        /** Downloadlist rightlick */
        @ApiDoc("Downloadlist Rightclick Context") DLC
    }

    public MenuStructure getMenu(Context context) throws InternalServerErrorException;

    public Object invokeAction(Context context, String id, long[] linkIds, long[] packageIds) throws InternalServerErrorException;
}
