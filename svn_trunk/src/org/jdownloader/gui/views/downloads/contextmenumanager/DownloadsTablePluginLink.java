package org.jdownloader.gui.views.downloads.contextmenumanager;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JComponent;

import jd.plugins.DownloadLink;
import jd.plugins.FilePackage;

import org.jdownloader.actions.AppAction;
import org.jdownloader.controlling.contextmenu.MenuItemData;
import org.jdownloader.controlling.contextmenu.MenuLink;
import org.jdownloader.controlling.contextmenu.gui.MenuBuilder;
import org.jdownloader.extensions.ExtensionNotLoadedException;
import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.translate._GUI;
import org.jdownloader.gui.views.SelectionInfo;
import org.jdownloader.gui.views.SelectionInfo.PluginView;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.EDTSelectionInfoCallback;
import org.jdownloader.gui.views.components.packagetable.PackageControllerTable.SelectionType;
import org.jdownloader.gui.views.downloads.table.DownloadsTable;

public class DownloadsTablePluginLink extends MenuItemData implements MenuLink {
    @Override
    public String getName() {
        return _GUI.T.DownloadsTablePluginLink_getName_object_();
    }

    @Override
    public List<AppAction> createActionsToLink() {
        return null;
    }

    @Override
    public JComponent createSettingsPanel() {
        return null;
    }

    @Override
    public String getIconKey() {
        return IconKey.ICON_PLUGIN;
    }

    @Override
    public JComponent addTo(final JComponent root, final MenuBuilder menuBuilder) throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, SecurityException, ExtensionNotLoadedException {
        final int rootIndex = root.getComponentCount();
        DownloadsTable.getInstance().getSelectionInfo(new EDTSelectionInfoCallback<FilePackage, DownloadLink>() {

            @Override
            public void onSelectionInfo(SelectionInfo<FilePackage, DownloadLink> selectionInfo) {
                final Collection<PluginView<DownloadLink>> views = selectionInfo.getPluginViews();
                final List<JComponent> menuEntries = new ArrayList<JComponent>();
                for (PluginView<DownloadLink> pv : views) {
                    final List<JComponent> pluginMenuEntries = pv.getPlugin().extendDownloadsTableContextMenu(menuBuilder.getCancelled(), root, pv, views);
                    if (pluginMenuEntries != null) {
                        menuEntries.addAll(pluginMenuEntries);
                    }
                }
                if (menuEntries.size() > 0) {
                    int index = rootIndex;
                    for (final JComponent comp : menuEntries) {
                        root.add(comp, index++);
                    }
                    root.revalidate();
                }
            }

            @Override
            public boolean isCancelled() {
                return menuBuilder.getCancelled().get();
            }
        }, SelectionType.SELECTED);
        return null;
    }
}
