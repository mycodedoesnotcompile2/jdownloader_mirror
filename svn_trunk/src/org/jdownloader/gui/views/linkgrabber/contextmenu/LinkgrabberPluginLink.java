package org.jdownloader.gui.views.linkgrabber.contextmenu;

import java.awt.Component;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JPopupMenu;

import jd.controlling.linkcrawler.CrawledLink;
import jd.controlling.linkcrawler.CrawledPackage;

import org.appwork.utils.swing.EDTHelper;
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
import org.jdownloader.gui.views.linkgrabber.LinkGrabberTable;

public class LinkgrabberPluginLink extends MenuItemData implements MenuLink {
    @Override
    public String getName() {
        return _GUI.T.LinkgrabberPluginLink_getName_object_();
    }

    @Override
    public String getIconKey() {
        return IconKey.ICON_PLUGIN;
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
    public JComponent addTo(final JComponent root, MenuBuilder menuBuilder) throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, ClassNotFoundException, NoSuchMethodException, SecurityException, ExtensionNotLoadedException {
        final int rootIndex = root.getComponentCount();
        LinkGrabberTable.getInstance().getSelectionInfo(new EDTSelectionInfoCallback<CrawledPackage, CrawledLink>() {

            @Override
            public void onSelectionInfo(SelectionInfo<CrawledPackage, CrawledLink> selectionInfo) {
                final Collection<PluginView<CrawledLink>> views = selectionInfo.getPluginViews();
                final JPopupMenu container = new JPopupMenu();
                for (PluginView<CrawledLink> pv : views) {
                    pv.getPlugin().extendLinkgrabberContextMenu(container, pv, views);
                }
                if (container.getComponentCount() > 0) {
                    int index = rootIndex;
                    for (final Component comp : container.getComponents()) {
                        root.add(comp, index++);
                    }
                    root.revalidate();
                }
            }

            @Override
            public boolean isCancelled() {
                return Boolean.FALSE.equals(new EDTHelper<Boolean>() {

                    @Override
                    public Boolean edtRun() {
                        return root.isVisible();
                    }
                }.getReturnValue());
            }
        }, SelectionType.SELECTED);
        return null;
    }
}
