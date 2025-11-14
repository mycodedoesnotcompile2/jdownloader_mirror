package org.jdownloader.gui.views.downloads.columns;

import java.util.Locale;

import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;
import jd.controlling.packagecontroller.PackageController;

import org.appwork.swing.exttable.columns.ExtTextColumn;
import org.appwork.utils.StringUtils;
import org.jdownloader.gui.translate._GUI;

/**
 * Class giving the implementation details of the enabled / disabled column type
 *
 * @author pp_me
 *
 */
public class IndexColumn extends ExtTextColumn<AbstractNode> {

    private final PackageController controller;
    private final Locale            locale;

    public IndexColumn(PackageController controller) {
        super(_GUI.T.IndexColumn_IndexColumn());
        this.controller = controller;
        locale = Locale.getDefault();
    }

    @Override
    public boolean isDefaultVisible() {
        return false;
    }

    @Override
    protected boolean isEditable(final AbstractNode obj, final boolean enabled) {
        return false;
    }

    @Override
    public int getMaxWidth() {
        return 100;
    }

    @Override
    public int getMinWidth() {
        return 12;
    }

    @Override
    public int getDefaultWidth() {
        return 22;
    }

    @Override
    public boolean isEditable(AbstractNode obj) {
        return false;
    }

    @Override
    public boolean isEnabled(final AbstractNode obj) {
        return true;
    }

    @Override
    public boolean isSortable(AbstractNode obj) {
        return false;
    }

    @Override
    public String getStringValue(AbstractNode value) {
        if (value instanceof AbstractPackageNode) {
            final int index = controller.indexOf((AbstractPackageNode) value);
            if (index == -1) {
                return "?";
            }
            return StringUtils.formatByPadLength(locale, StringUtils.getPadLength(Math.max(index + 1, controller.size())), index + 1);
        } else {
            final AbstractPackageChildrenNode child = (AbstractPackageChildrenNode) value;
            final AbstractPackageNode parent = (AbstractPackageNode) child.getParentNode();
            final int pkgIndex = parent == null ? -1 : controller.indexOf(parent);
            final int childIndex = parent == null ? -1 : parent.indexOf(child);
            if (pkgIndex != -1 && childIndex != -1) {
                return StringUtils.formatByPadLength(locale, StringUtils.getPadLength(Math.max(pkgIndex + 1, controller.size())), pkgIndex + 1) + "-" + StringUtils.formatByPadLength(locale, StringUtils.getPadLength(Math.max(childIndex + 1, parent.size())), childIndex + 1);
            } else if (childIndex != -1) {
                return "?-" + StringUtils.formatByPadLength(locale, StringUtils.getPadLength(Math.max(childIndex + 1, controller.size())), childIndex + 1);
            }
            return "?";
        }
    }

}