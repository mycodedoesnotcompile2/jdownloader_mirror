package org.jdownloader.gui.views.components.packagetable;

import org.jdownloader.gui.views.SelectionInfo;

import jd.controlling.packagecontroller.AbstractNode;
import jd.controlling.packagecontroller.AbstractPackageChildrenNode;
import jd.controlling.packagecontroller.AbstractPackageNode;

public interface PackageControllerTableSelectionModelInfo<PackageType extends AbstractPackageNode<ChildrenType, PackageType>, ChildrenType extends AbstractPackageChildrenNode<PackageType>> {
    public static enum EDGE {
        /* Somewhere in between, or the whole table fits into the viewport (no scrolling) */
        NONE,
        TOP,
        BOTTOM
    }

    public SelectionInfo<PackageType, ChildrenType> getSelectionInfo();

    public AbstractNode getSelectionModelLead();

    public AbstractNode getSelectionModelAnchor();

    /* Node of the first (partly) visible row, null reference if the table was empty or not inside a scroll pane */
    public AbstractNode getSelectionModelFirstVisible();

    public EDGE getSelectionModeltEdge();

    /* Pixels by which the first visible row was already scrolled out of the viewport at the top */
    public int getSelectionModelOffset();
}
