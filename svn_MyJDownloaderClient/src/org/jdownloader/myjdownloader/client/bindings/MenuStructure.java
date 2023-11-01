package org.jdownloader.myjdownloader.client.bindings;

import java.util.ArrayList;

import org.jdownloader.myjdownloader.client.json.AbstractJsonData;

public class MenuStructure extends AbstractJsonData {
    public static enum Type {
        /** Container */
        CONTAINER,
        /** Action */
        ACTION,
        /** Link */
        LINK
    }

    public MenuStructure(/* Storable */) {
    }

    private String id;

    public String getId() {
        return this.id;
    }

    public void setId(final String id) {
        this.id = id;
    }

    private Type                     type;
    private String                   name;
    private String                   icon;
    private ArrayList<MenuStructure> children;

    public Type getType() {
        return this.type;
    }

    public void setType(final Type type) {
        this.type = type;
    }

    public String getName() {
        return this.name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public String getIcon() {
        return this.icon;
    }

    public void setIcon(final String icon) {
        this.icon = icon;
    }

    public ArrayList<MenuStructure> getChildren() {
        return this.children;
    }

    public void setChildren(final ArrayList<MenuStructure> children) {
        this.children = children;
    }

    public void add(final MenuStructure create) {
        if (this.children == null) {
            this.children = new ArrayList<MenuStructure>();
        }
        this.children.add(create);
    }

    public int size() {
        return this.children == null ? 0 : this.children.size();
    }

    public void remove(final MenuStructure submenu) {
        if (this.children == null) {
            return;
        }
        this.children.remove(submenu);
    }
}
