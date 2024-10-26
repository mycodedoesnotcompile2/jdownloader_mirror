package org.appwork.dependencies;

import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.appwork.remoteapi.annotations.AllowNonStorableObjects;
import org.appwork.storage.SimpleTypeRef;
import org.appwork.storage.Storable;

public class Installed implements Storable {
    public static final SimpleTypeRef<Installed> TYPE = new SimpleTypeRef<Installed>(Installed.class);

    public Installed() {
        // TODO Auto-generated constructor stub
    }

    private String changes;

    public String getChanges() {
        return this.changes;
    }

    public void setChanges(final String changes) {
        this.changes = changes;
    }

    @AllowNonStorableObjects
    private Object licenses;

    public Object getLicenses() {
        return this.licenses;
    }

    public void setLicenses(final Object licenses) {
        this.licenses = licenses;
    }

    public Object getDependencies() {
        return this.dependencies;
    }

    public void setDependencies(final Object dependencies) {
        this.dependencies = dependencies;
    }

    @AllowNonStorableObjects
    private Object                  dependencies;
    private HashMap<String, String> hashes;
    private List<String>            files;

    public List<String> getFiles() {
        return this.files;
    }

    public void setFiles(final List<String> files) {
        this.files = files;
    }

    public HashMap<String, String> getHashes() {
        return this.hashes;
    }

    public void setHashes(final HashMap<String, String> hashes) {
        this.hashes = hashes;
    }

    public String getVersion() {
        return this.version;
    }

    public void setVersion(final String version) {
        this.version = version;
    }

    public Date getDate() {
        return this.date;
    }

    public void setDate(final Date date) {
        this.date = date;
    }

    public String getMinJRE() {
        return this.minJRE;
    }

    public void setMinJRE(final String minJRE) {
        this.minJRE = minJRE;
    }

    private String version;
    private Date   date;
    private String minJRE;
    private String description;

    public String getDescription() {
        return this.description;
    }

    public void setDescription(final String description) {
        this.description = description;
    }

    public String getName() {
        return this.name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    private String name;
}
