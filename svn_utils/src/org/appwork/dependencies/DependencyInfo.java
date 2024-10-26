package org.appwork.dependencies;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.appwork.storage.Storable;
import org.appwork.storage.StorableDoc;
import org.appwork.utils.Files;
import org.appwork.utils.StringUtils;
import org.appwork.utils.ide.IDEUtils;

public class DependencyInfo {
    public static final org.appwork.storage.TypeRef<DependencyInfo> TYPE = new org.appwork.storage.TypeRef<DependencyInfo>(DependencyInfo.class) {
                                                                         };
    private List<Installed>                                         history;

    public List<Installed> getHistory() {
        return this.history;
    }

    public void setHistory(final List<Installed> history) {
        this.history = history;
    }

    @StorableDoc("Artefact path. Maven: groupID/artefactID")
    private String    artefact;
    @StorableDoc("Set to true of the Updater should ask before up/downgrading to a new version")
    private boolean   confirm = true;
    @StorableDoc("Internal - managed by the updater. Currently installed version")
    private Installed installed;

    public Installed getInstalled() {
        return this.installed;
    }

    public void setInstalled(final Installed installed) {
        this.installed = installed;
    }

    @StorableDoc("Set a minimum JRE that must be supported by the lib. The updater will check if the lib is compiled for this JRE")
    private String                    minJRE;
    @StorableDoc("maven|manual")
    private String                    provider = "maven";
    @StorableDoc("Desired Version - this may be a concrete version 1.0.0 or a placeholder from the meta.json files (like 'latest' or 'mvnLatest' or 'jre1.6Latest')")
    private String                    version  = null;
    private HashMap<String, String>   renames;
    private ArrayList<String>         filter;
    @StorableDoc("Allowed dependencies")
    private List<Map<String, Object>> dependencies;

    public List<Map<String, Object>> getDependencies() {
        return this.dependencies;
    }

    public void setDependencies(final List<Map<String, Object>> dependencies) {
        this.dependencies = dependencies;
    }

    public ArrayList<String> getFilter() {
        return this.filter;
    }

    public void setFilter(final ArrayList<String> filter) {
        this.filter = filter;
    }

    public HashMap<String, String> getRenames() {
        return this.renames;
    }

    public void setRenames(final HashMap<String, String> renames) {
        this.renames = renames;
    }

    public DependencyInfo() {
        // TODO Auto-generated constructor stub
    }

    public String getArtefact() {
        return this.artefact;
    }

    public String getMinJRE() {
        return this.minJRE;
    }

    public String getProvider() {
        return this.provider;
    }

    public String getVersion() {
        return this.version;
    }

    public boolean isConfirm() {
        return this.confirm;
    }

    public void setArtefact(final String artefact) {
        this.artefact = artefact;
    }

    public void setConfirm(final boolean confirm) {
        this.confirm = confirm;
    }

    public void setMinJRE(final String minJRE) {
        this.minJRE = minJRE;
    }

    public void setProvider(final String provider) {
        this.provider = provider;
    }

    public void setVersion(final String version) {
        this.version = version;
    }

    public Version _getInstalledVersion() {
        if (this.installed == null) {
            return null;
        }
        if (this.installed.getVersion() == null) {
            return null;
        }
        return new Version(this.installed.getVersion());
    }

    public static class RequiredBy implements Storable {
        public RequiredBy() {
        }

        private String provider;

        public String getProvider() {
            return this.provider;
        }

        public void setProvider(final String provider) {
            this.provider = provider;
        }

        public String getArtefact() {
            return this.artefact;
        }

        public void setArtefact(final String artefact) {
            this.artefact = artefact;
        }

        public String getFile() {
            return this.file;
        }

        @Override
        public int hashCode() {
            return this.file.hashCode() + this.artefact.hashCode() + this.provider.hashCode();
        }

        @Override
        public boolean equals(final Object obj) {
            if (obj instanceof RequiredBy) {
                final RequiredBy other = (RequiredBy) obj;
                return StringUtils.equals(this.version, other.version) && StringUtils.equals(this.file, other.file) && StringUtils.equals(this.provider, other.provider) && StringUtils.equals(this.artefact, other.artefact);
            } else {
                return false;
            }
        }

        public void setFile(final String file) {
            this.file = file;
        }

        private String artefact;
        private String file;
        private String version;

        public String getVersion() {
            return this.version;
        }

        public void setVersion(final String version) {
            this.version = version;
        }
    }

    private HashSet<RequiredBy> requiredBy = new HashSet<RequiredBy>();

    public void setRequiredBy(final HashSet<RequiredBy> requiredBy) {
        this.requiredBy = requiredBy;
    }

    public HashSet<RequiredBy> getRequiredBy() {
        return this.requiredBy;
    }

    public boolean requiredBy(final File file, final String artefact, final String provider, final String version) {
        final RequiredBy req = new RequiredBy();
        req.setArtefact(artefact);
        req.setFile(Files.getRelativePath(IDEUtils.getWorkSpace(), file));
        req.setProvider(provider);
        req.setVersion(version);
        return this.requiredBy.add(req);
    }

    private boolean autoRenameEnabled = true;

    public boolean isAutoRenameEnabled() {
        return this.autoRenameEnabled;
    }

    public void setAutoRenameEnabled(final boolean autoRenameEnabled) {
        this.autoRenameEnabled = autoRenameEnabled;
    }
}
