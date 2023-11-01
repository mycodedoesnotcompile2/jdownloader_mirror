package org.appwork.storage;

import java.util.Date;

//if null checks are done for each field seperatly
@StorableValidateCondition(value = "{$or:[{minimumBuildDate:null},{maximumBuildDate:null},{maximumBuildDate:{$gte:§PARENT.minimumBuildDate}}]}", description = "minimumBuildDate must not be bigger than maximumBuildDate!")
public class BuildsInfo implements Storable {
    public static final String                                  KEY_MINIMUM_BUILD_DATE = "minimumBuildDate";
    public static final String                                  KEY_MAXIMUM_BUILD_DATE = "maximumBuildDate";
    public static final org.appwork.storage.TypeRef<BuildsInfo> TYPE                   = new org.appwork.storage.SimpleTypeRef<BuildsInfo>(BuildsInfo.class);

    public static enum TargetBuildIssue {
        /**
         * WARNING: Might be a problem - some target builds have issues - like a property that is only available in some target builds
         */
        SOME_TARGET_BUILDS_AFFECTED,
        /**
         * ERROR. All target builds have issues - e.g. NO build supports the used property
         */
        ALL_TARGET_BUILDS_AFFECTED,
        /**
         * Everything is fine - no target build version with issues
         */
        NO_TARGET_BUILDS_AFFECTED
    }

    public BuildsInfo() {
    }

    /**
     * @param date
     * @param time
     */
    public BuildsInfo(Date min, Date max) {
        this.minimumBuildDate = min;
        this.maximumBuildDate = max;
    }

    @StorableValidateNotNull
    @StorableValidateTimestampRelative(min = -50 * 365 * 24 * 60 * 60 * 1000l, max = +24 * 60 * 60 * 1000l, level = FailLevel.ERROR, message = "It can be found in the Connect Client about dialog or the projects.appwork.org release pages")
    @StorableDoc("Format yyyy-MM-ddTHH:mm[:ss.sss]Z. minimum and maximumBuildDate define a range of Connect Client Versions (by BuildDate) that must be compatible to this setup. If there are any properties in the setup that are deprecated, removed or not available yet, you will get errors or warnings.")
    @StorableExample("\"2021-10-07T00:00CET\"")
    private Date minimumBuildDate;

    public Date getMinimumBuildDate() {
        return minimumBuildDate;
    }

    public void setMinimumBuildDate(final Date minimumBuildDate) {
        this.minimumBuildDate = minimumBuildDate;
    }

    public Date getMaximumBuildDate() {
        return maximumBuildDate;
    }

    public void setMaximumBuildDate(final Date maximumBuildDate) {
        this.maximumBuildDate = maximumBuildDate;
    }

    @StorableValidateNotNull
    @StorableValidateTimestampRelative(min = -50 * 365 * 24 * 60 * 60 * 1000l, max = +24 * 60 * 60 * 1000l, level = FailLevel.ERROR, message = "It can be found in the Connect Client about dialog or the projects.appwork.org release pages")
    @StorableDoc("@See #minimumBuildDate")
    @StorableExample("\"2022-10-07T00:00CET\"")
    private Date maximumBuildDate;
}
