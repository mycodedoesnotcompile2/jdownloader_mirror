package org.appwork.storage;

import java.util.Date;
//if null checks are done for each field seperatly

@StorableDoc("The targetBuilds property in your configuration files specifies the version details of the application for which the configuration is intended. This property is critical for ensuring that all configurations match the required target versions of the application. When the configuration file is used, the system checks this property against the actual application versions. If any discrepancies are found, warnings are generated to alert the customer about potential incompatibilities. These warnings indicate which properties may not be supported by all or some of the targeted application versions.")
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
    @StorableExample("\"$NOW\"")
    private Date maximumBuildDate;
}
