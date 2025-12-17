package jd.plugins;

import org.jdownloader.translate._JDT;

public class DecrypterRetryException extends Exception {
    public static enum RetryReason {
        AGE_VERIFICATION_REQUIRED("Age verification required", "To access this content, age verification is required. In some cases it helps to use a VPN. Otherwise adding a verified account may help."),
        CAPTCHA(_JDT.T.decrypter_wrongcaptcha(), null),
        EMPTY_FOLDER(_JDT.T.decrypter_empty_folder(), _JDT.T.decrypter_empty_folder_description()),
        EMPTY_PROFILE(_JDT.T.decrypter_empty_profile(), null),
        EMPTY_SEARCH_QUERY(_JDT.FIX_ME("Search query without search results"), _JDT.FIX_ME("The search query you've entered did not lead to any search results.")),
        NO_ACCOUNT(_JDT.T.decrypter_invalidaccount(), null),
        FILE_NOT_FOUND(_JDT.T.decrypter_contentoffline(), null),
        PLUGIN_DEFECT(_JDT.T.decrypter_plugindefect(), null),
        PLUGIN_SETTINGS(_JDT.T.decrypter_pluginsettings(), _JDT.T.decrypter_pluginsettings_description()),
        PASSWORD(_JDT.T.decrypter_wrongpassword(), null),
        HOST(_JDT.T.plugins_errors_hosterproblem(), null),
        HOST_RATE_LIMIT(_JDT.T.plugins_errors_hosterproblem_rate_limit(), null),
        GEO(_JDT.T.decrypter_unavailable_geo(), _JDT.T.decrypter_unavailable_geo_description()),
        IP(_JDT.T.decrypter_unavailable_ip(), _JDT.T.decrypter_unavailable_ip_description()),
        UNSUPPORTED_LIVESTREAM(_JDT.T.decrypter_unavailable_unsupported_livestream(), _JDT.T.decrypter_unavailable_unsupported_livestream()),
        UNSUPPORTED_DRM("DRM", "Unsupported DRM protected content"),
        BLOCKED_BY(_JDT.T.decrypter_unavailable_blocked_by(), null);

        private final String exp;
        private final String longExp;

        private RetryReason(String exp, String longExp) {
            this.exp = exp;
            this.longExp = longExp;
        }

        public String getExplanation(Object requestor) {
            return exp;
        }

        public String getLongExplanation() {
            if (longExp != null) {
                return longExp;
            } else {
                return exp;
            }
        }
    }

    protected final RetryReason reason;

    public RetryReason getReason() {
        return reason;
    }

    public String getCustomName() {
        return customName;
    }

    public String getComment() {
        if (customComment != null) {
            return customComment;
        }
        return reason.getLongExplanation();
    }

    public String getCustomComment() {
        return customComment;
    }

    protected final String customName;
    protected final String customComment;

    public DecrypterRetryException(RetryReason reason) {
        this(reason, null, null);
    }

    public DecrypterRetryException(RetryReason reason, final String customName) {
        this(reason, customName, null);
    }

    public DecrypterRetryException(final RetryReason reason, final String customName, final String customComment) {
        this.reason = reason;
        this.customName = customName;
        this.customComment = customComment;
    }

    public DecrypterRetryException(final RetryReason reason, final String customName, final String customComment, Throwable cause) {
        super(cause);
        this.reason = reason;
        this.customName = customName;
        this.customComment = customComment;
    }
}