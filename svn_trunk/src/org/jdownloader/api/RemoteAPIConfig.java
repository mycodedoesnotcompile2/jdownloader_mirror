package org.jdownloader.api;

import java.util.ArrayList;

import org.appwork.storage.StorableValidatorIgnoresMissingSetter;
import org.appwork.storage.config.ConfigInterface;
import org.appwork.storage.config.annotations.AboutConfig;
import org.appwork.storage.config.annotations.DefaultBooleanValue;
import org.appwork.storage.config.annotations.DefaultIntValue;
import org.appwork.storage.config.annotations.DefaultStringValue;
import org.appwork.storage.config.annotations.DescriptionForConfigEntry;
import org.appwork.storage.config.annotations.RequiresRestart;

@StorableValidatorIgnoresMissingSetter
public interface RemoteAPIConfig extends ConfigInterface {
    @DefaultBooleanValue(true)
    @AboutConfig
    @RequiresRestart("A JDownloader Restart is Required")
    @DescriptionForConfigEntry("Enable or disable the ExternInterface API (Cnl2, Flashgot). When enabled, the interface listens on port 9666. SECURITY WARNING: Enabling this interface allows external websites to add download links to JDownloader. Only enable if needed and ensure proper authorization is configured via ExternInterfaceAuth. If disabled, external browser extensions cannot communicate with JDownloader.")
    boolean isExternInterfaceEnabled();

    void setExternInterfaceEnabled(boolean b);

    @DefaultBooleanValue(true)
    @AboutConfig
    @RequiresRestart("A JDownloader Restart is Required")
    @DescriptionForConfigEntry("Restrict ExternInterface (Cnl2, Flashgot) to localhost only. When enabled (default), the interface only accepts connections from localhost (127.0.0.1), preventing remote access. SECURITY WARNING: Disabling this allows remote network access to the ExternInterface API, which could expose JDownloader to unauthorized access from other devices on your network. Only disable if you specifically need remote access and understand the security implications.")
    boolean isExternInterfaceLocalhostOnly();

    void setExternInterfaceLocalhostOnly(boolean b);

    @AboutConfig
    @DescriptionForConfigEntry("List of authorized website domains that are allowed to use the ExternInterface API without user confirmation. Each entry should be a domain name (e.g., 'example.com'). When a website in this list sends requests to add download links, JDownloader will automatically allow them. SECURITY: Only add trusted websites to this list. Websites not in this list will trigger a user confirmation dialog before adding links. An empty list means all websites require user confirmation.")
    ArrayList<String> getExternInterfaceAuth();

    void setExternInterfaceAuth(ArrayList<String> auth);

    @DefaultBooleanValue(true)
    @AboutConfig
    @RequiresRestart("A JDownloader Restart is Required")
    @DescriptionForConfigEntry("Restrict the Deprecated API to localhost only. When enabled (default), the API only accepts connections from localhost (127.0.0.1), preventing remote access. SECURITY WARNING: Disabling this allows remote network access to the Deprecated API, which could expose JDownloader to unauthorized access from other devices on your network. The Deprecated API has known security limitations and should only be used locally. Only disable if you specifically need remote access and understand the security risks.")
    boolean isDeprecatedApiLocalhostOnly();

    void setDeprecatedApiLocalhostOnly(boolean b);

    @AboutConfig
    @RequiresRestart("A JDownloader Restart is Required")
    @DefaultBooleanValue(false)
    @DescriptionForConfigEntry("Enable or disable the Deprecated API server. When enabled, the API listens on the configured port (default: 3128). SECURITY WARNING: The Deprecated API has known security limitations and is not recommended for production use. It should only be enabled for localhost access and legacy compatibility. Enabling this API exposes JDownloader functionality over HTTP without modern security features. It is recommended to use the newer API implementations instead. Default is disabled for security reasons.")
    boolean isDeprecatedApiEnabled();

    void setDeprecatedApiEnabled(boolean b);

    @DefaultBooleanValue(true)
    public void setHeadlessMyJDownloaderMandatory(boolean b);

    public boolean isHeadlessMyJDownloaderMandatory();

    @AboutConfig
    @RequiresRestart("A JDownloader Restart is Required")
    @DefaultIntValue(3128)
    @DescriptionForConfigEntry("Port number for the Deprecated API server. Default is 3128. SECURITY: Ensure this port is not exposed to untrusted networks. The Deprecated API should only be accessible from localhost when DeprecatedApiLocalhostOnly is enabled. Changing this port requires a JDownloader restart.")
    int getDeprecatedApiPort();

    public void setDeprecatedApiPort(int port);

    @DefaultBooleanValue(false)
    @AboutConfig
    @DescriptionForConfigEntry("Enable or disable the JDAnywhere API. When enabled, this API provides remote access to JDownloader functionality. SECURITY: Only enable this API if you need remote access and have proper authentication configured. Ensure the API is properly secured and not exposed to untrusted networks. Default is disabled for security reasons.")
    boolean isJDAnywhereApiEnabled();

    void setJDAnywhereApiEnabled(boolean b);

    @DescriptionForConfigEntry("Configure CORS (Cross-Origin Resource Sharing) allowed origins for the Local API Server. This header controls which websites can make cross-origin requests to the API. Default (empty string) means no CORS origins are allowed, preventing cross-site scripting attacks. SECURITY WARNING: Setting this to '*' allows all origins, which is a significant security risk as it enables any website to access your API. Only set specific trusted origins (e.g., 'https://example.com') if you need cross-origin access. Leave empty for maximum security.")
    @DefaultStringValue("")
    /**
     * Default: do not allow CrossSiteSCripting
     *
     * @return
     */
    String getLocalAPIServerHeaderAccessControllAllowOrigin();

    @DescriptionForConfigEntry("Content Security Policy (CSP) header for the Local API Server. CSP helps prevent XSS attacks and controls which resources can be loaded. Default is 'default-src \\'self\\'' which only allows resources from the same origin. SECURITY: Modifying this policy can introduce security vulnerabilities. Only change if you understand CSP directives and have a specific requirement. See https://content-security-policy.com/ for CSP documentation. For REST APIs, the default 'default-src \\'self\\'' is recommended.")
    @DefaultStringValue("default-src 'self'")
    String getLocalAPIServerHeaderContentSecurityPolicy();

    @DescriptionForConfigEntry("X-Frame-Options header controls whether the API can be embedded in frames (iframes). Default is 'DENY' which prevents all framing, protecting against clickjacking attacks. SECURITY: Setting this to 'SAMEORIGIN' allows same-origin framing, and removing it entirely disables clickjacking protection. Only change if you have a specific requirement to allow framing. For REST APIs, 'DENY' is recommended to prevent clickjacking attacks.")
    @DefaultStringValue("DENY")
    String getLocalAPIServerHeaderXFrameOptions();

    @DescriptionForConfigEntry("Referrer-Policy header controls how much referrer information is sent with requests. Default is 'no-referrer' which sends no referrer information, maximizing privacy. SECURITY: This prevents leakage of sensitive information (API keys, tokens, session IDs) that might be in URLs. Other options include 'same-origin', 'strict-origin-when-cross-origin', etc. Only change if you need referrer information for analytics or other purposes. For REST APIs, 'no-referrer' is recommended for maximum privacy.")
    @DefaultStringValue("no-referrer")
    String getLocalAPIServerHeaderReferrerPolicy();
}
