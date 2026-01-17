/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact us.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: <e-mail@appwork.org>
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.net.httpserver;

import org.appwork.storage.StorableDoc;

/**
 * Enum representing possible values for the Sec-Fetch-Dest HTTP header.
 * 
 * The Sec-Fetch-Dest header indicates the destination of the request (what the fetched resource will be used for).
 * 
 * @author AppWork
 */
@StorableDoc("Enum representing possible values for the Sec-Fetch-Dest HTTP header. The Sec-Fetch-Dest header indicates the destination of the request (what the fetched resource will be used for).")
public enum SecFetchDest {
    /**
     * The request is for a document (HTML page).
     */
    @StorableDoc("The request is for a document (HTML page). Header value: \"document\"")
    DOCUMENT("document"),
    
    /**
     * The request is for an empty destination (e.g., fetch() with no specific destination).
     */
    @StorableDoc("The request is for an empty destination (e.g., fetch() with no specific destination). Header value: \"empty\"")
    EMPTY("empty"),
    
    /**
     * The request is for an audio resource.
     */
    @StorableDoc("The request is for an audio resource. Header value: \"audio\"")
    AUDIO("audio"),
    
    /**
     * The request is for an audio worklet.
     */
    @StorableDoc("The request is for an audio worklet. Header value: \"audioworklet\"")
    AUDIOWORKLET("audioworklet"),
    
    /**
     * The request is for an embedded resource (e.g., embed tag).
     */
    @StorableDoc("The request is for an embedded resource (e.g., embed tag). Header value: \"embed\"")
    EMBED("embed"),
    
    /**
     * The request is for a font resource.
     */
    @StorableDoc("The request is for a font resource. Header value: \"font\"")
    FONT("font"),
    
    /**
     * The request is for a frame.
     */
    @StorableDoc("The request is for a frame. Header value: \"frame\"")
    FRAME("frame"),
    
    /**
     * The request is for an iframe.
     */
    @StorableDoc("The request is for an iframe. Header value: \"iframe\"")
    IFRAME("iframe"),
    
    /**
     * The request is for an image resource.
     */
    @StorableDoc("The request is for an image resource. Header value: \"image\"")
    IMAGE("image"),
    
    /**
     * The request is for a web app manifest.
     */
    @StorableDoc("The request is for a web app manifest. Header value: \"manifest\"")
    MANIFEST("manifest"),
    
    /**
     * The request is for an object resource (e.g., object tag).
     */
    @StorableDoc("The request is for an object resource (e.g., object tag). Header value: \"object\"")
    OBJECT("object"),
    
    /**
     * The request is for a paint worklet.
     */
    @StorableDoc("The request is for a paint worklet. Header value: \"paintworklet\"")
    PAINTWORKLET("paintworklet"),
    
    /**
     * The request is for a report (e.g., CSP report).
     */
    @StorableDoc("The request is for a report (e.g., CSP report). Header value: \"report\"")
    REPORT("report"),
    
    /**
     * The request is for a script resource.
     */
    @StorableDoc("The request is for a script resource. Header value: \"script\"")
    SCRIPT("script"),
    
    /**
     * The request is for a service worker.
     */
    @StorableDoc("The request is for a service worker. Header value: \"serviceworker\"")
    SERVICEWORKER("serviceworker"),
    
    /**
     * The request is for a shared worker.
     */
    @StorableDoc("The request is for a shared worker. Header value: \"sharedworker\"")
    SHAREDWORKER("sharedworker"),
    
    /**
     * The request is for a stylesheet.
     */
    @StorableDoc("The request is for a stylesheet. Header value: \"style\"")
    STYLE("style"),
    
    /**
     * The request is for a text track (e.g., WebVTT).
     */
    @StorableDoc("The request is for a text track (e.g., WebVTT). Header value: \"track\"")
    TRACK("track"),
    
    /**
     * The request is for a video resource.
     */
    @StorableDoc("The request is for a video resource. Header value: \"video\"")
    VIDEO("video"),
    
    /**
     * The request is for a web worker.
     */
    @StorableDoc("The request is for a web worker. Header value: \"worker\"")
    WORKER("worker"),
    
    /**
     * The request is for an XSLT stylesheet.
     */
    @StorableDoc("The request is for an XSLT stylesheet. Header value: \"xslt\"")
    XSLT("xslt");

    private final String value;

    private SecFetchDest(final String value) {
        this.value = value;
    }

    /**
     * Returns the header value as string.
     * 
     * @return The header value (e.g., "document", "script", "image")
     */
    public String getValue() {
        return this.value;
    }

    /**
     * Converts a string to SecFetchDest (case-insensitive).
     * 
     * @param value
     *            The header value string
     * @return The SecFetchDest enum value, or null if not found
     */
    public static SecFetchDest fromString(final String value) {
        if (value == null) {
            return null;
        }
        for (final SecFetchDest dest : values()) {
            if (dest.value.equalsIgnoreCase(value)) {
                return dest;
            }
        }
        return null;
    }

    /**
     * Returns the header value as string.
     * 
     * @return The header value
     */
    @Override
    public String toString() {
        return this.value;
    }
}

