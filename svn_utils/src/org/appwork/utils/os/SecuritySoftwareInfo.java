/**
 * 
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
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
package org.appwork.utils.os;

import java.util.HashMap;

/**
 * @author Thomas
 *
 */
public class SecuritySoftwareInfo extends HashMap<String, String> {

    public String getName() {

        return get("displayName");
    }

    public String getState() {
        return get("productState");
    }

    /**
     * http://neophob.com/2010/03/wmi-query-windows-securitycenter2/
     *
     * @return
     */
    public boolean isEnabled() {
        if ("TRUE".equals(get("enabled"))) {

            // XP Firewall only
            return true;
        }
        if (get("productState") == null) {
            // XP
            return true;
        }
        String state = getState();
        if (state == null) {
            return false;
        }
        int i = Integer.parseInt(state);
        return isEnabledByState(i);
    }

    public static boolean isEnabledByState(int i) {
        // String hex = StringUtils.fillPre(Integer.toHexString(i), "0", 6);
        // int upToDate = 0xFF & i;
        int enabledFlag = 0xFF & i >> 8;
        // int type = 0xFF & i >> 16;
        // System.out.println(Integer.toHexString(enabled) + " " + Integer.toHexString(enabledFlag) + " " + Integer.toHexString(c));
        // if ("AVG AntiVirus Free Edition 2015".equals(getName())) {
        // System.out.println(getState() + "\t-> " + hex + "\t" + (enabledFlag >= 16) + " " + Regex.getLines(get("response")).length);
        // }
        return enabledFlag >= 16;
    }

    /*
     * (non-Javadoc)
     *
     * @see java.util.AbstractMap#toString()
     */
    @Override
    public String toString() {
        // TODO Auto-generated method stub
        return getName() + " Enabled: " + isEnabled() + " Up2Date: " + isUp2Date();
    }

    public boolean isUp2Date() {
        String up2DateXp = get("productUptoDate");
        if ("TRUE".equalsIgnoreCase(up2DateXp)) {
            return true;
        }
        String state = getState();
        if (state == null) {
            return false;
        }
        int i = Integer.parseInt(state);
        // String hex = StringUtils.fillPre(Integer.toHexString(i), "0", 6);
        int upToDate = 0xFF & i;
        // int enabledFlag = 0xFF & i >> 8;
        // int type = 0xFF & i >> 16;
        // System.out.println(Integer.toHexString(enabled) + " " + Integer.toHexString(enabledFlag) + " " + Integer.toHexString(c));
        return upToDate == 0;
    }
}
