//    jDownloader - Downloadmanager
//    Copyright (C) 2009  JD-Team support@jdownloader.org
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.

package jd.captcha;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Locale;
import java.util.concurrent.ConcurrentHashMap;

import jd.nutils.io.JDIO;
import jd.utils.JDUtilities;

import org.appwork.storage.config.JsonConfig;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.captcha.v2.solver.solver9kw.Captcha9kwSettings;
import org.jdownloader.logging.LogController;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class JACMethod implements Comparable<JACMethod> {

    private static java.util.List<JACMethod> methods = null;

    public static JACMethod forServiceName(String service) {
        for (JACMethod method : getMethods()) {
            if (service.equalsIgnoreCase(method.getServiceName())) {
                if (JsonConfig.create(Captcha9kwSettings.class).isEnabled() && method.getFileName().equalsIgnoreCase("captcha9kw")) {
                    if (JsonConfig.create(Captcha9kwSettings.class).isEnabled() && method.getFileName().equalsIgnoreCase("captcha9kw")) {
                        jd.gui.UserIO.getInstance().requestMessageDialog("9kw old method captcha9kw ", "Please delete old method captcha9kw and use jd2 with 9kw.\n");
                    }
                    LogController.CL().info("Inactive JAC method for the service " + service + " in directory " + method.getFileName());
                } else {
                    LogController.CL().info("Found JAC method for the service " + service + " in directory " + method.getFileName());
                    return method;
                }
            }
        }
        LogController.CL().info("There is no JAC method for the service " + service + "!");
        return null;
    }

    public static synchronized java.util.List<JACMethod> getMethods() {
        if (methods != null) {
            return methods;
        }
        java.util.List<JACMethod> methods = new ArrayList<JACMethod>();
        for (File methodDir : getMethodDirs()) {
            java.util.List<JACMethod> meths = parseJACInfo(methodDir);
            if (meths != null) {
                methods.addAll(meths);
            }
        }
        Collections.sort(methods);
        JACMethod.methods = methods;
        return methods;
    }

    private static File[] getMethodDirs() {
        File dir = JDUtilities.getResourceFile(JDUtilities.getJACMethodsDirectory());

        if (dir == null || !dir.exists()) {
            LogController.CL().severe("Konnte Methodenordner nicht finden: " + dir);
            return new File[0];
        }

        File[] entries = dir.listFiles(new FileFilter() {
            public boolean accept(File pathname) {
                if (!pathname.isDirectory()) {
                    return false;
                }
                File method = new File(pathname.getAbsoluteFile() + "/jacinfo.xml");
                return (method.exists() && isAvailableExternMethod(method));
            }
        });
        return entries;
    }

    public static java.util.List<JACMethod> parseJACInfo(File dir) {
        String filecontent = JDIO.readFileToString(new File(dir.getAbsolutePath() + "/jacinfo.xml"));
        Document doc = JDUtilities.parseXmlString(filecontent);
        if (doc == null) {
            return null;
        }

        String[] services = null;

        NodeList nl = doc.getFirstChild().getChildNodes();
        for (int i = 0; i < nl.getLength(); i++) {
            Node childNode = nl.item(i);

            if (childNode.getNodeName().equals("method")) {
                String servicelist = JDUtilities.getAttribute(childNode, "services");
                if (servicelist != null) {
                    services = servicelist.split(";");
                } else {
                    services = new String[] { JDUtilities.getAttribute(childNode, "name") };
                }

                break;
            }
        }

        java.util.List<JACMethod> methods = new ArrayList<JACMethod>();
        if (services != null) {
            for (String service : services) {
                LogController.CL().info("Method " + dir.getName() + " supports " + service);
                methods.add(new JACMethod(dir.getName(), service));
            }
        }
        return methods;
    }

    /**
     * Cache for {@link #hasMethod(String)}: lowercase service name -> result. Without it every call would read the jacinfo.xml from disk
     * again (and log). The method list itself ({@link #getMethods()}) is cached until the next restart as well, so this cache has the same
     * lifetime.
     */
    private static final ConcurrentHashMap<String, Boolean> HAS_METHOD_CACHE = new ConcurrentHashMap<String, Boolean>();

    public static boolean hasMethod(final String service) {
        if (service == null) {
            return false;
        }
        final String cacheKey = service.toLowerCase(Locale.ENGLISH);
        final Boolean cached = HAS_METHOD_CACHE.get(cacheKey);
        if (cached != null) {
            return cached.booleanValue();
        }
        final boolean hasMethod = checkHasMethod(service);
        HAS_METHOD_CACHE.put(cacheKey, Boolean.valueOf(hasMethod));
        return hasMethod;
    }

    /** Uncached check (disk access): does a usable JAC method exist for the given service? */
    private static boolean checkHasMethod(final String service) {
        JACMethod methodName = forServiceName(service);
        if (methodName == null) {
            return false;
        }
        File method = JDUtilities.getResourceFile(JDUtilities.getJACMethodsDirectory() + "/" + methodName.getFileName() + "/jacinfo.xml");
        return (method.exists() && isAvailableExternMethod(method));
    }

    private static boolean isAvailableExternMethod(File jacinfo) {
        return isAvailableExternMethod(JDIO.readFileToString(jacinfo));
    }

    private static boolean isAvailableExternMethod(String content) {
        if (content != null && content.contains("extern")) {
            if (CrossSystem.isLinux() && !content.contains("linux")) {
                return false;
            }
            if (CrossSystem.isMac() && !content.contains("mac")) {
                return false;
            }
            if (CrossSystem.isWindows() && !content.contains("windows")) {
                return false;
            }
        }
        return true;
    }

    private String filename;

    private String servicename;

    public JACMethod(String filename, String servicename) {
        this.filename = filename;
        if (servicename != null) {
            this.servicename = servicename.toLowerCase();
        }
    }

    public String getFileName() {
        return filename;
    }

    public String getServiceName() {
        return servicename;
    }

    public int compareTo(JACMethod o) {
        return (servicename + " " + filename).compareToIgnoreCase(o.getServiceName() + " " + o.getFileName());
    }

}
