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
package org.appwork.utils.event.ide;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import org.appwork.utils.IO;
import org.appwork.utils.swing.dialog.Dialog;
import org.appwork.utils.swing.dialog.DialogCanceledException;
import org.appwork.utils.swing.dialog.DialogClosedException;
import org.appwork.utils.swing.dialog.ExtFileChooserDialog;
import org.appwork.utils.swing.dialog.FileChooserSelectionMode;
import org.appwork.utils.swing.dialog.FileChooserType;

/**
 * @author daniel
 *
 */
public class EventSenderIDEFactory {
    /**
     * @param name
     * @param file
     * @throws IOException
     */
    private static void create(final String name, final File file) throws IOException {
        String pkg = "";
        System.out.println("");
        File p = file;
        do {
            if (pkg.length() > 0) {
                pkg = "." + pkg;
            }
            pkg = p.getName() + pkg;

        } while ((p = p.getParentFile()) != null && !p.getName().equals("src"));

        StringBuilder sb = new StringBuilder();
        final String senderName = name + "EventSender";
        final String eventName = name + "Event";
        final String listenerName = name + "Listener";

        sb.append("package " + pkg + ";\r\n\r\n");
        sb.append("import org.appwork.utils.event.Eventsender;\r\n\r\n");
        sb.append("public class " + senderName + " extends Eventsender<" + listenerName + ", " + eventName + "> {\r\n\r\n");
        sb.append("@Override\r\n");
        sb.append("protected void fireEvent(" + listenerName + " listener, " + eventName + " event) {\r\nswitch (event.getType()) {\r\n//fill\r\ndefault: System.out.println(\"Unhandled Event: \"+event); \r\n}\r\n}");
        sb.append("}");
        new File(file, senderName + ".java").delete();
        IO.writeStringToFile(new File(file, senderName + ".java"), sb.toString());
        sb = new StringBuilder();

        sb.append("package " + pkg + ";\r\n\r\n");
        sb.append("import java.util.EventListener;\r\n\r\n");
        sb.append("public interface " + listenerName + " extends EventListener {\r\n\r\n}");
        new File(file, listenerName + ".java").delete();
        IO.writeStringToFile(new File(file, listenerName + ".java"), sb.toString());

        sb = new StringBuilder();
        sb.append("package " + pkg + ";\r\n\r\n");
        sb.append("import org.appwork.utils.event.SimpleEvent;\r\n\r\n");
        sb.append("public class " + eventName + " extends SimpleEvent<Object, Object, " + eventName + ".Type> {\r\n\r\n");
        sb.append("public static enum Type{\r\n}\r\n");
        sb.append("public " + eventName + "(Object caller, Type type, Object... parameters) {\r\n");
        sb.append("super(caller, type, parameters);\r\n}\r\n");
        sb.append("}");
        new File(file, eventName + ".java").delete();
        IO.writeStringToFile(new File(file, eventName + ".java"), sb.toString());
    }

    public static void main(final String[] args) throws DialogClosedException, DialogCanceledException, IOException, URISyntaxException {
        final URL root = Thread.currentThread().getClass().getResource("/");
        final File rootFile = new File(root.toURI());
        final String name = Dialog.getInstance().showInputDialog("Enter Name");

        final ExtFileChooserDialog d = new ExtFileChooserDialog(0, "Choose folder", null, null);
        d.setStorageID("EventSenderCReater");
        d.setFileSelectionMode(FileChooserSelectionMode.DIRECTORIES_ONLY);

        d.setType(FileChooserType.OPEN_DIALOG);
        d.setMultiSelection(false);
        d.setPreSelection(rootFile.getParentFile().getParentFile());
        try {
            Dialog.I().showDialog(d);
        } catch (final DialogClosedException e) {            
            e.printStackTrace();
        } catch (final DialogCanceledException e) {            
            e.printStackTrace();
        }

        create(name, d.getSelectedFile());
        System.exit(1);
    }
}
