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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.swing.components.ide;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FilenameFilter;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.JToggleButton;

import org.appwork.app.gui.BasicGui;
import org.appwork.resources.AWUTheme;
import org.appwork.resources.IconRefImpl;
import org.appwork.swing.MigPanel;
import org.appwork.swing.components.searchcombo.SearchComboBox;

/**
 * @author thomas
 * @date 22.02.2023
 *
 */
public class SearchComboBoxExample {
    public static void main(final String[] args) {
        final BasicGui gui = new BasicGui(SearchComboBox.class.getSimpleName()) {
            @Override
            protected void layoutPanel() {
                try {
                    final SearchComboBox<String> box1 = new SearchComboBox<String>() {
                        /**
                         *
                         */
                        private static final long serialVersionUID = 743905470697711746L;

                        @Override
                        protected Icon getIconForValue(final String value) {
                            return value == null ? new IconRefImpl("close").icon(28) : AWUTheme.getInstance().getIcon(value, 28);
                        }

                        @Override
                        protected String getTextForValue(final String value) {
                            return value + "-icon";
                        }
                    };
                    final String p = AWUTheme.I().getPath();
                    final java.util.List<String> list = new ArrayList<String>();
                    final URL images = AWUTheme.class.getResource(p + "images");
                    for (final String s : new File(images.toURI()).list(new FilenameFilter() {
                        @Override
                        public boolean accept(final File dir, final String name) {
                            return name.endsWith(".png");
                        }
                    })) {
                        list.add(s.replace(".png", ""));
                    }
                    final MigPanel contentPane = new MigPanel("ins 10,wrap 1", "[grow,fill]", "[]");
                    this.getFrame().setContentPane(contentPane);
                    box1.setList(list);
                    box1.setBorder(BorderFactory.createEtchedBorder());
                    this.getFrame().getContentPane().add(box1);
                    final JToggleButton toggle = new JToggleButton("Toggle Allow unknown");
                    this.getFrame().getContentPane().add(toggle);
                    toggle.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            box1.setUnkownTextInputAllowed(toggle.isSelected());
                        }
                    });
                    final JToggleButton toggle2 = new JToggleButton("Toggle HelpText");
                    this.getFrame().getContentPane().add(toggle2);
                    toggle2.addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(final ActionEvent e) {
                            box1.setHelpText(toggle2.isSelected() ? "I'm Help Text" : null);
                        }
                    });
                } catch (final URISyntaxException e) {
                    e.printStackTrace();
                }
            }

            @Override
            protected void requestExit() {
                System.exit(1);
            }
        };
    }
}
