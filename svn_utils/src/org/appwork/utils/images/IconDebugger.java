/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.utils.images;

/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2025, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
 * ====================================================================================================================================================
 */
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

import org.appwork.resources.MultiResolutionImageHelper;

public class IconDebugger extends JFrame {
    private static IconDebugger   instance;
    private JPanel                container;
    private Map<String, DebugRow> entries;

    private IconDebugger() {
        super("Icon Debugger");
        container = new JPanel();
        container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));
        entries = new HashMap<String, DebugRow>();
        JScrollPane scrollPane = new JScrollPane(container);
        scrollPane.getVerticalScrollBar().setUnitIncrement(20);
        getContentPane().add(scrollPane, BorderLayout.CENTER);
        // Beim Schließen des Fensters soll die Anwendung NICHT beendet werden.
        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        setSize(600, 400);
        setLocationRelativeTo(null);
        setVisible(true);
    }

    private void update(Icon icon, String text) {
        if (entries.containsKey(text)) {
            DebugRow row = entries.get(text);
            row.iconLabel.setNewIcon(icon);
            row.updateInfo(icon);
        } else {
            DebugRow row = new DebugRow(icon, text);
            container.add(row.panel);
            entries.put(text, row);
        }
        container.revalidate();
        container.repaint();
    }

    public static void show(Icon icon, String text) {
        if (instance == null) {
            instance = new IconDebugger();
        }
        instance.update(icon, text);
        if (!instance.isVisible()) {
            instance.setVisible(true);
        }
    }

    public static void show(Image image, String text) {
        show(new ImageIcon(image), text);
    }

    public static void show(Image... images) {
        for (int i = 0; i < images.length; i++) {
            ImageIcon icon = new ImageIcon(images[i]);
            show(icon, "Image " + (i + 1));
        }
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                Icon redIcon = createIcon(Color.RED, 16, 16);
                Icon greenIcon = createIcon(Color.GREEN, 32, 32);
                Icon blueIcon = createIcon(Color.BLUE, 48, 48);
                IconDebugger.show(redIcon, "Test Icon");
                IconDebugger.show(greenIcon, "Another Icon");
                IconDebugger.show(blueIcon, "Test Icon");
                Image yellowImage = createImage(Color.YELLOW, 64, 64);
                Image magentaImage = createImage(Color.MAGENTA, 80, 80);
                IconDebugger.show(yellowImage, "Yellow Image");
                IconDebugger.show(magentaImage, "Magenta Image");
                IconDebugger.show(yellowImage, magentaImage);
            }
        });
    }

    private static Icon createIcon(Color color, int width, int height) {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2 = image.createGraphics();
        g2.setColor(color);
        g2.fillRect(0, 0, width, height);
        g2.dispose();
        return new ImageIcon(image);
    }

    private static Image createImage(Color color, int width, int height) {
        BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2 = image.createGraphics();
        g2.setColor(color);
        g2.fillRect(0, 0, width, height);
        g2.dispose();
        return image;
    }

    // Erweiterte Listener-Schnittstelle für Pixelinfo: temporär (mouse over) und fix (click)
    private static interface PixelInfoListener {
        void pixelInfoUpdated(String infoLine);

        void pixelInfoFixed(String infoLine);
    }

    // DebugRow enthält das Icon, einen statischen Beschreibungstext und ein kopierbares Textfeld.
    // Bei Mouseover werden temporär der aktuelle Pixelfarbwert (als Hex) angezeigt,
    // ein Klick fügt den aktuell ermittelten Farbwert als festen Bestandteil hinzu.
    private static class DebugRow {
        public ZoomableIconLabel iconLabel;
        public JTextArea         infoTextArea;
        public JPanel            panel;
        private String           iconText;
        private String           baseInfo;
        private String           temporaryPixelInfo = "";
        private StringBuilder    fixedPixelInfos    = new StringBuilder();

        public DebugRow(Icon icon, String text) {
            this.iconText = text;
            panel = new JPanel();
            panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
            iconLabel = new ZoomableIconLabel(icon);
            iconLabel.setAlignmentY(Component.CENTER_ALIGNMENT);
            iconLabel.setPixelInfoListener(new PixelInfoListener() {
                public void pixelInfoUpdated(String infoLine) {
                    temporaryPixelInfo = infoLine;
                    updateInfoPanel();
                }

                public void pixelInfoFixed(String infoLine) {
                    if (fixedPixelInfos.length() > 0) {
                        fixedPixelInfos.append(", ");
                    }
                    fixedPixelInfos.append(infoLine);
                    updateInfoPanel();
                }
            });
            baseInfo = buildInfo(icon);
            infoTextArea = new JTextArea();
            infoTextArea.setEditable(false);
            infoTextArea.setLineWrap(true);
            infoTextArea.setWrapStyleWord(true);
            infoTextArea.setFont(new Font("Monospaced", Font.PLAIN, 12));
            infoTextArea.setPreferredSize(new Dimension(250, 50));
            updateInfoPanel();
            panel.add(iconLabel);
            panel.add(Box.createRigidArea(new Dimension(10, 0)));
            panel.add(infoTextArea);
        }

        private void updateInfoPanel() {
            StringBuilder sb = new StringBuilder();
            sb.append("Icon: ").append(iconText).append("\n");
            sb.append(baseInfo);
            if (fixedPixelInfos.length() > 0) {
                sb.append("\nFixed: ").append(fixedPixelInfos.toString());
            }
            if (!temporaryPixelInfo.isEmpty()) {
                sb.append("\nCurrent: ").append(temporaryPixelInfo);
            }
            infoTextArea.setText(sb.toString());
        }

        public void updateInfo(Icon icon) {
            baseInfo = buildInfo(icon);
            updateInfoPanel();
        }

        // Hier wird zusätzlich geprüft, ob das zugrundeliegende Image ein MultiResolutionImage ist.
        // Falls ja, werden die verfügbaren Auflösungen (Variants) ermittelt.
        private String buildInfo(Icon icon) {
            StringBuilder sb = new StringBuilder();
            sb.append("Icon Class: ").append(icon.getClass().getName());
            sb.append(" | Dimensions: ").append(icon.getIconWidth()).append("x").append(icon.getIconHeight());
            if (icon instanceof ImageIcon) {
                Image image = ((ImageIcon) icon).getImage();
                try {
                    if (MultiResolutionImageHelper.isInstanceOf(image)) {
                        sb.append(" | MultiResolutionImage: yes");
                        java.util.List<Image> variants = MultiResolutionImageHelper.getResolutionVariants(image);
                        if (!variants.isEmpty()) {
                            sb.append(" | Variants:");
                            for (Image variant : variants) {
                                int w = new ImageIcon(variant).getIconWidth();
                                int h = new ImageIcon(variant).getIconHeight();
                                sb.append(" ").append(w).append("x").append(h).append(";");
                            }
                        }
                    } else {
                        sb.append(" | MultiResolutionImage: no");
                    }
                } catch (Exception e) {
                    sb.append(" | MultiResolutionImage: no");
                }
            }
            return sb.toString();
        }
    }

    // ZoomableIconLabel zeigt das Icon mit karikiertem Hintergrund (weiß/hellgrau),
    // unterstützt Zoomen per Mausrad und liefert bei Mouseover den exakten Pixelfarbwert (als Hex).
    // Bei Klick wird der aktuell ermittelte Farbwert fix an den PixelInfoListener gemeldet.
    private static class ZoomableIconLabel extends JLabel {
        private Icon              originalIcon;
        private double            zoomFactor = 1.0;
        private PixelInfoListener pixelInfoListener;

        public ZoomableIconLabel(Icon icon) {
            super(icon);
            setOpaque(false);
            setNewIcon(icon);
            setFocusable(true);
            addMouseWheelListener(new MouseWheelListener() {
                public void mouseWheelMoved(MouseWheelEvent e) {
                    if (originalIcon == null) {
                        return;
                    }
                    int notches = e.getWheelRotation();
                    double newZoom = zoomFactor - notches * 0.1;
                    if (newZoom < 0.1) {
                        newZoom = 0.1;
                    }
                    zoomFactor = newZoom;
                    applyZoom();
                }
            });
            addMouseMotionListener(new MouseMotionListener() {
                public void mouseMoved(MouseEvent e) {
                    if (originalIcon != null) {
                        String hex = getPixelHexAt(e.getX(), e.getY());
                        if (pixelInfoListener != null) {
                            pixelInfoListener.pixelInfoUpdated(hex);
                        }
                    }
                }

                public void mouseDragged(MouseEvent e) {
                    mouseMoved(e);
                }
            });
            addMouseListener(new MouseAdapter() {
                public void mouseClicked(MouseEvent e) {
                    if (originalIcon != null) {
                        String hex = getPixelHexAt(e.getX(), e.getY());
                        if (pixelInfoListener != null) {
                            pixelInfoListener.pixelInfoFixed(hex);
                        }
                    }
                }
            });
        }

        public void setPixelInfoListener(PixelInfoListener listener) {
            this.pixelInfoListener = listener;
        }

        // Für alle Icons, auch wenn sie nicht bereits ImageIcons sind, wird hier konvertiert.
        public void setNewIcon(Icon newIcon) {
            originalIcon = newIcon;
            applyZoom();
        }

        private String getPixelHexAt(int x, int y) {
            // Hier wird das aktuell angezeigte Icon (skalierte Version) verwendet.
            // Damit auch MultiResolution-Icons korrekt verarbeitet werden, wird zunächst
            // das Icon in ein Image konvertiert.
            Image image = IconIO.toImage(getIcon());
            BufferedImage bimg = IconIO.toBufferedImage(image);
            if (x >= 0 && x < bimg.getWidth() && y >= 0 && y < bimg.getHeight()) {
                int pixel = bimg.getRGB(x, y);
                int alpha = (pixel >> 24) & 0xFF;
                int red = (pixel >> 16) & 0xFF;
                int green = (pixel >> 8) & 0xFF;
                int blue = pixel & 0xFF;
                return String.format("#%02X%02X%02X%02X", alpha, red, green, blue);
            }
            return "Outside image bounds";
        }

        private void applyZoom() {
            int newWidth = (int) (originalIcon.getIconWidth() * zoomFactor);
            int newHeight = (int) (originalIcon.getIconHeight() * zoomFactor);
            setIcon(IconIO.getScaledInstance(originalIcon, newWidth, newHeight));
        }

        @Override
        protected void paintComponent(Graphics g) {
            // Karierter Hintergrund: abwechselnd weiß und hellgrau
            int tileSize = 10;
            int width = getWidth();
            int height = getHeight();
            for (int y = 0; y < height; y += tileSize) {
                for (int x = 0; x < width; x += tileSize) {
                    boolean isLight = ((x / tileSize) + (y / tileSize)) % 2 == 0;
                    g.setColor(isLight ? Color.white : Color.lightGray);
                    g.fillRect(x, y, tileSize, tileSize);
                }
            }
            super.paintComponent(g);
        }
    }
}
