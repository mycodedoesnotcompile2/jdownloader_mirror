package org.jdownloader.plugins.components.youtube.converter;

import java.io.File;

import javax.swing.Icon;

import jd.plugins.DownloadLink;
import jd.plugins.PluginForHost;
import jd.plugins.PluginProgress;

import org.jdownloader.gui.IconKey;
import org.jdownloader.gui.views.downloads.columns.ETAColumn;
import org.jdownloader.images.AbstractIcon;
import org.jdownloader.plugins.PluginTaskID;
import org.jdownloader.utils.SubtitleConverter;

public class YoutubeSRTConverter implements YoutubeConverter {
    private static final YoutubeSRTConverter INSTANCE = new YoutubeSRTConverter();

    /**
     * get the only existing instance of YoutubeSRTConverter. This is a singleton
     *
     * @return
     */
    public static YoutubeSRTConverter getInstance() {
        return YoutubeSRTConverter.INSTANCE;
    }

    /**
     * Create a new instance of YoutubeSRTConverter. This is a singleton class. Access the only existing instance by using
     * {@link #getInstance()}.
     */
    private YoutubeSRTConverter() {

    }

    @Override
    public boolean run(DownloadLink downloadLink, PluginForHost plugin) {
        PluginProgress set = null;
        try {
            downloadLink.addPluginProgress(set = new PluginProgress(0, 100, null) {
                {
                    setIcon(new AbstractIcon(IconKey.ICON_TEXT, 18));

                }

                @Override
                public long getCurrent() {
                    return 95;
                }

                @Override
                public PluginTaskID getID() {
                    return PluginTaskID.CONVERT;
                }

                @Override
                public Icon getIcon(Object requestor) {
                    if (requestor instanceof ETAColumn) {
                        return null;
                    }
                    return super.getIcon(requestor);
                }

                @Override
                public String getMessage(Object requestor) {
                    if (requestor instanceof ETAColumn) {
                        return "";
                    }
                    return "Convert";
                }
            });
            final File file = new File(downloadLink.getFileOutput());
            final File finalFile = new File(file.getAbsolutePath().replaceFirst("\\.srt\\.tmp$", ".srt"));
            try {
                final boolean ret = SubtitleConverter.convertGoogleCC2SRTSubtitles(file, finalFile);
                try {
                    downloadLink.setInternalTmpFilenameAppend(null);
                    downloadLink.setInternalTmpFilename(null);
                } catch (final Throwable e) {
                }
                return ret;
            } catch (Exception e) {
                plugin.getLogger().log(e);
                return false;
            }
        } finally {
            downloadLink.removePluginProgress(set);
        }

    }

}