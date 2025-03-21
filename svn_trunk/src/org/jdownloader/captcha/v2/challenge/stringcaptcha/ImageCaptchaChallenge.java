package org.jdownloader.captcha.v2.challenge.stringcaptcha;

import java.awt.Image;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import jd.nutils.encoding.Base64;
import jd.plugins.Plugin;

import org.appwork.utils.StringUtils;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.net.Base64OutputStream;
import org.jdownloader.captcha.v2.Challenge;
import org.seamless.util.io.IO;

public abstract class ImageCaptchaChallenge<T> extends Challenge<T> {
    protected volatile File imageFile;

    public ImageCaptchaChallenge(File file, String method, String explain, Plugin plugin) {
        super(method, explain);
        this.imageFile = file;
        this.plugin = plugin;
    }

    public Image getImage() throws IOException {
        final File imageFile = getImageFile();
        if (imageFile == null || !imageFile.isFile()) {
            return null;
        }
        return ImageIO.read(imageFile);
    }

    public byte[] getAnnotatedImageBytes() throws IOException {
        return IconIO.toJpgBytes(getAnnotatedImage());
    }

    /**
     * The AnnotatedImage may contain instructions that are not in the original image.
     *
     *
     * @return
     * @throws IOException
     */
    public Image getAnnotatedImage() throws IOException {
        return getImage();
    }

    public String toString() {
        return "CaptchaChallenge by " + getHost() + "-" + getTypeID() + " File: " + getImageFile();
    }

    public Plugin getPlugin() {
        return plugin;
    }

    private final Plugin plugin;

    public synchronized File getImageFile() {
        if (imageFile == null && plugin != null) {
            imageFile = plugin.getLocalCaptchaFile();
        }
        return imageFile;
    }

    public String getBase64ImageFile() throws IOException {
        final File imageFile = getImageFile();
        if (imageFile == null || !imageFile.isFile()) {
            return null;
        }
        final byte[] data = IO.readBytes(imageFile);
        return Base64.encodeToString(data, false);
    }

    public void setImageFile(File imageFile) {
        this.imageFile = imageFile;
    }

    public Object getAPIStorable(String format) throws Exception {
        final File imageFile = getImageFile();
        if (StringUtils.endsWithCaseInsensitive(imageFile.getName(), "gif")) {
            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            final Base64OutputStream b64os = new Base64OutputStream(bos);
            b64os.write(IO.readBytes(imageFile));
            b64os.close();
            return "image/gif;base64," + bos.toString("UTF-8");
        } else if (StringUtils.endsWithCaseInsensitive(imageFile.getName(), "png")) {
            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            final Base64OutputStream b64os = new Base64OutputStream(bos);
            b64os.write(IO.readBytes(imageFile));
            b64os.close();
            return "image/png;base64," + bos.toString("UTF-8");
        } else if (StringUtils.endsWithCaseInsensitive(imageFile.getName(), "jpg") || StringUtils.endsWithCaseInsensitive(imageFile.getName(), "jpeg")) {
            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            final Base64OutputStream b64os = new Base64OutputStream(bos);
            b64os.write(IO.readBytes(imageFile));
            b64os.close();
            return "image/jpeg;base64," + bos.toString("UTF-8");
        } else {
            return IconIO.toDataUrl(ImageIO.read(getImageFile()), IconIO.DataURLFormat.JPG);
        }
    }
}
