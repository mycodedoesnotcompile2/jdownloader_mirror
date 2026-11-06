package org.jdownloader.captcha.v2.challenge.stringcaptcha;

import java.awt.Image;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.appwork.utils.StringUtils;
import org.appwork.utils.images.IconIO;
import org.appwork.utils.net.Base64OutputStream;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.Challenge;
import org.jdownloader.captcha.v2.ValidationResult;
import org.seamless.util.io.IO;

import jd.nutils.encoding.Base64;
import jd.plugins.Plugin;

public abstract class ImageCaptchaChallenge<T> extends Challenge<T> {
    protected volatile File imageFile;

    public ImageCaptchaChallenge(File file, String method, String explain, Plugin plugin) {
        super(method, explain);
        this.imageFile = file;
        this.plugin = plugin;
    }

    /* null = not set */
    private volatile Boolean caseSensitive = null;
    /* null = not set */
    private volatile Boolean numeric       = null;

    /** @return true: Answer is case sensitive, false: Answer is not case sensitive, null: Not set. */
    public Boolean getCaseSensitive() {
        return caseSensitive;
    }

    public void setCaseSensitive(final Boolean caseSensitive) {
        this.caseSensitive = caseSensitive;
    }

    /** @return true: Answer contains only digits, false: Answer may contain other characters, null: Not set. */
    public Boolean getNumeric() {
        return numeric;
    }

    public void setNumeric(final Boolean numeric) {
        this.numeric = numeric;
    }

    /* Minimum number of characters of a text answer, at least 1: An empty answer is never valid. */
    private volatile int minLength = 1;
    /* Maximum number of characters of a text answer, 0 = no limit */
    private volatile int maxLength = 0;

    @Override
    public void cleanup() {
        try {
            super.cleanup();
        } finally {
            final File imageFile = getSetImageFile();
            if (imageFile != null && !imageFile.delete() && imageFile.exists()) {
                imageFile.deleteOnExit();
            }
        }
    }

    /**
     * @return Minimum number of characters of a text answer (whitespace at the start and the end is not counted). At least 1.<br>
     *         TODO: Make the default configurable via a setting.
     */
    public int getMinLength() {
        return minLength;
    }

    public void setMinLength(final int minLength) {
        this.minLength = Math.max(1, minLength);
    }

    /** @return Maximum number of characters of a text answer (whitespace at the start and the end is not counted). 0 = no limit. */
    public int getMaxLength() {
        return maxLength;
    }

    public void setMaxLength(final int maxLength) {
        this.maxLength = Math.max(0, maxLength);
    }

    /**
     * @return true if the min. and max. length is the same, which means that the answer must have exactly this number of characters.<br>
     *         TODO: The captcha dialog could close itself automatically as soon as the entered text has this exact length. This was not
     *         added yet because users may want to check their input before sending it.
     */
    public boolean hasFixedLength() {
        return getMaxLength() > 0 && getMinLength() == getMaxLength();
    }

    /** @return true if the given text answer has the required length, see {@link #getMinLength()} and {@link #getMaxLength()}. */
    public boolean isValidLength(final String value) {
        if (value == null) {
            return false;
        }
        final int length = value.trim().length();
        final int max = getMaxLength();
        return length >= getMinLength() && (max <= 0 || length <= max);
    }

    /**
     * Text answers must have the required length and, if the challenge is numeric, may only consist of digits. Answers that are not text
     * (e.g. click positions) are not checked here.
     */
    @Override
    public ValidationResult validateResponse(final AbstractResponse<T> response) {
        final ValidationResult result = super.validateResponse(response);
        if (result != ValidationResult.VALID) {
            return result;
        }
        final Object value = response.getValue();
        if (value instanceof String) {
            final String text = (String) value;
            if (text.trim().length() == 0) {
                return ValidationResult.EMPTY_RESPONSE;
            } else if (!isValidLength(text)) {
                return ValidationResult.WRONG_LENGTH;
            } else if (Boolean.TRUE.equals(numeric) && !text.matches("\\d+")) {
                return ValidationResult.NOT_NUMERIC;
            }
        }
        return ValidationResult.VALID;
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
        File imageFile = getSetImageFile();
        if (imageFile != null) {
            return imageFile;
        }
        final Plugin plugin = this.getPlugin();
        if (plugin == null) {
            return null;
        }
        return this.imageFile = plugin.getLocalCaptchaFile();
    }

    public synchronized File getSetImageFile() {
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
