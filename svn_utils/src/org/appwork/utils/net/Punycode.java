package org.appwork.utils.net;

import java.text.Normalizer;
import java.util.Locale;

public class Punycode {

    // RFC 3492 Bootstring-Parameter
    private static final int BASE         = 36;
    private static final int TMIN         = 1;
    private static final int TMAX         = 26;
    private static final int SKEW         = 38;
    private static final int DAMP         = 700;
    private static final int INITIAL_BIAS = 72;
    private static final int INITIAL_N    = 128;

    public static String toASCII(String domain) {
        if (domain == null || domain.isEmpty()) {
            return domain;
        }
        StringBuilder result = new StringBuilder(domain.length() + 16);
        int start = 0;
        int len = domain.length();
        while (start <= len) {
            int dot = findNextDot(domain, start);
            String label = (dot < 0) ? domain.substring(start) : domain.substring(start, dot);
            if (!label.isEmpty()) {
                if (result.length() > 0) {
                    result.append('.');
                }
                result.append(encodeLabel(label));
            } else if (dot >= 0 && start == 0) {
                // Führender Punkt beibehalten, falls gewünscht
                result.append('.');
            }
            if (dot < 0) {
                break;
            }
            start = dot + 1;
        }
        return result.toString();
    }

    private static String encodeLabel(String label) {
        if (isAscii(label)) {
            return label.toLowerCase(Locale.ROOT);
        }
        // IDNA2008 / UTS #46 Vorbereitung: NFC + Lowercase (erhält 'ß' und wandelt 'ẞ' -> 'ß')
        if (!Normalizer.isNormalized(label, Normalizer.Form.NFC)) {
            label = Normalizer.normalize(label, Normalizer.Form.NFC);
        }
        label = label.toLowerCase(Locale.ROOT);
        if (isAscii(label)) {
            return label;
        }

        int[] codePoints = toCodePoints(label);
        StringBuilder out = new StringBuilder(label.length() + 8);
        out.append("xn--");

        int basicCount = 0;
        for (int cp : codePoints) {
            if (cp < 0x80) {
                out.append((char) cp);
                basicCount++;
            }
        }
        int handled = basicCount;
        if (basicCount > 0) {
            out.append('-');
        }

        int n = INITIAL_N;
        int delta = 0;
        int bias = INITIAL_BIAS;

        while (handled < codePoints.length) {
            int m = Integer.MAX_VALUE;
            for (int cp : codePoints) {
                if (cp >= n && cp < m) {
                    m = cp;
                }
            }
            delta += (m - n) * (handled + 1);
            n = m;

            for (int cp : codePoints) {
                if (cp < n) {
                    delta++;
                } else if (cp == n) {
                    int q = delta;
                    for (int k = BASE;; k += BASE) {
                        int t = (k <= bias) ? TMIN : (k >= bias + TMAX ? TMAX : k - bias);
                        if (q < t) {
                            break;
                        }
                        out.append(digitToChar(t + (q - t) % (BASE - t)));
                        q = (q - t) / (BASE - t);
                    }
                    out.append(digitToChar(q));
                    bias = adapt(delta, handled + 1, handled == basicCount);
                    delta = 0;
                    handled++;
                }
            }
            delta++;
            n++;
        }
        return out.toString();
    }

    private static int adapt(int delta, int numPoints, boolean firstTime) {
        delta = firstTime ? delta / DAMP : delta >> 1;
        delta += delta / numPoints;
        int k = 0;
        while (delta > ((BASE - TMIN) * TMAX) / 2) {
            delta /= (BASE - TMIN);
            k += BASE;
        }
        return k + (((BASE - TMIN + 1) * delta) / (delta + SKEW));
    }

    private static char digitToChar(int d) {
        return (char) (d < 26 ? 'a' + d : '0' + (d - 26));
    }

    public static boolean isAscii(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) >= 0x80) {
                return false;
            }
        }
        return true;
    }

    private static int findNextDot(String s, int start) {
        for (int i = start; i < s.length(); i++) {
            char c = s.charAt(i);
            // IDNA-Label-Trenner: '.', '。', '．', '｡'
            if (c == '.' || c == '\u3002' || c == '\uFF0E' || c == '\uFF61') {
                return i;
            }
        }
        return -1;
    }

    private static int[] toCodePoints(String s) {
        int count = s.codePointCount(0, s.length());
        int[] cps = new int[count];
        int idx = 0;
        for (int i = 0; i < s.length();) {
            int cp = s.codePointAt(i);
            cps[idx++] = cp;
            i += Character.charCount(cp);
        }
        return cps;
    }
}