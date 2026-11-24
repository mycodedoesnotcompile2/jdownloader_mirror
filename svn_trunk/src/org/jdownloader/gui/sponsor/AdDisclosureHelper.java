package org.jdownloader.gui.sponsor;

import java.awt.Font;
import java.util.Locale;

import javax.swing.JLabel;
import javax.swing.SwingConstants;
import javax.swing.UIManager;

import org.appwork.txtresource.TranslationFactory;

public class AdDisclosureHelper {
    /*
     * Evaluated once on class load. A language change requires a JDownloader restart anyway.
     */
    public final static String AD_DISCLOSURE_TEXT = getAdDisclosureText(TranslationFactory.getDesiredLocale());
    public static final String AD_LABEL_SHORT     = getShortAdLabel(TranslationFactory.getDesiredLocale());

    /**
     * Creates the label that shows the ad disclosure text (single line, no wrapping).
     *
     * @param horizontalAlignment
     *            A {@link SwingConstants} horizontal alignment, or null for the label default
     * @param verticalAlignment
     *            A {@link SwingConstants} vertical alignment, or null for the label default
     */
    public static JLabel createAdDisclosureLabel(final Integer horizontalAlignment, final Integer verticalAlignment) {
        Font baseFont = UIManager.getFont("Label.font");
        if (baseFont == null) {
            baseFont = new JLabel().getFont();
        }
        final JLabel ret = new JLabel(AD_DISCLOSURE_TEXT);
        ret.setForeground(UIManager.getColor("Label.foreground"));
        if (horizontalAlignment != null) {
            ret.setHorizontalAlignment(horizontalAlignment);
        }
        if (verticalAlignment != null) {
            ret.setVerticalAlignment(verticalAlignment);
        }
        ret.setFont(baseFont.deriveFont(10f));
        return ret;
    }

    private static String getShortAdLabel(final Locale locale) {
        if (locale == null) {
            return "Ad";
        }
        final String lang = locale.getLanguage();
        if (lang == null || lang.length() == 0) {
            return "Ad";
        } else if ("de".equals(lang)) {
            return "Anzeige";
        } else if ("fr".equals(lang)) {
            return "Pub";
        } else if ("es".equals(lang)) {
            return "Publi";
        } else if ("it".equals(lang)) {
            return "Adv";
        } else if ("nl".equals(lang)) {
            return "Adv.";
        } else if ("pl".equals(lang)) {
            return "Reklama";
        } else if ("pt".equals(lang)) {
            return "Pub";
        } else if ("sv".equals(lang)) {
            return "Annons";
        } else if ("da".equals(lang)) {
            return "Annonce";
        } else if ("no".equals(lang) || "nb".equals(lang) || "nn".equals(lang)) {
            return "Annonse";
        } else if ("tr".equals(lang)) {
            return "Reklam";
        } else if ("ru".equals(lang) || "uk".equals(lang)) {
            return "Реклама";
        } else if ("ja".equals(lang)) {
            return "広告";
        } else if ("zh".equals(lang)) {
            return "广告";
        } else if ("ko".equals(lang)) {
            return "광고";
        } else {
            return "Ad";
        }
    }

    private static String getAdDisclosureText(final Locale locale) {
        final String lang = (locale != null && locale.getLanguage() != null) ? locale.getLanguage() : "";
        if ("de".equals(lang)) {
            return "Anzeige: Beim Kauf oder Abschluss können wir eine Provision erhalten.";
        } else if ("fr".equals(lang)) {
            return "Publicité : Nous pouvons percevoir une commission en cas d'achat ou de souscription.";
        } else if ("es".equals(lang)) {
            return "Publicidad: Podemos recibir una comisión si realizas una compra o contratación.";
        } else if ("it".equals(lang)) {
            return "Pubblicità: Potremmo ricevere una commissione in caso di acquisto o sottoscrizione.";
        } else if ("nl".equals(lang)) {
            return "Advertentie: Bij een aankoop of afsluiting kunnen wij een commissie ontvangen.";
        } else if ("pl".equals(lang)) {
            return "Reklama: W przypadku zakupu lub zawarcia umowy możemy otrzymać prowizję.";
        } else if ("pt".equals(lang)) {
            return "Publicidade: Podemos receber uma comissão em caso de compra ou subscrição.";
        } else if ("sv".equals(lang)) {
            return "Annons: Vi kan få en provision om du gör ett köp eller tecknar ett abonnemang.";
        } else if ("da".equals(lang)) {
            return "Annonce: Vi kan modtage en provision, hvis du foretager et køb eller tilmelder dig.";
        } else if ("no".equals(lang) || "nb".equals(lang) || "nn".equals(lang)) {
            return "Annonse: Vi kan motta en provisjon hvis du foretar et kjøp eller registrerer deg.";
        } else if ("tr".equals(lang)) {
            return "Reklam: Satın alma veya abonelik durumunda komisyon alabiliriz.";
        } else if ("ru".equals(lang)) {
            return "Реклама: При покупке или оформлении подписки мы можем получить комиссию.";
        } else if ("uk".equals(lang)) {
            return "Реклама: Під час покупки або оформлення підписки ми можемо отримати комісію.";
        } else if ("ja".equals(lang)) {
            return "広告：購入または契約が行われた場合、当社は紹介料を受け取ることがあります。";
        } else if ("zh".equals(lang)) {
            return "广告：如果您通过此链接购买或订阅，我们可能会获得佣金。";
        } else if ("ko".equals(lang)) {
            return "광고: 구매 또는 가입 시 당사가 수수료를 받을 수 있습니다.";
        } else {
            /* English default (FTC compliant for US/UK and international) */
            return "Ad: We may earn a commission if you make a purchase or sign up.";
        }
    }
}
