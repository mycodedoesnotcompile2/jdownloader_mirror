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
package org.appwork.utils.formatter;

import java.text.NumberFormat;
import java.util.Currency;
import java.util.Locale;

/**
 * Locale aware formatter for monetary amounts.
 *
 * The user's default locale ({@link Locale#getDefault()}) decides the position of the currency symbol as well as the grouping- and
 * decimal-separators, so that the output matches what the user expects (e.g. "1.234,56 $" for a German user vs. "$1,234.56" for a US user).
 * The passed {@link Currency} only decides which symbol/code is shown and the default number of fraction digits.
 */
public class CurrencyFormatter {
    /**
     * Sentinel value for the fraction-digit parameters meaning "keep the format's default number of fraction digits".
     */
    public static final int DEFAULT_FRACTION_DIGITS = -1;

    /**
     * Formats the given amount using the user's default locale and the given currency's default number of fraction digits.
     *
     * @param amount
     *            the monetary amount
     * @param currency
     *            the currency whose symbol is displayed, or null to format the plain number without any currency symbol
     * @return the formatted amount
     */
    public static String format(final double amount, final Currency currency) {
        return format(amount, currency, DEFAULT_FRACTION_DIGITS, DEFAULT_FRACTION_DIGITS);
    }

    /**
     * Formats the given amount using the user's default locale with an explicit number of fraction digits.
     *
     * @param amount
     *            the monetary amount
     * @param currency
     *            the currency whose symbol is displayed, or null to format the plain number without any currency symbol
     * @param minFractionDigits
     *            minimum number of fraction digits, or {@link #DEFAULT_FRACTION_DIGITS} to keep the format's default
     * @param maxFractionDigits
     *            maximum number of fraction digits, or {@link #DEFAULT_FRACTION_DIGITS} to keep the format's default
     * @return the formatted amount
     */
    public static String format(final double amount, final Currency currency, final int minFractionDigits, final int maxFractionDigits) {
        final NumberFormat nf = createFormat(currency);
        if (minFractionDigits != DEFAULT_FRACTION_DIGITS) {
            nf.setMinimumFractionDigits(minFractionDigits);
        }
        if (maxFractionDigits != DEFAULT_FRACTION_DIGITS) {
            nf.setMaximumFractionDigits(maxFractionDigits);
        }
        return nf.format(amount);
    }

    /**
     * Creates a fresh {@link NumberFormat} for the user's default locale.
     *
     * If a currency is given, a currency-instance is returned with that currency applied; otherwise a plain number-instance is returned so
     * that no (potentially wrong) default currency symbol is displayed. The returned instance is not shared and may be freely mutated by the
     * caller (e.g. to set fraction digits or to be cloned per table-row).
     *
     * @param currency
     *            the currency to apply, or null for a plain number format
     * @return a new, mutable NumberFormat
     */
    public static NumberFormat createFormat(final Currency currency) {
        final NumberFormat nf;
        if (currency != null) {
            nf = NumberFormat.getCurrencyInstance(Locale.getDefault());
            nf.setCurrency(currency);
        } else {
            nf = NumberFormat.getNumberInstance(Locale.getDefault());
        }
        return nf;
    }
}
