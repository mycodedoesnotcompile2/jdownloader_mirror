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
package org.appwork.utils.locale;

import org.appwork.txtresource.Default;
import org.appwork.txtresource.Defaults;
import org.appwork.txtresource.DoNotTranslate;
import org.appwork.txtresource.Example;
import org.appwork.txtresource.Name;
import org.appwork.txtresource.TranslateInterface;
import org.appwork.txtresource.Warning;

/**
 * @author thomas
 *
 */
@Defaults(lngs = { "en", "de" })
public interface AWUTranslation extends TranslateInterface {
    @Default(lngs = { "en", "de" }, values = { "Cancel", "Abbrechen" })
    String ABSTRACTDIALOG_BUTTON_CANCEL();

    @Default(lngs = { "en", "de" }, values = { "Ok", "Ok" })
    String ABSTRACTDIALOG_BUTTON_OK();

    @Default(lngs = { "en", "de" }, values = { "Don't show this again", "Nicht mehr anzeigen" })
    String ABSTRACTDIALOG_STYLE_SHOW_DO_NOT_DISPLAY_AGAIN();

    @Default(lngs = { "en", "de" }, values = { "Please enter your logins here", "Bitte geben Sie hier Ihre Zugangsdaten ein" })
    String AccountNew_AccountNew_message();

    @Default(lngs = { "en", "de" }, values = { "Enter Logins", "Zugangsdaten eingeben" })
    String AccountNew_AccountNew_title();

    @Default(lngs = { "en", "de" }, values = { "Username", "Benutzername" })
    String AccountNew_layoutDialogContent_accountname();

    @Default(lngs = { "en", "de" }, values = { "Password", "Passwort" })
    String AccountNew_layoutDialogContent_password();

    @Default(lngs = { "en", "de" }, values = { "Remember", "Merken?" })
    String AccountNew_layoutDialogContent_save();

    @Default(lngs = { "en", "de" }, values = { "Enabled", "Aktiviert" })
    String active();

    @Default(lngs = { "en", "de" }, values = { "Average: %s1/s |", "Durchschnitt: %s1/s |" })
    String AppWorkUtils_Graph_getAverageSpeedString2(@Name("speed") @Example("3,45mb") String speed);

    @Default(lngs = { "en", "de" }, values = { "Current: %s1/s", "Aktuell: %s1/s" })
    String AppWorkUtils_Graph_getSpeedString(@Name("speed") @Example("3,45mb") String speed);

    @Default(lngs = { "en", "de" }, values = { "Command -> -", " Befehl -> -" })
    String COMMANDLINEAPP_COMMAND();

    @Default(lngs = { "en", "de" }, values = { "Connecting...", "Verbinden..." })
    String connecting();

    @Default(lngs = { "en", "de" }, values = { "Copy", "Kopieren" })
    String COPYCUTPASTE_COPY();

    @Default(lngs = { "en", "de" }, values = { "Cut", "Ausschneiden" })
    String COPYCUTPASTE_CUT();

    @Default(lngs = { "en", "de" }, values = { "Delete", "Löschen" })
    String COPYCUTPASTE_DELETE();

    @Default(lngs = { "en", "de" }, values = { "Paste", "Einfügen" })
    String COPYCUTPASTE_PASTE();

    @Default(lngs = { "en", "de" }, values = { "Select", "Markieren" })
    String COPYCUTPASTE_SELECT();

    // @Default(lngs = { "en", "de" }, values = { "Please open this link in your browser: ", "Bitte öffnen Sie diesen Link in Ihrem
    // Browser:" })
    // String crossSystem_open_url_failed_msg();
    @Default(lngs = { "en", "de" }, values = { "Please confirm!", "Bitte bestätigen!" })
    String DIALOG_CONFIRMDIALOG_TITLE();

    @Default(lngs = { "en", "de" }, values = { "Error Occured", "Ein Fehler ist aufgetreten" })
    String DIALOG_ERROR_TITLE();

    @Default(lngs = { "en", "de" }, values = { "Switch to detailed view", "Detailansicht" })
    String DIALOG_FILECHOOSER_TOOLTIP_DETAILS();

    @Default(lngs = { "en", "de" }, values = { "Switch to Home", "Zum Benutzerverzeichnis wechseln" })
    String DIALOG_FILECHOOSER_TOOLTIP_HOMEFOLDER();

    @Default(lngs = { "en", "de" }, values = { "Switch to list view", "Listenansicht" })
    String DIALOG_FILECHOOSER_TOOLTIP_LIST();

    @Default(lngs = { "en", "de" }, values = { "Create new folder", "Neuen Ordner erstellen" })
    String DIALOG_FILECHOOSER_TOOLTIP_NEWFOLDER();

    @Default(lngs = { "en", "de" }, values = { "Switch to parent folder", "Einen Ordner nach oben" })
    String DIALOG_FILECHOOSER_TOOLTIP_UPFOLDER();

    @Default(lngs = { "en", "de" }, values = { "Please enter!", "Bitte eingeben!" })
    String DIALOG_INPUT_TITLE();

    @Default(lngs = { "en", "de" }, values = { "Message", "Nachricht" })
    String DIALOG_MESSAGE_TITLE();

    @Default(lngs = { "en", "de" }, values = { "Please enter!", "Bitte eingeben!" })
    String DIALOG_PASSWORD_TITLE();

    @Default(lngs = { "en", "de" }, values = { "Please enter!", "Bitte eingeben" })
    String DIALOG_SLIDER_TITLE();

    @Default(lngs = { "en", "de" }, values = { "Downloading file", "Lade Datei herunter" })
    String download_msg();

    @Default(lngs = { "en", "de" }, values = { "Download", "Download" })
    String download_title();

    @Default(lngs = { "en", "de" }, values = { "This Error is cause by:", "Grund für diesen Fehler:" })
    String ExceptionDialog_layoutDialogContent_logLabel();

    @Default(lngs = { "en", "de" }, values = { "Error details", "Fehlerdetails" })
    String ExceptionDialog_layoutDialogContent_more_button();

    @Default(lngs = { "en", "de" }, values = { "Your Password", "Ihr Passwort" })
    String extpasswordeditorcolumn_tooltip();

    @Default(lngs = { "en", "de" }, values = { "Search table", "Tabelle durchsuchen" })
    String EXTTABLE_SEARCH_DIALOG_TITLE();

    @Default(lngs = { "en", "de" }, values = { "Disable all", "Alle deaktivieren" })
    String extttable_disable_all();

    @Default(lngs = { "en", "de" }, values = { "Enable all", "Alle aktivieren" })
    String extttable_enabled_all();

    @Default(lngs = { "en", "de" }, values = { "Disabled", "Deaktiviert" })
    String inactive();

    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en", "de" }, values = { "%s1 B", "%s1 B" })
    String literally_byte(@Name("amount") @Example("1,56") long fileSize);

    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en", "de" }, values = { "%s1 GB", "%s1 GB" })
    String literally_gibibyte(@Name("amount") @Example("1,56") String format);

    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en", "de" }, values = { "%s1 GB", "%s1 GB" })
    String literally_gigabyte(@Name("amount") @Example("1,56") String format);

    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en", "de" }, values = { "%s1 KB", "%s1 KB" })
    String literally_kibibyte(@Name("amount") @Example("1,56") String format);

    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en", "de" }, values = { "%s1 KB", "%s1 KB" })
    String literally_kilobyte(@Name("amount") @Example("1,56") String format);

    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en", "de" }, values = { "%s1 MB", "%s1 MB" })
    String literally_mebibyte(@Name("amount") @Example("1,56") String format);

    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en", "de" }, values = { "%s1 MB", "%s1 MB" })
    String literally_megabyte(@Name("amount") @Example("1,56") String format);

    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en", "de" }, values = { "%s1 TB", "%s1 TB" })
    String literally_tebibyte(@Name("amount") @Example("1,56") String format);

    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en", "de" }, values = { "%s1 TB", "%s1 TB" })
    String literally_terabyte(@Name("amount") @Example("1,56") String format);

    @Default(lngs = { "en", "de" }, values = { "Column width locked", "Spaltenbreite verankert" })
    String LockColumnWidthAction2();

    @Default(lngs = { "en", "de" }, values = { "Login", "Anmelden" })
    String LOGINDIALOG_BUTTON_LOGIN();

    @Default(lngs = { "en", "de" }, values = { "Create new User", "Neuen Benutzer anlegen" })
    String LOGINDIALOG_BUTTON_REGISTER();

    @Default(lngs = { "en", "de" }, values = { "Remember", "Merken" })
    String LOGINDIALOG_CHECKBOX_REMEMBER();

    @Default(lngs = { "en", "de" }, values = { "Password", "Passwort" })
    String LOGINDIALOG_LABEL_PASSWORD();

    @Default(lngs = { "en", "de" }, values = { "Repeat Password", "Passwort wiederholen" })
    String LOGINDIALOG_LABEL_PASSWORD_REPEAT();

    @Default(lngs = { "en", "de" }, values = { "Login", "Benutzername" })
    String LOGINDIALOG_LABEL_USERNAME();

    @Default(lngs = { "en", "de" }, values = { "New Password:", "Neues Passwort" })
    String PASSWORDDIALOG_PASSWORDCHANGE_NEWPASSWORD();

    @Default(lngs = { "en", "de" }, values = { "Confirm Password:", "Passwort bestätigen" })
    String PASSWORDDIALOG_PASSWORDCHANGE_NEWPASSWORD_REPEAT();

    @Default(lngs = { "en", "de" }, values = { "Old Password:", "Altes Passwort" })
    String PASSWORDDIALOG_PASSWORDCHANGE_OLDPASSWORD();

    @Default(lngs = { "en", "de" }, values = { "Browse", "Auswählen" })
    String pathchooser_browselabel();

    @Default(lngs = { "en", "de" }, values = { "Choose path!", "Pfad auswählen!" })
    String pathchooser_dialog_title();

    @Default(lngs = { "en", "de" }, values = { "Enter a path...", "Bitte Pfad eingeben..." })
    String pathchooser_helptext();

    // @Default(lngs = { "en", "de" }, values = { "Please wait until download has finished", "Bitte warten bis der Download beendet ist" })
    // String please_wait();
    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en", "de" }, values = { "%s3% %s1/%s2", "%s3% %s1/%s2" })
    String progress(@Name("done") @Example("10") String loaded, @Name("total") @Example("100") String total, @Name("percent") @Example("10.0%") double d);

    @Default(lngs = { "en", "de" }, values = { "Direct %s1", "Direkt %s1" })
    String proxy_direct(@Name("IP") @Example("127.0.0.1") String ip);

    @Default(lngs = { "en", "de" }, values = { "%s1:%s2 (Http Proxy)", "%s1:%s2 (Http Proxy)" })
    @DoNotTranslate("Http")
    String proxy_http(@Name("host") @Example("192.168.2.1") String host, @Name("port") @Example("8080") int port);

    @DoNotTranslate("Https")
    @Default(lngs = { "en", "de" }, values = { "%s1:%s2 (Https Proxy)", "%s1:%s2 (Https Proxy)" })
    String proxy_https(@Name("host") @Example("192.168.2.1") String host, @Name("port") @Example("8080") int port);

    @Default(lngs = { "en", "de" }, values = { "Direct", "Direkt" })
    String proxy_none();

    @DoNotTranslate("Socks4")
    @Default(lngs = { "en", "de" }, values = { "%s1:%s2 (Socks4 Proxy)", "%s1:%s2 (Socks4 Proxy)" })
    String proxy_socks4(@Name("host") @Example("192.168.2.1") String host, @Name("port") @Example("1080") int port);

    @DoNotTranslate("Socks4a")
    @Default(lngs = { "en", "de" }, values = { "%s1:%s2 (Socks4a Proxy)", "%s1:%s2 (Socks4a Proxy)" })
    String proxy_socks4a(@Name("host") @Example("192.168.2.1") String host, @Name("port") @Example("1080") int port);

    @DoNotTranslate("Socks5")
    @Default(lngs = { "en", "de" }, values = { "%s1:%s2 (Socks5 Proxy)", "%s1:%s2 (Socks5 Proxy)" })
    String proxy_socks5(@Name("host") @Example("192.168.2.1") String host, @Name("port") @Example("1080") int port);

    @Default(lngs = { "en", "de" }, values = { "Reset Columns", "Spalten zurücksetzen" })
    String ResetColumnsAction();

    @Default(lngs = { "en", "de" }, values = { "Search Table", "Tabelle durchsuchen" })
    String SearchContextAction();

    @Default(lngs = { "en", "de" }, values = { "Find", "Suchen" })
    String SEARCHDIALOG_BUTTON_FIND();

    @Default(lngs = { "en", "de" }, values = { "Case sensitive", "Groß/Kleinschreibung" })
    String SEARCHDIALOG_CHECKBOX_CASESENSITIVE();

    @Default(lngs = { "en", "de" }, values = { "Regular Expressions", "Reguläre Ausrücke" })
    String SEARCHDIALOG_CHECKBOX_REGULAREXPRESSION();

    // @Default(lngs = { "en", "de" }, values = { "Visit our Homepage", "Besuchen Sie unsere Homepage" })
    // String Statusbar_Statusbar_visiturl_tooltip();
    @Default(lngs = { "en", "de" }, values = { "This dialog has a countdown and closes after a few seconds. Click to cancel the countdown", "Dieser Dialog hat einen Countdown und schließt sich in einigen Sekunden. Klicken Sie hier um den Countdown abzubrechen." })
    String TIMERDIALOG_TOOLTIP_TIMERLABEL();

    @Default(lngs = { "en", "de" }, values = { "Look In:", "Suchen in:" })
    String DIALOG_FILECHOOSER_lookInLabelText();

    // FileChooser =Look In:
    @Default(lngs = { "en", "de" }, values = { "Save In:", "Speichern in:" })
    String DIALOG_FILECHOOSER_saveInLabelText();

    // FileChooser =Save In:
    @Default(lngs = { "en", "de" }, values = { "File Name:", "Dateiname:" })
    String DIALOG_FILECHOOSER_fileNameLabelText();

    // FileChooser =File Name:
    @Default(lngs = { "en", "de" }, values = { "Folder name:", "Ordnername:" })
    String DIALOG_FILECHOOSER_folderNameLabelText();

    // FileChooser =Folder name:
    @Default(lngs = { "en", "de" }, values = { "Files of Type:", "Dateityp:" })
    String DIALOG_FILECHOOSER_filesOfTypeLabelText();

    // FileChooser =Up One Level
    @Default(lngs = { "en", "de" }, values = { "Up", "Nach oben" })
    String DIALOG_FILECHOOSER_upFolderAccessibleName();

    // FileChooser =Home
    @Default(lngs = { "en", "de" }, values = { "Home", "Home" })
    String DIALOG_FILECHOOSER_homeFolderAccessibleName();

    // FileChooser =Create New Folder
    @Default(lngs = { "en", "de" }, values = { "New Folder", "Neuer Ordner" })
    String DIALOG_FILECHOOSER_newFolderAccessibleName();

    // FileChooser =List
    @Default(lngs = { "en", "de" }, values = { "List", "Liste" })
    String DIALOG_FILECHOOSER_listViewButtonAccessibleName();

    // FileChooser =Details
    @Default(lngs = { "en", "de" }, values = { "Details", "Details" })
    String DIALOG_FILECHOOSER_detailsViewButtonAccessibleName();

    // FileChooser =Details
    @Default(lngs = { "en", "de" }, values = { "Error creating new folder", "Fehler beim Erstellen eines neuen Ordners" })
    String DIALOG_FILECHOOSER_newFolderErrorText();

    // FileChooser =Error creating new folder
    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en", "de" }, values = { ": ", ": " })
    String DIALOG_FILECHOOSER_newFolderErrorSeparator();

    // FileChooser =:
    @Default(lngs = { "en", "de" }, values = { "Unable to create folder", "Ordner kann nicht erstellt werden" })
    String DIALOG_FILECHOOSER_newFolderParentDoesntExistTitleText();

    // FileChooser =Unable to create folder
    @Default(lngs = { "en", "de" }, values = { "Unable to create the folder.\\r\\n\\r\\nThe system cannot find the path specified.", "Ordner kann nicht erstellt werden.\\r\\n\\r\\n System kann den angegebenen Pfad nicht finden." })
    String DIALOG_FILECHOOSER_newFolderParentDoesntExistText();

    // FileChooser =Unable to create the folder.
    @Default(lngs = { "en", "de" }, values = { "Generic File", "Allgemeine Datei" })
    String DIALOG_FILECHOOSER_fileDescriptionText();

    // FileChooser =Generic File
    @Default(lngs = { "en", "de" }, values = { "Directory", "Verzeichnis" })
    String DIALOG_FILECHOOSER_directoryDescriptionText();

    // FileChooser =Directory
    @Default(lngs = { "en", "de" }, values = { "Save", "Speichern" })
    String DIALOG_FILECHOOSER_saveButtonText();

    // FileChooser =Save
    @Default(lngs = { "en", "de" }, values = { "Open", "Öffnen" })
    String DIALOG_FILECHOOSER_openButtonText();

    // FileChooser =Open
    @Default(lngs = { "en", "de" }, values = { "Save", "Speichern" })
    String DIALOG_FILECHOOSER_saveDialogTitleText();

    // FileChooser =Save
    @Default(lngs = { "en", "de" }, values = { "Open", "Öffnen" })
    String DIALOG_FILECHOOSER_openDialogTitleText();

    // FileChooser =Open
    @Default(lngs = { "en", "de" }, values = { "Cancel", "Abbrechen" })
    String DIALOG_FILECHOOSER_cancelButtonText();

    // FileChooser =Cancel
    @Default(lngs = { "en", "de" }, values = { "Update", "Aktualisieren" })
    String DIALOG_FILECHOOSER_updateButtonText();

    // FileChooser =Update
    @Default(lngs = { "en", "de" }, values = { "Help", "Hilfe" })
    String DIALOG_FILECHOOSER_helpButtonText();

    // FileChooser =Help
    @Default(lngs = { "en", "de" }, values = { "Open", "Öffnen" })
    String DIALOG_FILECHOOSER_directoryOpenButtonText();

    // FileChooser =Open
    @Default(lngs = { "en", "de" }, values = { "Save selected file", "Ausgewählte Datei speichern" })
    String DIALOG_FILECHOOSER_saveButtonToolTipText();

    // FileChooser =Save selected file
    @Default(lngs = { "en", "de" }, values = { "Open selected file", "Ausgewählte Datei öffnen" })
    String DIALOG_FILECHOOSER_openButtonToolTipText();

    // FileChooser =Open selected file
    @Default(lngs = { "en", "de" }, values = { "Abort file chooser dialog", "Dialogfeld für Dateiauswahl schließen" })
    String DIALOG_FILECHOOSER_cancelButtonToolTipText();

    // FileChooser =Abort file chooser dialog
    @Default(lngs = { "en", "de" }, values = { "Update directory listing", "Verzeichnisliste aktualisieren" })
    String DIALOG_FILECHOOSER_updateButtonToolTipText();

    // FileChooser =Update directory listing
    @Default(lngs = { "en", "de" }, values = { "FileChooser help", "FileChooser-Hilfe" })
    String DIALOG_FILECHOOSER_helpButtonToolTipText();

    // FileChooser =FileChooser help
    @Default(lngs = { "en", "de" }, values = { "Open selected directory", "Ausgewähltes Verzeichnis öffnen" })
    String DIALOG_FILECHOOSER_directoryOpenButtonToolTipText();

    // FileChooser =Open selected directory
    @Default(lngs = { "en", "de" }, values = { "Network", "Netzwerk" })
    String DIALOG_FILECHOOSER_networkfolder();

    @Default(lngs = { "en", "de" }, values = { "No Connection to the Internet", "Keine Internetverbindung" })
    String proxydialog_title();

    @Default(lngs = { "en", "de" }, values = { "Save", "Speichern" })
    String lit_save();

    @Default(lngs = { "en" }, values = { "Host:Port" })
    String ProxyDialog_hostport();

    // @DoNotTranslate("HTTP")
    // @Default(lngs = { "en" }, values = { "HTTP" })
    // String ProxyDialog_http();
    @Default(lngs = { "en" }, values = { "Password" })
    String ProxyDialog_password();

    // @DoNotTranslate("Socks5")
    // @Default(lngs = { "en" }, values = { "Socks5" })
    // String ProxyDialog_socks5();
    //
    // @DoNotTranslate("Socks4")
    // @Default(lngs = { "en" }, values = { "Socks4" })
    // String ProxyDialog_socks4();
    //
    // @Default(lngs = { "en" }, values = { "Direct" })
    // String ProxyDialog_direct();
    @Default(lngs = { "en" }, values = { "Type" })
    String ProxyDialog_type();

    @Default(lngs = { "en" }, values = { "Username" })
    String ProxyDialog_username();

    @Default(lngs = { "en" }, values = { "dd.MM.yy HH:mm" })
    @DoNotTranslate("dd.MM.yy HH:mm")
    @Warning("This value musst be in Java SimpleDateFormat. If you translate, ensure compatibility!")
    String extdatecolumn_dateandtimeformat();

    @Default(lngs = { "en" }, values = { "Authorization required" })
    String ProxyDialog_requires_auth();

    @Default(lngs = { "en" }, values = { "Enter hostname or IP address..." })
    String ProxyDialog_hostport_help();

    @Default(lngs = { "en" }, values = { "Enter username..." })
    String ProxyDialog_username_help();

    @Default(lngs = { "en" }, values = { "Enter password..." })
    String ProxyDialog_password_help();

    @Default(lngs = { "en" }, values = { "Continue" })
    String lit_continue();

    @Warning("Do not auto-translate - might result in errors")
    @Default(lngs = { "en" }, values = { "%s1" })
    String tableheader_tooltip_normal(@Name("Column_Name") String name);

    @Default(lngs = { "en" }, values = { "%s1 (Resizing Locked)" })
    String tableheader_tooltip_locked(@Name("Column_Name") String name);

    @Default(lngs = { "en", "de" }, values = { "Column width locked", "Spaltenbreite verankert" })
    String unLockColumnWidthAction2();

    @Default(lngs = { "en" }, values = { "Remember" })
    String proxydialog_remember();

    // @Default(lngs = { "en", "de" }, values = { "Yes", "Ja" })
    // String lit_yes();
    //
    // @Default(lngs = { "en", "de" }, values = { "System Default", "Systemproxy" })
    // String ProxyDialog_auto();
    @Default(lngs = { "en", "de" }, values = { "Close", "Schließen" })
    String lit_close();

    @Default(lngs = { "en", "de" }, values = { "and", "und" })
    String lit_and();

    @Default(lngs = { "en", "de" }, values = { "Debug- & Developer Menu", "Debug- & Entwicklermenü" })
    String debug_menu();

    @Default(lngs = { "en", "de" }, values = { "Direct Gateway Connection", "Direkte Gateway Verbindung" })
    String proxy_type_direct();

    @Default(lngs = { "en", "de" }, values = { "Direct Connection", "Direkte Verbindung" })
    String proxy_type_none();

    @DoNotTranslate("HTTPS(SSL)")
    @Default(lngs = { "en", "de" }, values = { "HTTPS(SSL) Proxy", "HTTPS(SSL) Proxy" })
    String proxy_type_https();

    @Default(lngs = { "en", "de" }, values = { "HTTP Proxy", "HTTP Proxy" })
    @DoNotTranslate("HTTP")
    String proxy_type_http();

    @Default(lngs = { "en", "de" }, values = { "Socks5 Proxy", "Socks5 Proxy" })
    @DoNotTranslate("Socks5")
    String proxy_type_socks5();

    @Default(lngs = { "en", "de" }, values = { "Socks4 Proxy", "Socks4 Proxy" })
    @DoNotTranslate("Socks4")
    String proxy_type_socks4();

    @Default(lngs = { "en", "de" }, values = { "Socks4a Proxy", "Socks4a Proxy" })
    @DoNotTranslate("Socks4a")
    String proxy_type_socks4a();

    // @Default(lngs = { "en", "de" }, values = { "HTTP", "Socks4 Proxy" })
    // String ProxyDialog_https();
    @DoNotTranslate("PAC")
    @Default(lngs = { "en", "de" }, values = { "Auto-detect Proxy (Proxy PAC Script)", "Automatische Erkennung (z.B. Proxy PAC Script)" })
    String proxy_type_auto_detect();

    @Default(lngs = { "en", "de" }, values = { "Port", "Port" })
    String ProxyDialog_port_help();

    // Stunden werden als h abgekürzt im Deutschen "Das lateinische Wort ist hora, daher das Einheitenzeichen h"
    @Default(lngs = { "en", "de" }, values = { "h", "h" })
    @Warning("Do not auto-translate - might result in errors")
    String Timeformater_short_hours();

    @Default(lngs = { "en", "de" }, values = { "m", "m" })
    @Warning("Do not auto-translate - might result in errors")
    String Timeformater_short_minute();

    @Default(lngs = { "en", "de" }, values = { "s", "s" })
    @Warning("Do not auto-translate - might result in errors")
    String Timeformater_short_seconds();

    @Default(lngs = { "en", "de" }, values = { "millisecond", "Millisekunde" })
    String lit_millisecond();

    @Default(lngs = { "en", "de" }, values = { "milliseconds", "Millisekunden" })
    String lit_milliseconds();

    @Default(lngs = { "en", "de" }, values = { "second", "Sekunde" })
    String lit_second();

    @Default(lngs = { "en", "de" }, values = { "seconds", "Sekunden" })
    String lit_seconds();

    @Default(lngs = { "en", "de" }, values = { "minute", "Minute" })
    String lit_minute();

    @Default(lngs = { "en", "de" }, values = { "minutes", "Minuten" })
    String lit_minutes();

    @Default(lngs = { "en", "de" }, values = { "years", "Jahre" })
    String lit_years();

    @Default(lngs = { "en", "de" }, values = { "year", "Jahr" })
    String lit_year();

    @Default(lngs = { "en", "de" }, values = { "months", "Monate" })
    String lit_months();

    @Default(lngs = { "en", "de" }, values = { "month", "Monat" })
    String lit_month();

    @Default(lngs = { "en", "de" }, values = { "weeks", "Wochen" })
    String lit_weeks();

    @Default(lngs = { "en", "de" }, values = { "week", "Woche" })
    String lit_week();

    @Default(lngs = { "en", "de" }, values = { "day", "Tag" })
    String lit_day();

    @Default(lngs = { "en", "de" }, values = { "days", "Tage" })
    String lit_days();

    @Default(lngs = { "en", "de" }, values = { "hours", "Stunden" })
    String lit_hours();

    @Default(lngs = { "en", "de" }, values = { "hour", "Stunde" })
    String lit_hour();

    @Default(lngs = { "en", "de" }, values = { "Yes", "Ja" })
    String lit_yes_upper();

    @Default(lngs = { "en", "de" }, values = { "No", "Nein" })
    String lit_no_upper();
}
