package org.jdownloader.captcha.v2.challenge.oauth;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import jd.gui.swing.dialog.DialogType;
import net.miginfocom.swing.MigLayout;

import org.appwork.swing.action.BasicAction;
import org.appwork.uio.ConfirmDialogInterface;
import org.appwork.uio.InputDialogInterface;
import org.appwork.uio.UIOManager;
import org.appwork.utils.os.CrossSystem;
import org.appwork.utils.swing.SwingUtils;
import org.appwork.utils.swing.dialog.InputDialog;
import org.jdownloader.DomainInfo;
import org.jdownloader.captcha.v2.AbstractCaptchaDialog;
import org.jdownloader.captcha.v2.AbstractResponse;
import org.jdownloader.captcha.v2.ChallengeSolver;
import org.jdownloader.captcha.v2.solverjob.ChallengeSolverJobListener;
import org.jdownloader.gui.translate._GUI;

public class OAuthDialog extends AbstractCaptchaDialog<Boolean> implements ActionListener, ChallengeSolverJobListener, ConfirmDialogInterface {

    private JPanel p;

    public OAuthDialog(int flag, DialogType type, DomainInfo domain, OAuthChallenge challenge) {
        super(challenge, flag | UIOManager.BUTTONS_HIDE_OK, _GUI.T.OAUTH_DIALOG_TITLE(domain.getTld()), type, domain, null);

        challenge.getJob().getEventSender().addListener(this, true);
        setLeftActions(new BasicAction() {
            {
                setName(_GUI.T.lit_open_browser());
            }

            @Override
            public void actionPerformed(ActionEvent e) {
                openBrowser();
            }
        });

    }

    protected void openBrowser() {
        final String url = ((OAuthChallenge) challenge).getUrl();
        if (CrossSystem.openURL(url) != null || true) {
            new Thread() {
                {
                    setDaemon(true);
                }

                @Override
                public void run() {
                    final InputDialog oauthDialog = new InputDialog(UIOManager.LOGIC_COUNTDOWN, _GUI.T.lit_open_browser(), challenge.getExplain(), url, null, _GUI.T.lit_continue(), null);
                    oauthDialog.setTimeout(5 * 60 * 1000);
                    UIOManager.I().show(InputDialogInterface.class, oauthDialog);
                }
            }.start();
        }

    }

    protected int getPreferredHeight() {
        return -1;
    }

    @Override
    protected int getPreferredWidth() {
        return -1;
    }

    @Override
    protected boolean isResizable() {
        return false;
    }

    @Override
    protected Boolean createReturnValue() {
        return false;
    }

    @Override
    public JComponent layoutDialogContent() {
        dialog.addWindowListener(new WindowListener() {

            @Override
            public void windowOpened(WindowEvent e) {
                openBrowser();
            }

            @Override
            public void windowIconified(WindowEvent e) {
            }

            @Override
            public void windowDeiconified(WindowEvent e) {
            }

            @Override
            public void windowDeactivated(WindowEvent e) {
            }

            @Override
            public void windowClosing(WindowEvent e) {
            }

            @Override
            public void windowClosed(WindowEvent e) {
            }

            @Override
            public void windowActivated(WindowEvent e) {
            }
        });
        p = new JPanel(new MigLayout("ins 0,wrap 1", "[grow,fill]", "[][grow,fill]"));
        JLabel lbl;
        p.add(lbl = new JLabel("<html>" + challenge.getExplain().replace("\r\n", "<br>") + "</html>"));
        SwingUtils.setOpaque(lbl, false);
        return p;
    }

    @Override
    public void onSolverJobReceivedNewResponse(AbstractResponse<?> response) {
        dispose();
    }

    @Override
    public void onSolverDone(ChallengeSolver<?> solver) {

    }

    @Override
    public void onSolverStarts(ChallengeSolver<?> parameter) {
    }

    @Override
    public void onSolverTimedOut(ChallengeSolver<?> parameter) {

    }

    @Override
    public String getMessage() {
        final String url = ((OAuthChallenge) challenge).getUrl();
        return challenge.getExplain() + "\r\n" + url;
    }

    @Override
    protected JPanel createCaptchaPanel() {
        return null;
    }

}