package org.jdownloader.captcha.v2.challenge.multiclickcaptcha;

import org.appwork.storage.Storable;

public class MultiClickedPoint implements Storable {
    private int[] x;
    private int[] y;

    public MultiClickedPoint(/* storable */) {
    }

    public MultiClickedPoint(int[] x, int[] y) {
        if (x == null || y == null) {
            throw new IllegalArgumentException();
        } else if (x.length == 0 || x.length == 0) {
            throw new IllegalArgumentException();
        } else if (x.length != y.length) {
            throw new IllegalArgumentException();
        }
        this.x = x;
        this.y = y;
    }

    public int[] getX() {
        return x;
    }

    public void setX(int[] x) {
        this.x = x;
    }

    public int[] getY() {
        return y;
    }

    public void setY(int[] y) {
        this.y = y;
    }
}
