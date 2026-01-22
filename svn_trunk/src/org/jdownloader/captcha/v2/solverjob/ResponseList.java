package org.jdownloader.captcha.v2.solverjob;

import java.util.ArrayList;

import org.appwork.utils.CompareUtils;
import org.jdownloader.captcha.v2.AbstractResponse;

public class ResponseList<E> extends ArrayList<AbstractResponse<E>> implements Comparable<ResponseList<E>> {
    private static final long serialVersionUID = 1L;
    private Integer           trustlevelPercentageSum              = null;

    public boolean add(AbstractResponse<E> e) {
        if (e.getTrustLevel() != null) {
            if (trustlevelPercentageSum == null) {
                trustlevelPercentageSum = 0;
            }
            trustlevelPercentageSum += e.getTrustLevel();
        }
        return super.add(e);
    }

    public Integer getTrustlevelPercentageSum() {
        return trustlevelPercentageSum;
    }

    @Override
    public int compareTo(ResponseList<E> o) {
        return CompareUtils.compareInt(o.trustlevelPercentageSum, trustlevelPercentageSum);
    }

    public E getValue() {
        return get(0).getValue();
    }
}
