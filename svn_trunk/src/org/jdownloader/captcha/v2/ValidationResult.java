package org.jdownloader.captcha.v2;

import org.appwork.storage.config.annotations.LabelInterface;
import org.jdownloader.gui.translate._GUI;

public enum ValidationResult implements LabelInterface {
    /* Generic "invalid" without a more detailed reason, e.g. reported by the website after the answer was submitted. */
    INVALID {
        @Override
        public String getLabel() {
            return _GUI.T.ValidationResult_INVALID();
        }
    },
    VALID {
        @Override
        public String getLabel() {
            return _GUI.T.ValidationResult_VALID();
        }
    },
    UNUSED {
        @Override
        public String getLabel() {
            return _GUI.T.ValidationResult_UNUSED();
        }
    },
    /* Detailed reasons why an answer was rejected during local validation (all of them count as INVALID). */
    EMPTY_RESPONSE {
        @Override
        public String getLabel() {
            return _GUI.T.ValidationResult_EMPTY_RESPONSE();
        }
    },
    INVALID_TOKEN {
        @Override
        public String getLabel() {
            return _GUI.T.ValidationResult_INVALID_TOKEN();
        }
    },
    NOT_NUMERIC {
        @Override
        public String getLabel() {
            return _GUI.T.ValidationResult_NOT_NUMERIC();
        }
    },
    COORDINATES_MISMATCH {
        @Override
        public String getLabel() {
            return _GUI.T.ValidationResult_COORDINATES_MISMATCH();
        }
    },
    TOO_FEW_CLICKS {
        @Override
        public String getLabel() {
            return _GUI.T.ValidationResult_TOO_FEW_CLICKS();
        }
    },
    TOO_MANY_CLICKS {
        @Override
        public String getLabel() {
            return _GUI.T.ValidationResult_TOO_MANY_CLICKS();
        }
    },
    WRONG_CLICK_COUNT {
        @Override
        public String getLabel() {
            return _GUI.T.ValidationResult_WRONG_CLICK_COUNT();
        }
    };
}
