package org.appwork.io.streams.signature;

import java.io.IOException;

/**
 * The stream ended without a endsignature, but with a normal(valid) signature instead. This means, that the stream is not complete, but was
 * closed anyway
 *
 * @author thomas
 * @date 11.10.2021
 *
 */
public class StreamEndedUnexpectedException extends IOException {
}
