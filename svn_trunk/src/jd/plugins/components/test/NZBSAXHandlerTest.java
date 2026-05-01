package jd.plugins.components.test;

import jd.nutils.encoding.Encoding;
import jd.plugins.components.NZBSAXHandler;

import org.appwork.testframework.AWTest;

public class NZBSAXHandlerTest extends AWTest {

    private String decodeXML(final String input) {
        return Encoding.htmlOnlyDecode(input);
    }

    @Override
    public void runTest() throws Exception {
        String subject = "[1/1] - &quot;Big Buck Bunny - S01E01.mkv&quot; yEnc (1/2) 1478616";
        assertEquals(NZBSAXHandler.parseFilenameFromSubject(decodeXML(subject)), "Big Buck Bunny - S01E01.mkv");
        assertEquals(NZBSAXHandler.parseFilesizeFromYEncSubject(decodeXML(subject)), "1478616");

        subject = "[1/5] - &quot;Big Buck Bunny - S01E01.mkv&quot; yEnc (1/24) 16981056";
        assertEquals(NZBSAXHandler.parseFilenameFromSubject(decodeXML(subject)), "Big Buck Bunny - S01E01.mkv");
        assertEquals(NZBSAXHandler.parseFilesizeFromYEncSubject(decodeXML(subject)), "16981056");

        subject = "[4/5] - &quot;Big Buck Bunny - S01E01.mkv.vol01+02.par2&quot; yEnc (1/3) 1434656";
        assertEquals(NZBSAXHandler.parseFilenameFromSubject(decodeXML(subject)), "Big Buck Bunny - S01E01.mkv.vol01+02.par2");
        assertEquals(NZBSAXHandler.parseFilesizeFromYEncSubject(decodeXML(subject)), "1434656");

        subject = "Here's your file!  abc-mr2a.r01 (1/2)";
        assertEquals(NZBSAXHandler.parseFilenameFromSubject(decodeXML(subject)), "abc-mr2a.r01");
        subject = "(Test) [from Fester Bestertester] - \"test.txt\" yEnc (1/1)";
        assertEquals(NZBSAXHandler.parseFilenameFromSubject(decodeXML(subject)), "test.txt");
        subject = "&quot;ubuntu-12.04-desktop-i386.par2&quot; yEnc (1/1)";
        assertEquals(NZBSAXHandler.parseFilenameFromSubject(decodeXML(subject)), "ubuntu-12.04-desktop-i386.par2");
        subject = "20155f68b0df03cc9bca2c58d426163ff [2/15] - &#34;20155f68b0df03cc9bca2c58d426163ff&#34; yEnc (62/68)";
        assertEquals(NZBSAXHandler.parseFilenameFromSubject(decodeXML(subject)), "20155f68b0df03cc9bca2c58d426163ff");
    }

    public static void main(String[] args) {
        run();
    }

}
