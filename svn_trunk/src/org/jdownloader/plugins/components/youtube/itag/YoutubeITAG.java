package org.jdownloader.plugins.components.youtube.itag;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.appwork.utils.parser.UrlQuery;

public enum YoutubeITAG {
    /* https://gist.github.com/MartinEesmaa/2f4b261cb90a47e9c41ba115a011a4aa */
    DASH_VIDEO_144p_AV1(394, StreamContainer.DASH_VIDEO, VideoResolution.P_144, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_144p_AV1_HIGHER_BITRATE(694, StreamContainer.DASH_VIDEO, VideoResolution.P_144, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_240p_AV1(395, StreamContainer.DASH_VIDEO, VideoResolution.P_240, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_240p_AV1_HIGHER_BITRATE(695, StreamContainer.DASH_VIDEO, VideoResolution.P_240, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_360p_AV1(396, StreamContainer.DASH_VIDEO, VideoResolution.P_360, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_360p_AV1_HIGHER_BITRATE(696, StreamContainer.DASH_VIDEO, VideoResolution.P_360, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_480p_AV1(397, StreamContainer.DASH_VIDEO, VideoResolution.P_480, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_480p_AV1_HIGHER_BITRATE(697, StreamContainer.DASH_VIDEO, VideoResolution.P_480, VideoCodec.AV1, VideoFrameRate.FPS_30),
    // check again, maybe FPS_60?
    DASH_VIDEO_720p_AV1(398, StreamContainer.DASH_VIDEO, VideoResolution.P_720, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_720p_AV1_HIGHER_BITRATE(698, StreamContainer.DASH_VIDEO, VideoResolution.P_720, VideoCodec.AV1, VideoFrameRate.FPS_30),
    // check again,maybe FPS_60?
    DASH_VIDEO_1080p_AV1(399, StreamContainer.DASH_VIDEO, VideoResolution.P_1080, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_1080p_AV1_HIGHER_BITRATE(699, StreamContainer.DASH_VIDEO, VideoResolution.P_1080, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_1440p_AV1(400, StreamContainer.DASH_VIDEO, VideoResolution.P_1440, VideoCodec.AV1, VideoFrameRate.FPS_60),
    DASH_VIDEO_1440p_AV1_HIGHER_BITRATE(700, StreamContainer.DASH_VIDEO, VideoResolution.P_1440, VideoCodec.AV1, VideoFrameRate.FPS_60),
    DASH_VIDEO_2160p_AV1(401, StreamContainer.DASH_VIDEO, VideoResolution.P_2160, VideoCodec.AV1, VideoFrameRate.FPS_60),
    DASH_VIDEO_2160p_AV1_HIGHER_BITRATE(701, StreamContainer.DASH_VIDEO, VideoResolution.P_2160, VideoCodec.AV1, VideoFrameRate.FPS_60),
    DASH_VIDEO_4320p_AV1(402, StreamContainer.DASH_VIDEO, VideoResolution.P_4320, VideoCodec.AV1, VideoFrameRate.FPS_60),
    DASH_VIDEO_4320p_AV1_HIGHER_BITRATE(571, StreamContainer.DASH_VIDEO, VideoResolution.P_4320, VideoCodec.AV1, VideoFrameRate.FPS_60),
    DASH_VIDEO_4320p_AV1_HFR_HIGH(702, StreamContainer.DASH_VIDEO, VideoResolution.P_4320, VideoCodec.AV1, VideoFrameRate.FPS_60),
    // Extra video formats of 608p,https://gist.github.com/MartinEesmaa/2f4b261cb90a47e9c41ba115a011a4aa
    DASH_VIDEO_1080x608_AV1(788, StreamContainer.DASH_VIDEO, VideoResolution.P_1080_608, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_608x1080_AV1(788, StreamContainer.DASH_VIDEO, VideoResolution.P_608_1080, VideoCodec.AV1, VideoFrameRate.FPS_30),
    DASH_VIDEO_1080x608_VP9_EFFICIENCY(779, StreamContainer.DASH_VIDEO, VideoResolution.P_1080_608, VideoCodec.VP9, VideoFrameRate.FPS_30),
    DASH_VIDEO_608x1080_VP9_EFFICIENCY(779, StreamContainer.DASH_VIDEO, VideoResolution.P_608_1080, VideoCodec.VP9, VideoFrameRate.FPS_30),
    DASH_VIDEO_1080x608_VP9_BEST(780, StreamContainer.DASH_VIDEO, VideoResolution.P_1080_608, VideoCodec.VP9, VideoFrameRate.FPS_30),
    DASH_VIDEO_608x1080_VP9_BEST(780, StreamContainer.DASH_VIDEO, VideoResolution.P_608_1080, VideoCodec.VP9, VideoFrameRate.FPS_30),

    // AAC (LC) 128 Kbps Stereo (2) Yes, YT Music
    DASH_AUDIO_128K_AAC(140, StreamContainer.DASH_AUDIO, AudioCodec.AAC, AudioBitrate.KBIT_128),
    // DASH_AUDIO_48K_OPUS(249, null, null, "Opus", "38kbit", YoutubeITAG.OPUS_48),
    DASH_AUDIO_128K_WEBM(171, StreamContainer.DASH_AUDIO, AudioCodec.VORBIS, AudioBitrate.KBIT_128),
    DASH_AUDIO_192K_WEBM(172, StreamContainer.DASH_AUDIO, AudioCodec.VORBIS, AudioBitrate.KBIT_192),
    DASH_AUDIO_256K_WEBM_SPATIAL(339, StreamContainer.DASH_AUDIO, AudioCodec.VORBIS_SPATIAL, AudioBitrate.KBIT_256),
    // DTSE (DTS Express) 384 Kbps Surround (5.1) Rarely*
    DASH_AUDIO_384K_DTSE_SPATIAL(325, StreamContainer.DASH_AUDIO, AudioCodec.DTSE_SPATIAL, AudioBitrate.KBIT_384),
    // EAC3 384 Kbps Surround (5.1) Rarely
    DASH_AUDIO_384K_EC3_SPATIAL(328, StreamContainer.DASH_AUDIO, AudioCodec.EC3_SPATIAL, AudioBitrate.KBIT_384),
    // AC3 384 Kbps Surround (5.1) Rarely
    DASH_AUDIO_384K_AC3_SPATIAL(380, StreamContainer.DASH_AUDIO, AudioCodec.AC3_SPATIAL, AudioBitrate.KBIT_384),
    // AAC (LC) 256 Kbps Surround (5.1) ?*
    DASH_AUDIO_256K_AAC_SPATIAL(327, StreamContainer.DASH_AUDIO, AudioCodec.AAC_SPATIAL, AudioBitrate.KBIT_256),
    // AAC (HE v1) 192 Kbps Surround (5.1) Rarely
    DASH_AUDIO_192K_AAC_SPATIAL(256, StreamContainer.DASH_AUDIO, AudioCodec.AAC_SPATIAL, AudioBitrate.KBIT_192),
    // AAC (LC) 384 Kbps Surround (5.1) Rarely
    DASH_AUDIO_384K_AAC_SPATIAL(258, StreamContainer.DASH_AUDIO, AudioCodec.AAC_SPATIAL, AudioBitrate.KBIT_384),
    // AAC (LC) 256 Kbps Stereo (2) No, YT Music*
    DASH_AUDIO_256K_AAC(141, StreamContainer.DASH_AUDIO, AudioCodec.AAC, AudioBitrate.KBIT_256),
    // AAC (HE v1) 48 Kbps Stereo (2) Yes, YT Music
    DASH_AUDIO_48K_AAC(139, StreamContainer.DASH_AUDIO, AudioCodec.AAC, AudioBitrate.KBIT_48),
    // AAC (HE v1) 30 Kbps Stereo (2) Yes
    DASH_AUDIO_32K_AAC(599, StreamContainer.DASH_AUDIO, AudioCodec.AAC, AudioBitrate.KBIT_32),
    // Opus (VBR) ~480 Kbps (?) Quadraphonic (4) ?*
    DASH_AUDIO_512K_OPUS_SPATIAL(338, StreamContainer.DASH_AUDIO, AudioCodec.OPUS_SPATIAL, AudioBitrate.KBIT_512),
    // Opus (VBR) ~256 Kbps Stereo (2) Some, YT Music*
    DASH_AUDIO_OPUS_256KBIT(774, StreamContainer.DASH_AUDIO, AudioCodec.OPUS, AudioBitrate.KBIT_256),
    // Opus (VBR) <=160 Kbps Stereo (2) Yes
    DASH_AUDIO_OPUS_160KBIT(251, StreamContainer.DASH_AUDIO, AudioCodec.OPUS, AudioBitrate.KBIT_160),
    // Opus (VBR) ~70 Kbps Stereo (2) Yes
    DASH_AUDIO_OPUS_64KBIT(250, StreamContainer.DASH_AUDIO, AudioCodec.OPUS, AudioBitrate.KBIT_64),
    // Opus (VBR) ~50 Kbps Stereo (2) Yes
    DASH_AUDIO_OPUS_48KBIT(249, StreamContainer.DASH_AUDIO, AudioCodec.OPUS, AudioBitrate.KBIT_48),
    // Opus (VBR) ~35 Kbps Stereo (2) Yes
    DASH_AUDIO_OPUS_32KBIT(600, StreamContainer.DASH_AUDIO, AudioCodec.OPUS, AudioBitrate.KBIT_32),
    DASH_VIDEO_1080_H264_FPS60(299, StreamContainer.DASH_VIDEO, VideoResolution.P_1080, VideoCodec.H264, VideoFrameRate.FPS_60),
    DASH_VIDEO_1080P_H264(137, StreamContainer.DASH_VIDEO, VideoResolution.P_1080, VideoCodec.H264, VideoFrameRate.FPS_30),
    // http://www.youtube.com/watch?v=gBabKoHSErI
    DASH_VIDEO_1440P_H264(264, StreamContainer.DASH_VIDEO, VideoResolution.P_1440, VideoCodec.H264, VideoFrameRate.FPS_30),
    DASH_VIDEO_144P_H264(160, StreamContainer.DASH_VIDEO, VideoResolution.P_144, VideoCodec.H264, VideoFrameRate.FPS_30),
    DASH_VIDEO_144P_H264_FPS15(597, StreamContainer.DASH_VIDEO, VideoResolution.P_144, VideoCodec.H264, VideoFrameRate.FPS_15),
    // Mobile/Portrait Format https://www.youtube.com/watch?v=kiZse2vZXfw
    DASH_VIDEO_1920_H264_FPS_60(266, StreamContainer.DASH_VIDEO, VideoResolution.P_1920, VideoCodec.H264, VideoFrameRate.FPS_60),
    DASH_VIDEO_2160_H264_FPS_60(266, StreamContainer.DASH_VIDEO, VideoResolution.P_2160, VideoCodec.H264, VideoFrameRate.FPS_60),
    DASH_VIDEO_2160_H264(266, StreamContainer.DASH_VIDEO, VideoResolution.P_2160, VideoCodec.H264, VideoFrameRate.FPS_30),
    DASH_VIDEO_1920_H264(266, StreamContainer.DASH_VIDEO, VideoResolution.P_1920, VideoCodec.H264, VideoFrameRate.FPS_30),
    DASH_VIDEO_240P_H264(133, StreamContainer.DASH_VIDEO, VideoResolution.P_240, VideoCodec.H264, VideoFrameRate.FPS_30),
    DASH_VIDEO_360P_H264(134, StreamContainer.DASH_VIDEO, VideoResolution.P_360, VideoCodec.H264, VideoFrameRate.FPS_30),
    DASH_VIDEO_480P_H264(135, StreamContainer.DASH_VIDEO, VideoResolution.P_480, VideoCodec.H264, VideoFrameRate.FPS_30),
    /*
     *
     *
     * Video ID : 1 Format : AVC Format/Info : Advanced Video Codec Format profile : Main@L3 Format settings, CABAC : Yes Format settings,
     * ReFrames : 3 frames Codec ID : avc1 Codec ID/Info : Advanced Video Coding Duration : 6s 773ms Bit rate : 1 063 Kbps Width : 720
     * pixels Height : 480 pixels Display aspect ratio : 3:2 Frame rate mode : Variable Frame rate : 29.970 fps Minimum frame rate : 29.970
     * fps Maximum frame rate : 30.364 fps Standard : NTSC Color space : YUV Chroma subsampling : 4:2:0 Bit depth : 8 bits Scan type :
     * Progressive Bits/(Pixel*Frame) : 0.103 Stream size : 879 KiB (100%) Encoded date : UTC 2013-02-23 01:52:16 Tagged date : UTC
     * 2013-02-23 01:52:16
     */
    DASH_VIDEO_480P_H264_2(212, StreamContainer.DASH_VIDEO, VideoResolution.P_480, VideoCodec.H264, VideoFrameRate.FPS_30),
    DASH_VIDEO_720_H264_FPS60(298, StreamContainer.DASH_VIDEO, VideoResolution.P_720, VideoCodec.H264, VideoFrameRate.FPS_60),
    DASH_VIDEO_720P_H264(136, StreamContainer.DASH_VIDEO, VideoResolution.P_720, VideoCodec.H264, VideoFrameRate.FPS_30),
    DASH_VIDEO_ITAG304_MP4_1280P_60FPS(304, StreamContainer.DASH_VIDEO, VideoResolution.P_1280, VideoCodec.H264, VideoFrameRate.FPS_60),
    DASH_VIDEO_ITAG305_MP4_1280P_60FPS(305, StreamContainer.DASH_VIDEO, VideoResolution.P_1920, VideoCodec.H264, VideoFrameRate.FPS_60),
    DASH_VIDEO_ITAG308_VP9_1440P_60FPS(308, StreamContainer.DASH_VIDEO, VideoResolution.P_1440, VideoCodec.VP9, VideoFrameRate.FPS_60),
    // https://www.youtube.com/watch?v=kiZse2vZXfw&nohtml5=False
    DASH_VIDEO_ITAG313_VP9_1920P_30FPS(313, StreamContainer.DASH_VIDEO, VideoResolution.P_1920, VideoCodec.VP9_WORSE_PROFILE_1, VideoFrameRate.FPS_30),
    DASH_VIDEO_ITAG313_VP9_2160P_30FPS(313, StreamContainer.DASH_VIDEO, VideoResolution.P_2160, VideoCodec.VP9_WORSE_PROFILE_1, VideoFrameRate.FPS_30),
    // Handy/Portrait Format https://www.youtube.com/watch?v=kiZse2vZXfw
    DASH_VIDEO_ITAG315_VP9_1920P_60FPS(315, StreamContainer.DASH_VIDEO, VideoResolution.P_1920, VideoCodec.VP9, VideoFrameRate.FPS_60),
    DASH_VIDEO_ITAG315_VP9_2160P_60FPS(315, StreamContainer.DASH_VIDEO, VideoResolution.P_2160, VideoCodec.VP9, VideoFrameRate.FPS_60),
    DASH_VIDEO_VP9_1080P_PREMIUM(356, StreamContainer.DASH_VIDEO, VideoResolution.P_1080, VideoCodec.VP9, VideoFrameRate.FPS_30),
    // HDR
    // bitrate 20mb
    DASH_VIDEO_VP9_2160P_60FPS(337, StreamContainer.DASH_VIDEO, VideoResolution.P_2160, VideoCodec.VP9_HDR, VideoFrameRate.FPS_60),
    // br 11166389
    DASH_VIDEO_VP9_1440P_60FPS(336, StreamContainer.DASH_VIDEO, VideoResolution.P_1440, VideoCodec.VP9_HDR, VideoFrameRate.FPS_60),
    // br 3201533
    DASH_VIDEO_VP9_1080P_60FPS(335, StreamContainer.DASH_VIDEO, VideoResolution.P_1080, VideoCodec.VP9_HDR, VideoFrameRate.FPS_60),
    // br 1991504
    DASH_VIDEO_VP9_720P_60FPS(334, StreamContainer.DASH_VIDEO, VideoResolution.P_720, VideoCodec.VP9_HDR, VideoFrameRate.FPS_60),
    // br 909472
    DASH_VIDEO_VP9_480P_60FPS(333, StreamContainer.DASH_VIDEO, VideoResolution.P_480, VideoCodec.VP9_HDR, VideoFrameRate.FPS_60),
    DASH_VIDEO_VP9_360P_60FPS(332, StreamContainer.DASH_VIDEO, VideoResolution.P_360, VideoCodec.VP9_HDR, VideoFrameRate.FPS_60),
    // br 256302
    DASH_VIDEO_VP9_240P_60FPS(331, StreamContainer.DASH_VIDEO, VideoResolution.P_240, VideoCodec.VP9_HDR, VideoFrameRate.FPS_60),
    // br 156637
    DASH_VIDEO_VP9_144P_60FPS(330, StreamContainer.DASH_VIDEO, VideoResolution.P_144, VideoCodec.VP9_HDR, VideoFrameRate.FPS_60),
    // has usually a lower quality than DASH_VIDEO_2160_H264_FPS_60
    DASH_VIDEO_ORIGINAL_H264_GENERIC_4K(138, StreamContainer.DASH_VIDEO, VideoResolution.P_2160, VideoCodec.H264, VideoFrameRate.FPS_30),
    DASH_VIDEO_ORIGINAL_H264_GENERIC_1080P(138, StreamContainer.DASH_VIDEO, VideoResolution.P_1080, VideoCodec.H264, VideoFrameRate.FPS_30),
    // https://www.youtube.com/watch?v=sLprVF6d7Ug
    DASH_VIDEO_ORIGINAL_H264_GENERIC_8K(138, StreamContainer.DASH_VIDEO, VideoResolution.P_4320, VideoCodec.H264, VideoFrameRate.FPS_30),
    // https://www.youtube.com/watch?v=ZSn3Tvc7jQU
    // DASH_WEBM_VIDEO_1080P_VP9_60FPS(299, "VP9", null, null, VideoResolution.VIDEO_RESOLUTION_1080P,
    // VideoCodec.VIDEO_CODEC_VP9),
    DASH_WEBM_VIDEO_1080P_VP9(248, StreamContainer.DASH_VIDEO, VideoResolution.P_1080, VideoCodec.VP9, VideoFrameRate.FPS_30),
    // https://www.youtube.com/watch?v=T3ny9zIckP0
    // the 2610 stream on itag 266 is deklared as 60fps stream in the backend, but it is actually just 30fps. maybe 60fps will come soon on
    // the same itag?
    DASH_WEBM_VIDEO_1080P_VP9_60FPS(303, StreamContainer.DASH_VIDEO, VideoResolution.P_1080, VideoCodec.VP9, VideoFrameRate.FPS_60),
    DASH_WEBM_VIDEO_1440P_VP9(271, StreamContainer.DASH_VIDEO, VideoResolution.P_1440, VideoCodec.VP9, VideoFrameRate.FPS_30),
    DASH_WEBM_VIDEO_144P_VP9(278, StreamContainer.DASH_VIDEO, VideoResolution.P_144, VideoCodec.VP9, VideoFrameRate.FPS_30),
    DASH_WEBM_VIDEO_144P_VP9_15FPS(598, StreamContainer.DASH_VIDEO, VideoResolution.P_144, VideoCodec.VP9, VideoFrameRate.FPS_15),
    // DASH_WEBM_VIDEO_720P_VP9(247, "VP9",null, null, VideoResolution.VIDEO_RESOLUTION_720P,
    // VideoContainer.WEBM, VideoCodec.VIDEO_CODEC_VP9),
    // itag 272 videos are either 3840x2160 (e.g. RtoitU2A-3E) or 7680x4320 (sLprVF6d7Ug)
    // https://www.youtube.com/watch?v=RtoitU2A-3E 2160p
    // https://www.youtube.com/watch?v=sLprVF6d7Ug 4320p
    DASH_WEBM_VIDEO_4320P_VP9(272, StreamContainer.DASH_VIDEO, VideoResolution.P_4320, VideoCodec.VP9, VideoFrameRate.FPS_30),
    DASH_WEBM_VIDEO_2160P_VP9(272, StreamContainer.DASH_VIDEO, VideoResolution.P_2160, VideoCodec.VP9, VideoFrameRate.FPS_30),
    DASH_WEBM_VIDEO_240P_VP9(242, StreamContainer.DASH_VIDEO, VideoResolution.P_240, VideoCodec.VP9, VideoFrameRate.FPS_30),
    DASH_WEBM_VIDEO_360P_VP9(243, StreamContainer.DASH_VIDEO, VideoResolution.P_360, VideoCodec.VP9, VideoFrameRate.FPS_30),
    DASH_WEBM_VIDEO_480P_VP9(244, StreamContainer.DASH_VIDEO, VideoResolution.P_480, VideoCodec.VP9, VideoFrameRate.FPS_30),
    DESCRIPTION(10002, StreamContainer.PLAIN, MediaTagsVarious.DESCRIPTION),
    DASH_WEBM_VIDEO_480P_VP9_2(245, StreamContainer.DASH_VIDEO, VideoResolution.P_480, VideoCodec.VP9_BETTER_PROFILE_1, VideoFrameRate.FPS_30),
    DASH_WEBM_VIDEO_480P_VP9_3(246, StreamContainer.DASH_VIDEO, VideoResolution.P_480, VideoCodec.VP9_BETTER_PROFILE_2, VideoFrameRate.FPS_30),
    DASH_WEBM_VIDEO_480P_VP9_4(779, StreamContainer.DASH_VIDEO, VideoResolution.P_480, VideoCodec.VP9_BETTER_PROFILE_2, VideoFrameRate.FPS_30),
    DASH_WEBM_VIDEO_480P_VP9_5(780, StreamContainer.DASH_VIDEO, VideoResolution.P_480, VideoCodec.VP9_BETTER_PROFILE_2, VideoFrameRate.FPS_30),
    DASH_WEBM_VIDEO_720P_VP9(247, StreamContainer.DASH_VIDEO, VideoResolution.P_720, VideoCodec.VP9, VideoFrameRate.FPS_30),
    // https://www.youtube.com/watch?v=T3ny9zIckP0
    DASH_WEBM_VIDEO_720P_VP9_60FPS(302, StreamContainer.DASH_VIDEO, VideoResolution.P_720, VideoCodec.VP9, VideoFrameRate.FPS_60),
    FLV_VIDEO_360P_H264_AUDIO_AAC(34, StreamContainer.FLV, VideoResolution.P_360, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_128),
    FLV_VIDEO_480P_H264_AUDIO_AAC(35, StreamContainer.FLV, VideoResolution.P_480, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_128),
    FLV_VIDEO_HIGH_270P_H263_AUDIO_MP3(6, StreamContainer.FLV, VideoResolution.P_270, VideoCodec.H263, VideoFrameRate.FPS_30, AudioCodec.MP3, AudioBitrate.KBIT_64),
    FLV_VIDEO_LOW_240P_H263_AUDIO_MP3(5, StreamContainer.FLV, VideoResolution.P_240, VideoCodec.H263, VideoFrameRate.FPS_30, AudioCodec.MP3, AudioBitrate.KBIT_64),
    // we do not need such a bad itag. Ignore the variants
    THREEGP4_ITAG13_H263_144P_15FPS_AMRNB_12KBIT(13, StreamContainer.THREEGP, VideoResolution.P_144, VideoCodec.H263, VideoFrameRate.FPS_15, AudioCodec.AMR, AudioBitrate.KBIT_12),
    // fake id
    IMAGE_HQ(10002, StreamContainer.PLAIN, ImageQuality.HIGH),
    // fake id
    IMAGE_LQ(10004, StreamContainer.PLAIN, ImageQuality.LOW),
    // fake id
    IMAGE_MAX(10001, StreamContainer.PLAIN, ImageQuality.HIGHEST),
    // fake id
    IMAGE_MQ(10003, StreamContainer.PLAIN, ImageQuality.NORMAL),

    // fake id
    COVER_HQ(11002, StreamContainer.PLAIN, ImageQuality.HIGH),
    // fake id
    COVER_LQ(11004, StreamContainer.PLAIN, ImageQuality.LOW),
    // fake id
    COVER_MAX(11001, StreamContainer.PLAIN, ImageQuality.HIGHEST),
    // fake id
    COVER_MQ(11003, StreamContainer.PLAIN, ImageQuality.NORMAL),

    // 192 kbits aac
    MP4_VIDEO_1080P_H264_AUDIO_AAC(37, StreamContainer.MP4, VideoResolution.P_1080, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_192),
    MP4_VIDEO_1080P_H264_AUDIO_AAC_3D(85, StreamContainer.MP4, VideoResolution.P_1080, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_192),
    // not sure
    MP4_VIDEO_240P_H264_AUDIO_AAC_3D(83, StreamContainer.MP4, VideoResolution.P_240, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_96),
    // seams like there are no 96 kbit versions any more.
    // MP4_VIDEO_360P_H264_AUDIO_AAC(18, StreamContainer.MP4, VideoResolution.P_360, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC,
    // AudioBitrate.KBIT_128),
    // MP4_VIDEO_360P_H264_AUDIO_AAC_3D(82, StreamContainer.MP4, VideoResolution.P_360, VideoCodec.H264, VideoFrameRate.FPS_30,
    // AudioCodec.AAC, AudioBitrate.KBIT_128),
    MP4_VIDEO_360P_H264_AUDIO_AAC_3D_V1(82, StreamContainer.MP4, VideoResolution.P_360, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_96),
    MP4_VIDEO_360P_H264_AUDIO_AAC_V1(18, StreamContainer.MP4, VideoResolution.P_360, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_96),
    MP4_VIDEO_720P_H264_AUDIO_AAC_3D(84, StreamContainer.MP4, VideoResolution.P_720, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_192),
    MP4_VIDEO_720P_H264_AUDIO_AAC_3D_V1(84, StreamContainer.MP4, VideoResolution.P_720, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_128),
    MP4_VIDEO_720P_H264_AUDIO_AAC_3D_V3(84, StreamContainer.MP4, VideoResolution.P_720, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_152),
    /**
     * Starting since May 2024, the Format 22 is removed for every YouTube videos uploaded.
     *
     * https://gist.github.com/MartinEesmaa/2f4b261cb90a47e9c41ba115a011a4aa
     */
    @Deprecated
    MP4_VIDEO_720P_H264_AUDIO_AAC(22, StreamContainer.MP4, VideoResolution.P_720, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_192),
    @Deprecated
    MP4_VIDEO_720P_H264_AUDIO_AAC_V3(22, StreamContainer.MP4, VideoResolution.P_720, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_152),
    @Deprecated
    MP4_VIDEO_720P_H264_AUDIO_AAC_V1(22, StreamContainer.MP4, VideoResolution.P_720, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_128),
    // http://www.h3xed.com/web-and-internet/youtube-audio-quality-bitrate-240p-360p-480p-720p-1080p
    MP4_VIDEO_AUDIO_ORIGINAL(38, StreamContainer.MP4, VideoResolution.P_2160, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_192),
    // fake id
    SUBTITLE(10002, StreamContainer.PLAIN, MediaTagsVarious.SUBTITLE),
    // very different audio bitrates!!!
    THREEGP_VIDEO_144P_H264_AUDIO_AAC(17, StreamContainer.THREEGP, VideoResolution.P_144, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_32_ESTIMATED),
    // LIve stream?
    // I tested several streams, and this itag contains 180p and 32kbit audio
    THREEGP_VIDEO_240P_H263_AUDIO_AAC(132, StreamContainer.THREEGP, VideoResolution.P_240, VideoCodec.H263, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_32),
    THREEGP_VIDEO_240P_H264_AUDIO_AAC(36, StreamContainer.THREEGP, VideoResolution.P_240, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_32),
    // not sure - did not find testvideos
    WEBM_VIDEO_1080P_VP8_AUDIO_VORBIS(46, StreamContainer.WEBM, VideoResolution.P_1080, VideoCodec.VP8, VideoFrameRate.FPS_30, AudioCodec.VORBIS, AudioBitrate.KBIT_192),
    WEBM_VIDEO_360P_VP8_AUDIO_128K_VORBIS_3D(100, StreamContainer.WEBM, VideoResolution.P_360, VideoCodec.VP9, VideoFrameRate.FPS_30, AudioCodec.VORBIS, AudioBitrate.KBIT_128),
    WEBM_VIDEO_360P_VP8_AUDIO_192K_VORBIS_3D(101, StreamContainer.WEBM, VideoResolution.P_360, VideoCodec.VP9, VideoFrameRate.FPS_30, AudioCodec.VORBIS, AudioBitrate.KBIT_192),
    WEBM_VIDEO_360P_VP8_AUDIO_VORBIS(43, StreamContainer.WEBM, VideoResolution.P_360, VideoCodec.VP8, VideoFrameRate.FPS_30, AudioCodec.VORBIS, AudioBitrate.KBIT_128),
    // not sure - did not find testvideos
    WEBM_VIDEO_480P_VP8_AUDIO_VORBIS(44, StreamContainer.WEBM, VideoResolution.P_480, VideoCodec.VP8, VideoFrameRate.FPS_30, AudioCodec.VORBIS, AudioBitrate.KBIT_128),
    WEBM_VIDEO_720P_VP8_AUDIO_192K_VORBIS_3D(102, StreamContainer.WEBM, VideoResolution.P_720, VideoCodec.VP8, VideoFrameRate.FPS_30, AudioCodec.VORBIS, AudioBitrate.KBIT_192),
    // both Itags are almost the same video
    MP4_ITAG59_H264_480P_24FPS_AAC_128KBIT(59, StreamContainer.MP4, VideoResolution.P_480, VideoCodec.H264, VideoFrameRate.FPS_24, AudioCodec.AAC, AudioBitrate.KBIT_128),
    MP4_ITAG78_H264_480P_24FPS_AAC_128KBIT(78, StreamContainer.MP4, VideoResolution.P_480, VideoCodec.H264, VideoFrameRate.FPS_24, AudioCodec.AAC, AudioBitrate.KBIT_128),
    // not sure - did not find testvideos
    WEBM_VIDEO_720P_VP8_AUDIO_VORBIS(45, StreamContainer.WEBM, VideoResolution.P_720, VideoCodec.VP8, VideoFrameRate.FPS_30, AudioCodec.VORBIS, AudioBitrate.KBIT_192),
    // fps 6
    HLS_VIDEO_MP4_72P_6FPS_AUDIO_AAC(151, StreamContainer.MP4, VideoResolution.P_72, VideoCodec.H264, VideoFrameRate.FPS_6, AudioCodec.AAC, AudioBitrate.KBIT_24),
    // fps 15
    HLS_VIDEO_MP4_240P_15FPS_AUDIO_AAC(132, StreamContainer.MP4, VideoResolution.P_240, VideoCodec.H264, VideoFrameRate.FPS_15, AudioCodec.AAC, AudioBitrate.KBIT_48),
    HLS_VIDEO_MP4_144P_AUDIO_AAC(91, StreamContainer.MP4, VideoResolution.P_144, VideoCodec.H264, VideoFrameRate.FPS_15, AudioCodec.AAC, AudioBitrate.KBIT_48),
    HLS_VIDEO_MP4_240P_AUDIO_AAC_2(92, StreamContainer.MP4, VideoResolution.P_240, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_48),
    HLS_VIDEO_MP4_360P_AUDIO_AAC(93, StreamContainer.MP4, VideoResolution.P_360, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_128),
    HLS_VIDEO_MP4_480P_AUDIO_AAC(94, StreamContainer.MP4, VideoResolution.P_480, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_128),
    HLS_VIDEO_MP4_720P_AUDIO_AAC(95, StreamContainer.MP4, VideoResolution.P_720, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_256),
    HLS_VIDEO_MP4_720P_AUDIO_AAC_300(300, StreamContainer.MP4, VideoResolution.P_720, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_256),
    HLS_VIDEO_MP4_1080P_AUDIO_AAC(96, StreamContainer.MP4, VideoResolution.P_1080, VideoCodec.H264, VideoFrameRate.FPS_30, AudioCodec.AAC, AudioBitrate.KBIT_256);

    private static HashMap<Integer, List<YoutubeITAG>> TAG_MAP = new HashMap<Integer, List<YoutubeITAG>>();
    static {
        for (YoutubeITAG tag : values()) {
            List<YoutubeITAG> lst = TAG_MAP.get(tag.getITAG());
            if (lst == null) {
                lst = new ArrayList<YoutubeITAG>();
                TAG_MAP.put(tag.getITAG(), lst);
            }
            lst.add(tag);
        }
    }

    public static enum YoutubeITAGVersion {
        // http://www.h3xed.com/web-and-internet/youtube-audio-quality-bitrate-240p-360p-480p-720p-1080p
        // Before March 2011, YouTube used these audio qualities for several years:
        //
        // Resolution Audio Bit Rate Compression
        // 1080p 128 kbps AAC
        // 720p 128 kbps AAC
        // 480p 96 kbps AAC
        // 360p 96 kbps AAC
        // 240p 64 kbps MP3
        V1(0, new Date(2011 - 1900, 2, 1, 23, 59, 59).getTime()),
        // http://www.h3xed.com/web-and-internet/youtube-audio-quality-bitrate-240p-360p-480p-720p-1080p
        // Videos uploaded March 2011 to May 2011 had these audio qualities, as long as the originally uploaded video had a high enough
        // audio bit rate or was lossless:
        //
        // Resolution Audio Bit Rate Compression
        // VideoResolution.VIDEO_RESOLUTION_1440Pp 128 kbps AAC
        // 720p 128 kbps AAC
        // 480p 128 kbps AAC
        // 360p 128 kbps AAC
        // 240p 64 kbps MP3
        V2(new Date(2011 - 1900, 2, 1, 0, 0, 0).getTime(), new Date(2011 - 1900, 4, 1, 23, 59, 59).getTime()),
        // http://www.h3xed.com/web-and-internet/youtube-audio-quality-bitrate-240p-360p-480p-720p-1080p
        // Videos uploaded May 2011 to July 2012 had these audio qualities, as long as the originally uploaded video had a high enough audio
        // bit rate or was lossless:
        //
        // Resolution Audio Bit Rate Compression
        // Original 152 kbps AAC
        // 1080p 152 kbps AAC
        // 720p 152 kbps AAC
        // 480p 128 kbps AAC
        // 360p 128 kbps AAC
        // 240p 64 kbps MP3
        V3(new Date(2011 - 1900, 4, 1, 0, 0, 0).getTime(), new Date(2012 - 1900, 6, 1, 23, 59, 59).getTime()),
        // http://www.h3xed.com/web-and-internet/youtube-audio-quality-bitrate-240p-360p-480p-720p-1080p
        // Videos uploaded July 2012 to date should have these audio qualities, as long as the originally uploaded video had a high enough
        // audio bit rate or was lossless:
        //
        // Resolution Audio Bit Rate Compression
        // Original 192 kbps AAC
        // 1080p 192 kbps AAC
        // 720p 192 kbps AAC
        // 480p 128 kbps AAC
        // 360p 128 kbps AAC
        // 240p 64 kbps MP3
        //
        // Occasionally you can find a 240p-only video that has 96 kbps audio bit rate, like this one. I'm not sure why.
        // "Original" resolution is any video size that is larger than 1920x1080. For example, if you upload a 1920x1200 or 2560x1600 video,
        // it should show the "Original" option. The audio quality is currently the same as 720p and 1080p.
        // YouTube will often update older videos to play better audio quality, if that higher quality audio was saved when it was
        // originally uploaded.
        V4(new Date(2012 - 1900, 6, 1, 0, 0, 0).getTime(), Long.MAX_VALUE);

        private long from;
        private long to;

        YoutubeITAGVersion(long from, long to) {
            this.from = from;
            this.to = to;
        }

        public boolean matches(long uploadDate) {
            return uploadDate >= from && uploadDate < to;
        }

        public static YoutubeITAGVersion getByDate(long uploadDate) {
            for (YoutubeITAGVersion v : YoutubeITAGVersion.values()) {
                if (v.matches(uploadDate)) {
                    return v;
                }
            }
            return YoutubeITAGVersion.V4;
        }
    }

    public static YoutubeITAG get(int itag, int width, int height, int fps, String type, UrlQuery query, long uploadDate) {
        YoutubeITAGVersion version = null;
        if (query == null) {
            query = new UrlQuery();
        }
        switch (itag) {
        case 132:
            // https://www.youtube.com/watch?v=KF47Za1lfjM
            if ("hls_playlist".equals(query.get("manifest")) || (type != null && (type.contains("avc") || type.contains("mp4")))) {
                return HLS_VIDEO_MP4_240P_15FPS_AUDIO_AAC;
            } else {
                return THREEGP_VIDEO_240P_H263_AUDIO_AAC;
            }
            // case 18:
            // if (version == null) {
            // version = YoutubeITAGVersion.getByDate(uploadDate);
            // }
            // switch (version) {
            // case V1:
            // return MP4_VIDEO_360P_H264_AUDIO_AAC_V1;
            // default:
            // return MP4_VIDEO_360P_H264_AUDIO_AAC;
            // }
        case 22:
            if (version == null) {
                version = YoutubeITAGVersion.getByDate(uploadDate);
            }
            switch (version) {
            case V1:
            case V2:
                return MP4_VIDEO_720P_H264_AUDIO_AAC_V1;
            case V3:
                return MP4_VIDEO_720P_H264_AUDIO_AAC_V3;
            default:
                return MP4_VIDEO_720P_H264_AUDIO_AAC;
            }
        case 84:
            if (version == null) {
                version = YoutubeITAGVersion.getByDate(uploadDate);
            }
            switch (version) {
            case V1:
            case V2:
                return MP4_VIDEO_720P_H264_AUDIO_AAC_3D_V1;
            case V3:
                return MP4_VIDEO_720P_H264_AUDIO_AAC_3D_V3;
            default:
                return MP4_VIDEO_720P_H264_AUDIO_AAC_3D;
            }
        }
        List<YoutubeITAG> options = TAG_MAP.get(itag);
        if (options == null) {
            return null;
        }
        YoutubeITAG best = null;
        double bestValue = Double.MAX_VALUE;
        for (final YoutubeITAG tag : options) {
            double value = 0d;
            if (tag.getVideoResolution() != null) {
                value += Math.abs(height - tag.getVideoResolution().getHeight());
                value += Math.abs(fps - tag.getVideoFrameRate().getFps());
            }
            if (best == null || bestValue > value) {
                bestValue = value;
                best = tag;
                if (value == 0d) {
                    break;
                }
            }
        }
        if (bestValue != 0d && height > 0 && fps > 3) {
            // System.out.println("Height missmatch");
            // Log.warning("Youtube ITag Mismatch: lookup fps" + fps + " height" + height + " -> " + best);
        }
        return best;
    }

    public VideoResolution getVideoResolution() {
        return videoResolution;
    }

    private final int          itag;
    private MediaTagsVarious[] qualityTags;
    private VideoResolution    videoResolution;
    private VideoCodec         videoCodec;
    private AudioCodec         audioCodec;
    private AudioBitrate       audioBitrate;
    private ImageQuality       imageQuality;
    private StreamContainer    rawContainer;

    public VideoCodec getVideoCodec() {
        return videoCodec;
    }

    public AudioCodec getAudioCodec() {
        return audioCodec;
    }

    public AudioBitrate getAudioBitrate() {
        return audioBitrate;
    }

    public ImageQuality getImageQuality() {
        return imageQuality;
    }

    public VideoFrameRate getVideoFrameRate() {
        return videoFrameRate;
    }

    private YoutubeITAG(final int itag, StreamContainer streamcontainer, MediaTagsVarious various) {
        this(itag);
        this.qualityTags = new MediaTagsVarious[] { various };
        this.rawContainer = streamcontainer;
    }

    private YoutubeITAG(final int itag, StreamContainer streamcontainer, ImageQuality image) {
        this(itag);
        this.imageQuality = image;
        this.rawContainer = streamcontainer;
    }

    private YoutubeITAG(final int itag, StreamContainer streamcontainer, AudioCodec tags, AudioBitrate bitrate) {
        this(itag);
        audioCodec = tags;
        this.audioBitrate = bitrate;
        this.rawContainer = streamcontainer;
    }

    private YoutubeITAG(final int itag, StreamContainer streamcontainer, VideoResolution resolution, VideoCodec codec, VideoFrameRate fps, AudioCodec audioCodec, AudioBitrate bitrate, MediaTagsVarious various) {
        this(itag);
        this.videoResolution = resolution;
        this.videoCodec = codec;
        this.audioCodec = audioCodec;
        this.audioBitrate = bitrate;
        this.videoFrameRate = fps;
        this.qualityTags = new MediaTagsVarious[] { various };
        this.rawContainer = streamcontainer;
    }

    private YoutubeITAG(final int itag, StreamContainer streamcontainer, VideoResolution resolution, VideoCodec codec, VideoFrameRate fps, AudioCodec audioCodec, AudioBitrate bitrate) {
        this(itag);
        this.videoResolution = resolution;
        this.videoCodec = codec;
        this.audioCodec = audioCodec;
        this.audioBitrate = bitrate;
        this.videoFrameRate = fps;
        this.rawContainer = streamcontainer;
    }

    private YoutubeITAG(final int itag, StreamContainer streamcontainer, VideoResolution resolution, VideoCodec codec, VideoFrameRate fps, MediaTagsVarious various) {
        this(itag);
        this.videoResolution = resolution;
        this.rawContainer = streamcontainer;
        this.videoCodec = codec;
        this.videoFrameRate = fps;
        this.qualityTags = new MediaTagsVarious[] { various };
    }

    private YoutubeITAG(final int itag, StreamContainer streamcontainer, VideoResolution resolution, VideoCodec codec, VideoFrameRate fps) {
        this(itag);
        this.videoResolution = resolution;
        this.rawContainer = streamcontainer;
        this.videoCodec = codec;
        this.videoFrameRate = fps;
        this.qualityTags = new MediaTagsVarious[] {};
    }

    public boolean containsTag(MediaTagsVarious tag) {
        for (MediaTagsVarious s : qualityTags) {
            if (s == tag) {
                return true;
            }
        }
        return false;
    }

    public MediaTagsVarious[] getQualityTags() {
        return qualityTags;
    }

    private YoutubeITAG(final int itag) {
        this.itag = itag;
    }

    public int getITAG() {
        return this.itag;
    }

    private VideoFrameRate videoFrameRate = null;

    public static List<YoutubeITAG> getTagList(int itag) {
        return Collections.unmodifiableList(TAG_MAP.get(itag));
    }

    public StreamContainer getRawContainer() {
        return rawContainer;
    }
}