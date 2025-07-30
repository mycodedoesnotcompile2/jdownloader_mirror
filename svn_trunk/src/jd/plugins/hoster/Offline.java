//jDownloader - Downloadmanager
//Copyright (C) 2011  JD-Team support@jdownloader.org
//
//This program is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this program.  If not, see <http://www.gnu.org/licenses/>.
package jd.plugins.hoster;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.appwork.utils.StringUtils;
import org.jdownloader.plugins.controller.LazyPlugin;
import org.jdownloader.plugins.controller.host.HostPluginController;

import jd.PluginWrapper;
import jd.http.Browser;
import jd.plugins.Account;
import jd.plugins.AccountInfo;
import jd.plugins.DownloadLink;
import jd.plugins.DownloadLink.AvailableStatus;
import jd.plugins.HostPlugin;
import jd.plugins.LinkStatus;
import jd.plugins.PluginException;
import jd.plugins.PluginForHost;

/**
 * The idea behind this is to speed up linkchecking for host providers that go permanently offline. URLs tend to stay cached/archived on the
 * intrawebs longer than host provider. By providing the original plugin regular expression(s) we do not have to rely on directhttp plugin
 * for linkchecking, or surrounding issues with 'silent errors' within the linkgrabber if the file extension isn't matched against
 * directhttp. <br />
 * - raztoki<br />
 * <br />
 * Set interfaceVersion to 3 to avoid old Stable trying to load this Plugin<br />
 *
 * @author raztoki<br />
 */
@HostPlugin(revision = "$Revision: 51276 $", interfaceVersion = 3, names = {}, urls = {})
public class Offline extends PluginForHost {
    public static String getOfflineVersion() {
        final HostPlugin hostPlugin = Offline.class.getAnnotation(HostPlugin.class);
        return hostPlugin != null ? hostPlugin.revision() : "";
    }

    /**
     *
     * @param wrapper
     */
    public Offline(PluginWrapper wrapper) {
        super(wrapper);
    }

    @Override
    public LazyPlugin.FEATURE[] getFeatures() {
        return new LazyPlugin.FEATURE[] { LazyPlugin.FEATURE.INTERNAL };
    }

    public static List<String[]> getPluginDomains() {
        // each entry in List<String[]> will result in one PluginForHost
        List<String[]> ret = null;
        final Map<String, Object> cache = HostPluginController.PLUGIN_UPDATE_CACHE.get();
        final String cacheID = Offline.class.getName() + "-PluginDomains." + getOfflineVersion();
        if (cache != null && (ret = (List<String[]>) cache.get(cacheID)) != null) {
            return ret;
        }
        ret = new ArrayList<String[]>();
        final String[] singleDomainHosts = new String[] { "datafile.com", "noco.tv", "upload.zone", "fflares.com", "fileflares.com", "filescdn.com", "filescdn.net", "mixstep.co", "vip.belle.la", "aniteca.zlx.com.br", "streaminporn.xyz", "upload2win.com", "top4upload.com", "stream.moe", "ezfiles.net", "cloudupload.co", "axfiles.net", "xdrive.cc", "omerta.is", "borfos.com", "xshare.eu", "xeupload.com", "nodefiles.com", "gwshare.com", "upasias.com", "upload4earn.com", "downgb.com", "fenixfile.com", "flexydrive.com", "indoshares.com", "yousaved.it", "rapidturk.com", "diskokosmiko.mx", "linestorage.org", "uploadbits.com", "uploadbits.net", "uploadburst.com", "filecloud.io", "ezfile.ch", "we4load.com", "xfilesharing.us", "host.hackerbox.org", "ulozisko.sk", "teramixer.com", "imgspot.org", "uploadadz.com", "bdnupload.com", "catshare.net", "videobash.com", "ultimatedown.com", "d-h.st",
                "tenlua.vn", "5azn.net", "rapidpaid.com", "iranupload.com", "bytewhale.com", "filecyber.com", "putfiles.in", "fileud.com", "tempfile.ru", "streammania.com", "brapid.sk", "watchers.to", "movdivx.com", "megadrive.tv", "megadrive.co", "swoopshare.com", "supershare.pl", "uptodo.net", "rawabbet.com", "zorofiles.com", "arivoweb.com", "hippohosted.com", "rabidfiles.com", "animefiles.online", "uploads.to", "uplod.it", "photo.qip.ru", "file.qip.ru", "uploadable.ch", "bigfile.to", "imgzen.com", "imgdragon.com", "coreimg.net", "pic-maniac.com", "filemack.com", "filemac.com", "filekom.com", "file.oboz.ua", "protect-url.net", "p-u.in", "speedshare.eu", "magic4up.com", "uploadkadeh.com", "megafiles.us", "fileproject.com.br", "fileinstant.com", "uploadx.org", "uploadx.co", "uploadz.org", "uploadz.co", "uploadz.click", "uploaduj.net", "uploads.ws", "upl.me", "herosh.com",
                "avatarshare.com", "sju.wang", "uploadkadeh.ir", "uploading.site", "miravideos.net", "1000eb.com", "upload.mn", "upload.af", "sharehost.eu", "linkzhost.com", "files.com", "imgcandy.net", "failai.lt", "loadgator.com", "megafileupload.com", "megafirez.com", "megawatch.net", "dj97.com", "lacoqui.net", "vodlock.co", "vodlocker.city", "wizupload.com", "ziifile.com", "foxyimg.link", "chronos.to", "minhateca.com.br", "datasbit.com", "tubeq.xxx", "superupload.com", "disk.tom.ru", "mygirlfriendvids.net", "kingvid.tv", "faphub.xxx", "funonly.net", "bemywife.cc", "noslocker.com", "nosvideo.com", "hotamateurs.xxx", "bezvadata.cz", "anafile.com", "imgtiger.org", "minup.net", "jumbload.com", "media4up.com", "x3xtube.com", "basicupload.com", "jeodrive.com", "coolbytez.com", "keepshare.net", "superbshare.com", "levinpic.org", "imggold.org", "gavitex.com", "filesflash.com",
                "share.vnn.vn", "filebebo.cc", "imgdiamond.com", "imgswift.com", "arabloads.net", "filesisland.com", "share.az", "gigasize.com", "tuberealm.com", "nudeflix.com", "grifthost.com", "xvidstage.com", "up07.net", "wastedamateurs.com", "filedais.com", "streamin.to", "myimg.club", "depfile.com", "mangatraders.biz", "vid.me", "uploadrocket.net", "faplust.com", "unlimitzone.com", "copiapop.com", "partagora.com", "filespace.io", "kingfiles.net", "uber-sha.re", "obscuredfiles.com", "uploadlw.com", "ulnow.com", "nashdisk.ru", "partage-facile.com", "ourupload.com", "glbupload.com", "imgve.com", "drfile.net", "bitload.org", "megafile.co", "gulf4up.com", "mydrive.com", "uplea.com", "letwatch.us", "onemillionfiles.com", "unlimit.co.il", "ul-instant.pw", "goear.com", "exfile.ru", "extradj.com", "exashare.com", "ecostream.tv", "karadownloads.com", "dropjar.com", "zstream.to", "ulshare.se",
                "f-bit.ru", "movyo.to", "yourvideohost.com", "tigerfiles.net", "thevideos.tv", "megafile.org", "99yp.cc", "ultrashare.net", "host4desi.com", "coo5shaine.com", "rapidvideo.ws", "neodrive.co", "mediafree.co", "filez.tv", "hochladen.to", "ampya.com", "filearn.com", "crocko.com", "porn5.com", "torrent.ajee.sh", "tikfile.com", "sharesend.com", "upfiles.net", "rioupload.com", "limefile.com", "powerwatch.pw", "up4file.com", "sli.mg", "sfshare.se", "rabbitfile.com", "megadysk.pl", "megadrv.com", "grimblr.com", "dwn.so", "cloudshares.net", "72bbb.com", "vip-shared.com", "someimage.com", "rodfile.com", "mightyupload.com", "pornative.com", "oload.net", "mediafire.bz", "lafiles.com", "kure.tv", "junocloud.me", "fileweed.net", "imgblitz.pw", "fibero.co", "filescloud.co", "fileload.io", "dedifile.com", "anysend.com", "2downloadz.com", "otakushare.com", "uploadbb.co", "bitcasa.com",
                "imgwet.com", "ziddu.com", "imxd.net", "myvideo.de", "timsah.com", "shared.sx", "stagevu.com", "vip-file.com", "shareflare.net", "moevideo.net", "letitbit.net", "lumload.com", "rosharing.net", "erafile.com", "uploading.com", "cloudsix.me", "zalaa.com", "vodlocker.com", "vidbull.com", "timeload.net", "sendspace.pl", "secureupload.eu", "mp3takeout.com", "imzdrop.com", "idowatch.net", "jpopsuki.tv", "minus.com", "happystreams.net", "kingload.net", "interfile.net", "imgbb.net", "filemoney.com", "auengine.com", "rocketfiles.net", "flashvids.org", "yamivideo.com", "divxhosted.com", "4upld.com", "filespart.com", "uploadc.com", "skymiga.com", "shareblue.eu", "videopremium.tv", "veterok.tv", "goo.im", "soundowl.com", "megaxfile.com", "bitshare.com", "promptfile.com", "fireswap.org", "vessel.com", "loadus.net", "gbload.com", "themediastorage.com", "uphere.pl", "megashares.com",
                "share-online.biz", "datei.to", "widool.com", "vidspot.net", "hzfile.al", "video.tt", "tradingporn.com", "pdfcast.org", "mydisk.ge", "mp3hamster.net", "megacache.net", "laoupload.com", "hoodload.com", "filerev.cc", "filepi.com", "filehoot.com", "maxcloud.xyz", "mediaupload.us", "hugefiles.net", "upvast.com", "allmyvideos.net", "filestorm.xyz", "1tube.to", "hdstream.to", "sendfile.pl", "sharesix.net", "sharesix.com", "filenuke.com", "sharing.zone", "filesabc.com", "sendfiles.nl", "firstplanet.eu", "shareneo.net", "vidxtreme.to", "ugoupload.net", "sharingmaster.com", "videomega.tv", "speedyshare.com", "uploadbaz.com", "stiahni.si", "imgbar.net", "voooh.com", "filesuniverse.com", "vodbeast.com", "primephile.com", "hzfile.asia", "filenuke.net", "revclouds.com", "upple.it", "filesbomb.in", "livecloudz.com", "uploadscenter.com", "uploadspace.pl", "filebeam.com",
                "bestreams.net", "zettahost.tv", "divxpress.com", "primeshare.tv", "uploadingit.com", "veevr.com", "putstream.com", "zalohuj.to", "yy132.cn", "snakefiles.com", "rapidvideo.tv", "radicalshare.com", "nizfile.net", "novamov.me", "freespace.by", "fastimg.org", "fileflush.com", "downlod.co", "datoteke.com", "animeuploads.com", "cizgifilmlerizle.com", "1000shared.com", "myupload.dk", "vidce.tv", "uploadcoins.com", "fufox.net", "ninjashare.pl", "city-upload.com", "7958.com", "beemp3s.org", "coladrive.com", "megaloads.org", "easydow.org", "easywatch.tv", "vplay.ro", "chayfile.com", "purevid.com", "hdload.info", "devilshare.net", "lolabits.es", "gigafront.de", "5fantastic.pl", "copy.com", "treefiles.com", "mrfile.me", "filepurpose.com", "fexe.com", "avht.net", "24uploading.com", "datagrad.ru", "filesup.co", "steepafiles.com", "yourfiles.to", "hotbytez.com", "fileover.net",
                "vipup.in", "terafile.co", "vidig.biz", "uploadmax.net", "soniclocker.com", "ps3gameroom.net", "ipithos.to", "nowvideo.ws", "fileloby.com", "imgsee.me", "jumbofiles.com", "imgmega.com", "fisierulmeu.ro", "filepost.com", "djvv.com", "creeperfile.com", "bleuup.net", "carrier.so", "nosupload.com", "vidgrab.net", "netkups.com", "luckyshare.net", "abelhas.pt", "divshare.com", "letitload.com", "uploadsun.com", "cloudcorner.com", "gasxxx.com", "imageporter.com", "jizzbox.com", "foxytube.com", "fileplaneta.com", "nekaka.com", "vipfileshare.com", "filecore.co.nz", "idrivesync.com", "youporn-deutsch.com", "onlinestoragesolution.com", "oooup.com", "anonymousdelivers.us", "watching.to", "vipfile.in", "teraupload.net", "swiftupload.com", "xddisk.com", "xfiles.ivoclarvivadent.com", "vidd.tv", "up2box.co", "storefiles.co", "songs.to", "nowvideos.eu", "mediaprog.ru", "jumbofile.net",
                "maxupload.tv", "jippyshare.com", "labload.com", "joinfile.com", "hcbit.com", "gigafileupload.com", "filesin.com", "filekee.com", "fileshost.ws", "fileshare.to", "filenium.com", "filer.cx", "exstorage.net", "fileneo.com", "filemeup.net", "data.cod.ru", "cx.com", "cloudsuzy.com", "foxplay.info", "1gbpsbox.com", "banicrazy.info", "veodrop.com", "bigdownloader.com", "beehd.net", "3files.net", "filehost.pw", "storbit.net", "anyfiles.org", "2upfile.com", "ryushare.com", "onevideo.to", "junkyvideo.com", "filecrash.co", "filearning.com", "xenubox.com", "remixshare.com", "filecloud.cc", "4upfiles.com", "plunder.com", "elffiles.com", "shantibit.com", "x-share.ru", "vinupload.com", "linkfile.de", "jumbofiles.net", "freeporn.to", "filebrella.com", "storedrives.com", "uploadblast.com", "skyvids.net", "4uploaded.com", "up4.im", "v-vids.com", "firedrive.com", "sockshare.com",
                "bearfiles.in", "mooshare.biz", "animecloud.me", "aimini.net", "rainy.la", "project-free-upload.com", "netload.in", "riotshare.com", "premiuns.org", "sendfaile.com", "pururin.com", "tumi.tv", "180upload.com", "tuspics.net", "stageflv.com", "shr77.com", "filebulk.com", "speedy-share.com", "sloozie.com", "otr-download.de", "fizy.com", "1tpan.com", "4uploading.com", "dropvideo.com", "filebox.ro", "vidbux.com", "filerock.net", "movreel.com", "megavideoz.eu", "realvid.net", "blip.tv", "gulfup.com", "hardwareclips.com", "uploadnet.co", "files.gw.kz", "files123.net", "free4share.de", "gettyfile.ru", "free-share.ru", "rziz.net", "zinwa.com", "wuala.com", "fileband.com", "megairon.net", "xfileload.com", "free-uploading.com", "fileown.com", "verzend.be", "rapidsonic.com", "filepack.pl", "goldbytez.com", "filemaze.ws", "divxden.com", "vidplay.net", "royalvids.eu",
                "superromworld.de", "n-roms.de", "simpleshare.org", "sharebeast.com", "sanshare.com", "faststream.in", "storedeasy.com", "jumbofiles.org", "uploadlux.com", "linestorage.com", "thefile.me", "muchshare.net", "queenshare.com", "videobam.com", "mp3the.net", "fileforever.net", "quickshare.cz", "warped.co", "played.to", "topupload1.com", "creafile.net", "creafile.com", "krotix.net", "akafile.com", "videonan.com", "upload-drive.com", "share.time.mn", "pushfile.com", "fileriio.com", "dropfiles.info", "2drive.net", "400disk.com", "up-loading.net", "zapisz.to",
                /** turbobit alias' */
                "sharephile.com", "mxua.com", "katzfiles.com", "fsakura.com", "uploadur.com", "wyslijplik.pl", "tishare.com", "share50.com", "med1fire.com", "dumpfiles.org", "videolog.tv", "storeplace.org", "mojoload.com", "ifilehosting.net", "gigfiles.net", "maxisonic.com", "sv-esload.com", "upgaf.com", "gulfdown.com", "imageeer.com", "loombo.com", "upbrasil.info", "filegig.com", "xtraupload.net", "upservmedia.com", "cloudlync.com", "uploadto.us", "loudupload.net", "minoshare.com", "fun-vids.org", "flowload.com", "divxplanet.com", "saryshare.com", "uploadrive.com", "noelshare.com", "yunio.com", "daj.to", "voowl.com", "filerio.im", "boosterking.com", "u-tube.ru", "medifire.net", "galaxy-file.com", "uploadhunt.com", "upload-il.com", "uploadhero.co", "filefeltolto.hu", "przeslij.net", "altervideo.net", "fileswap.com", "safesharing.eu", "japlode.com", "shared.com", "streamit.to",
                "ilook.to", "imgah.com", "grooveshark.com", "mais.me", "megaul.com", "maxshare.pl", "mcupload.com", "mediavalise.com", "uncapped-downloads.com", "deerfile.com", "fupload.net", "zomgupload.com", "disk.84dm.com", "gfssex.com", "7thsky.es", "2download.de", "filezup.net", "fastupload.org", "files2share.ch", "metfiles.com", "1024disk.com", "boojour.eu", "clips-and-pics.org", "axifile.com", "filedust.net", "mixturecloud.com", "1st-files.com", "gigabyteupload.com", "filespeed.net", "midupload.com", "rainupload.com", "kingshare.to", "mejuba.com", "xlocker.net", "xvideohost.com", "rapidshare.com", "dwnshare.pl", "azerfile.com", "sindema.info", "seenupload.com", "sangfile.com", "fileloads.cc", "wdivx.com", "hotsvideos.com", "lovevideo.tv", "transitfiles.com", "xuploading.net", "asfile.com", "roottail.com", "filehostup.com", "freehostina.com", "porntubevidz.com", "upchi.co.il",
                "fastvideo.eu", "hostingbulk.com", "therapide.cz", "yonzy.com", "ravishare.com", "vozupload.com", "getzilla.net", "videodd.net", "dogefile.com", "nakido.com", "filevice.com", "diskfiles.net", "billionuploads.com", "filebite.cc", "gsinfinite.com", "heaven666.org", "saganfiles.com", "theamateurzone.info", "ultrafile.me", "fileb.ag", "k-files.kz", "clicktoview.org", "xvidstream.net", "filemup.com", "miloshare.com", "lomafile.com", "megacrypter.com", "iiiup.com", "digzip.com", "senseless.tv", "henchfile.com", "cometfiles.com", "mijnbestand.nl", "upafile.com", "bl.st", "filthyrx.com", "turbovid.net", "privatefiles.com", "vacishare.com", "xerver.co", "qshare.com", "tuxfile.com", "gigaup.fr", "hddspace.com", "foxishare.com", "filestube.com", "miloyski.com", "upload.hidemyass.com", "divxhosting.net", "sube.me", "quickupload.net", "mydisc.net", "isavelink.com", "filestorm.to",
                "cruzload.com", "bubblefiles.com", "dodane.pl", "myuplbox.com", "updown.bz", "thefilebay.com", "mukupload.com", "box4up.com", "5ilthy.com", "filepom.com", "dizzcloud.com", "cloudfly.us", "vidhog.com", "slingfile.com", "iperupload.com", "oceanus.ch", "mrmkv-fileshare.com", "filejungle.com", "fileserve.com", "zalil.ru", "uploadsat.com", "turbotransfer.pl", "uploadlab.com", "shareyourfile.biz", "tomwans.com", "megabox.ro", "failai.kava.lt", "stahovadlo.cz", "justin.tv", "4share.ws", "pliczek.net", "2gb-hosting.com", "maskfile.com", "rockdizfile.com", "yesload.net", "dotsemper.com", "mixbird.com", "myjizztube.com", "packupload.com", "megaszafa.com", "potload.com", "usefile.com", "nitrobits.com", "uploads.center", "filemonkey.in", "multishared.me", "wallbase.cc", "swankshare.com", "vidbox.yt", "vidzbeez.com", "megafiles.se", "fileom.com", "bluehaste.com", "socifiles.com",
                "mlfat4arab.com", "megashare.by", "flinzy.com", "filesend.net", "fastfileshare.com.ar", "ddlstorage.com", "spaadyshare.com", "sizfile.com", "hostingcup.com", "filecanyon.com", "bytesbox.com", "dropvideos.net", "tubecloud.net", "xrabbit.com", "tubeq.net", "multiupload.com", "pizzaupload.com", "ntupload.com", "magnovideo.com", "files.to", "moidisk.ru", "vidaru.com", "epicshare.net", "filemov.net", "hipfile.com", "zingload.com", "fileove.com", "rnbload.com", "filesaur.com", "wrzuc.to", "hyshare.com", "redload.net", "blitzfiles.com", "qkup.net", "lajusangat.net", "filechum.com", "uploadnetwork.eu", "videoslim.net", "filesfrog.net", "fiberupload.net", "hottera.com", "filego.org", "putme.org", "upshare.me", "uploadzeal.com", "space4file.com", "nzbload.com", "vreer.com", "safashare.com", "shareprofi.com", "crisshare.com", "lemuploads.com", "pandapla.net", "shurload.es",
                "megaupdown.com", "imagehaven.net", "filedap.com", "limevideo.net", "vids.bz", "wallobit.com", "davvas.com", "uploadinc.com", "filelaser.com", "finaload.com", "hostinoo.com", "sinhro.net", "megashare.com", "berofile.com", "uploadizer.net", "megarelease.org", "share-byte.net", "rapidstation.com", "filechin.com", "fujifile.me", "freeuploads.fr", "guizmodl.net", "file1.info", "maximusupload.com", "pandamemo.com", "igetfile.com", "megacloud.com", "egofiles.com", "cloudvidz.net", "videozed.net", "donevideo.com", "migaload.com", "maxvideo.pl", "tgf-services.com", "albafile.com", "cramit.in", "4savefile.com", "batshare.com", "oleup.com", "filecopter.net", "easyfilesharing.info", "uploadedhd.com", "ilikefile.com", "fileomg.com", "file.am", "navihost.us", "upfile.biz", "dynaupload.com", "file-speed.com", "cloudxeon.com", "hdplay.org", "files2upload.net", "dollyshare.com",
                "filetug.com", "mirorii.com", "faceporn.no", "bitload.it", "beatplexity.com", "bandbase.dk", "bananahost.it", "gigamax.ch", "goldfile.eu", "judgeporn.com", "elcorillord.org", "rocketfile.net", "maxisharing.com", "duckload.co", "homesexdaily.com", "pornbanana.com", "celebritycunt.net", "speedvid.tv", "filexb.com", "upsharez.com", "universalfilehosting.com", "4vid.me", "anyap.info", "evominds.com", "topvideo.cc", "zenfiles.biz", "fryhost.com", "turtleshare.com", "uploadjet.net", "devilstorage.com", "videofox.net", "extabit.com", "videofrog.eu", "nirafile.com", "omploader.org", "speedload.org", "sharefiles.co", "upit.in", "dump1.com", "fleon.me", "limelinx.com", "flashstream.in", "hotuploading.com", "filecity.net", "servifile.com", "filegag.com", "sharerun.com", "heftyfile.com", "uploadorb.com", "clipshouse.com", "edoc.com", "fileswappr.com", "filerace.com", "cloudzer.net",
                "netdrive.ws", "file4sharing.com", "rapidapk.com", "docyoushare.com", "kongsifile.com", "vureel.com", "dataport.cz", "uploadoz.com", "warserver.cz", "1clickshare.net", "cepzo.com", "fireuploads.net", "megaup1oad.net", "megaup.me", "ezzfile.com", "skylo.me", "filefolks.com", "videoslasher.com", "filebigz.com", "filezy.net", "vidx.to", "uploadstation.com", "wooupload.com", "ifile.ws", "filesbb.com", "ihostia.com", "youload.me", "ok2upload.com", "bitupload.com", "cobrashare.sk", "enjoybox.in", "share4files.com", "up.msrem.com", "megaload.it", "terafiles.net", "freestorage.ro", "filekai.com", "divxforevertr.com", "filekeeping.de", "livefile.org", "iranfilm16.com", "shufuni.com", "belgeler.com", "loadhero.net", "ngsfile.com", "1-clickshare.com", "fastsonic.net", "brutalsha.re", "moviesnxs.com", "hotfile.com", "sharedbit.net", "ufox.com", "comload.net", "6ybh-upload.com",
                "cloudnes.com", "fileprohost.com", "cyberlocker.ch", "filebox.com", "x7files.com", "videozer.com", "megabitshare.com", "filestay.com", "uplly.com", "asixfiles.com", "zefile.com", "kingsupload.com", "fileking.co", "sharevid.co", "4fastfile.com", "1-upload.com", "dump.ro", "dippic.com", "uploking.com", "zshare.ma", "book-mark.net", "ginbig.com", "ddl.mn", "syfiles.com", "iuploadfiles.com", "thexyz.net", "zakachali.com", "indianpornvid.com", "hotfiles.ws", "wizzupload.com", "banashare.com", "downupload.com", "putshare.com", "vidbox.net", "filetube.to", "nowveo.com", "uploadic.com", "flashdrive.it", "flashdrive.uk.com", "filewinds.com", "wrzucaj.com", "toucansharing.com", "uploaddot.com", "zooupload.com", "uploadcore.com", "spaceha.com", "tubethumbs.com", "peeje.com", "datacloud.to", "xxxmsncam.com", "uploadboxs.com", "247upload.com", "fileshare.in.ua", "upload.tc",
                "filesmall.com", "fileuplo.de", "quakefile.com", "vdoreel.com", "flazhshare.com", "upmorefiles.com", "cloudyload.com", "icyfiles.com", "vidpe.com", "clouds.to", "zuzufile.com", "hostfil.es", "onlinedisk.ru", "fileduct.com", "frogup.com", "filejumbo.com", "dump.ru", "fileshawk.com", "vidstream.us", "filezpro.com", "fileupper.com", "speedy-share.net", "files.ge", "gbitfiles.com", "xtilourbano.info", "allbox4.com", "arab-box.com", "farmupload.com", "filedefend.com", "filesega.com", "kupload.org", "multishare.org", "98file.com", "wantload.com", "esnips.com", "uload.to", "share76.com", "filemates.com", "stahnu.to", "filestock.ru", "uploader.pl", "mach2upload.com", "megaunload.net", "bonpoo.com", "modovideo.com", "bitoman.ru", "maknyos.com", "upgrand.com", "pigsonic.com", "filevelocity.com", "filegaze.com", "ddldrive.com", "fileforth.com", "files-save.com", "media-4.me",
                "backupload.net", "upafacil.com", "filedownloads.org", "filesector.cc", "netuploaded.com", "squillion.com", "sharebees.com", "filetobox.com", "mojedata.sk", "grupload.com", "stickam.com", "gimmedatnewjoint.com", "dup.co.il", "eazyupload.net", "depoindir.com", "own3d.tv", "drop.st", "favupload.com", "anonstream.com", "odsiebie.pl", "shareupload.com", "filebeer.info", "uploadfloor.com", "venusfile.com", "welload.com", "upaj.pl", "shareupload.net", "tsarfile.com", "omegave.org", "fsx.hu", "kiwiload.com", "gbmeister.com", "filesharing88.net", "fileza.net", "filecloud.ws", "filesome.com", "filehost.ws", "filemade.com", "bloonga.com", "zettaupload.com", "aavg.net", "freeporn.com", "bitbonus.com", "sharebeats.com", "vidhost.me", "filetechnology.com", "badongo.com", "uptorch.com", "videoveeb.com", "fileupped.com", "repofile.com", "filemsg.com", "dopeshare.com", "filefat.com",
                "fileplayground.com", "fileor.com", "aieshare.com", "q4share.com", "share-now.net", "1hostclick.com", "mummyfile.com", "hsupload.com", "upthe.net", "ufile.eu", "bitroad.net", "coolshare.cz", "speedfile.cz", "your-filehosting.com", "brontofile.com", "filestrum.com", "filedove.com", "sharpfile.com", "filerose.com", "filereactor.com", "boltsharing.com", "turboupload.com", "glumbouploads.com", "terabit.to", "buckshare.com", "zeusupload.com", "filedino.com", "filedude.com", "uptal.org", "uptal.net", "file-bit.net", "xtshare.com", "cosaupload.org", "sharing-online.com", "filestrack.com", "shareator.net", "azushare.net", "filecosy.com", "monsteruploads.eu", "vidhuge.com", "doneshare.com", "cixup.com", "animegoon.com", "supermov.com", "ufliq.com", "vidreel.com", "deditv.com", "supershare.net", "shareshared.com", "uploadville.com", "fileserver.cc", "bebasupload.com",
                "savefile.ro", "ovfile.com", "divxbase.com", "gptfile.com", "eyvx.com", "farshare.to", "azsharing.com", "freefilessharing.com", "elitedisk.com", "freakmov.com", "cloudnator.com", "filesavr.com", "saveufile.in.th", "migahost.com", "fastfreefilehosting.com", "files2k.eu", "shafiles.me", "jalurcepat.com", "divload.org", "refile.net", "oron.com", "wupload.com", "filesonic.com", "xxlupload.com", "cumfox.com", "pyramidfiles.com", "nahraj.cz", "jsharer.com", "annonhost.net", "filekeeper.org", "dynyoo.com", "163pan.com", "imagehost.org", "4us.to", "yabadaba.ru", "madshare.com", "diglo.com", "tubeload.to", "tunabox.net", "yourfilehost.com", "uploadegg.com", "brsbox.com", "amateurboobtube.com", "good.net", "freeload.to", "netporn.nl", "przeklej.pl", "alldrives.ge", "allshares.ge", "holderfile.com", "megashare.vnn.vn", "link.ge", "up.jeje.ge", "up-4.com", "cloudcache",
                "ddlanime.com", "mountfile.com", "platinshare.com", "megavideo.com", "megaupload.com", "megaporn.com", "zshare.net", "uploading4u.com", "megafree.kz", "batubia.com", "upload24.net", "files.namba.kz", "datumbit.com", "fik1.com", "fileape.com", "filezzz.com", "imagewaste.com", "fyels.com", "gotupload.com", "sharehub.com", "sharehut.com", "filesurf.ru", "openfile.ru", "letitfile.ru", "tab.net.ua", "uploadbox.com", "supashare.net", "usershare.net", "skipfile.com", "10upload.com", "x7.to", "uploadking.com", "uploadhere.com", "fileshaker.com", "vistaupload.com", "groovefile.com", "enterupload.com", "xshareware.com", "xun6.com", "yourupload.de", "youshare.eu", "mafiaupload.com", "addat.hu", "archiv.to", "bigupload.com", "biggerupload.com", "bitload.com", "bufiles.com", "cash-file.net", "combozip.com", "duckload.com", "exoshare.com", "file2upload.net", "filebase.to",
                "filebling.com", "filecrown.com", "filefrog.to", "filefront.com", "filehook.com", "filestage.to", "filezup.com", "fullshare.net", "gaiafile.com", "keepfile.com", "kewlshare.com", "lizshare.net", "loaded.it", "loadfiles.in", "megarapid.eu", "megashare.vn", "metahyper.com", "missupload.com", "netstorer.com", "nextgenvidz.com", "piggyshare.com", "profitupload.com", "quickload.to", "quickyshare.com", "share.cx", "sharehoster.de", "shareua.com", "speedload.to", "upfile.in", "ugotfile.com", "upload.ge", "uploadmachine.com", "uploady.to", "uploadstore.net", "vspace.cc", "web-share.net", "yvh.cc", "x-files.kz", "oteupload.com", "vidabc.com", "catshare.org", "javmon.com", "xtwisted.com", "sharenxs.com", "anyfiles.pl", "nowvideo.to", "rapidshare.ru", "watchgfporn.com", "mygirlfriendporn.com", "wrzuta.pl", "jizzhut.com", "fistfast.com", "sexoquente.tv", "imgchili.com", "sexix.net",
                "sexvidx.tv", "pornimagex.com", "porndreamer.com", "wickedcloud.io", "filehd.host", "hulkimge.com", "cloudlocker.biz", "ayefiles.com", "nowvideo.pw", "mov-world.net", "vidzi.tv", "vidzi.si", "vidzi.cc", "uploads4u.net", "zippy4share.com", "beeload.com", "big4shared.com", "howfile.com", "flix555.com", "amonshare.com", "cloudshare.to", "1linx.net", "cloudyfiles.me", "cloudyfiles.co", "cloudyfiles.com", "cloudyfiles.net", "cfiles.me", "cfiles.co", "cfiles.com", "cfiles.net", "businessnews.online", "businessnewslive2018.online", "businessnewscurrent.online", "3rbup.com", "srfiles.com", "sfiles.com", "suprafiles.co", "suprafiles.net", "suprafiles.org", "suprafiles.me", "userscdn.com", "gorillavid.in", "movpod.in", "daclips.in", "epload.co", "invitationfile.com", "upload.com.ua", "file555.com", "usaupload.net", "exclusiveloader.com", "exclusivefaile.com", "bankupload.com",
                "relink.us", "relink.to", "foldr.us", "mir.to", "raptu.com", "rapidvid.to", "rapidvideo.com", "playernaut.com", "kbagi.com", "kumpulbagi.id", "kumpulbagi.com", "pixup.us", "imgserve.net", "dbr.ee", "openload.co", "openload.io", "oload.co", "oload.tv", "oload.stream", "oload.site", "oload.services", "oload.download", "oload.info", "oload.fun", "oload.club", "oload.press", "ol.link", "oladblock.me", "openloads.co", "oload.biz", "oload.life", "oload.website", "oload.best", "oload.vip", "fruithosts.net", "streamango.com", "streamangos.com", "streamcherry.com", "verystream.com", "verystreams.com", "verystream.co", "verystream.pro", "verystream.icu", "woof.tube", "pornpillow.com", "hostingrill.co", "uploadion.com", "monkifiles.com", "vshare.io", "pobierz.to", "downtobox.com", "ssh.yt", "picstream.tv", "weshare.me", "downace.com", "borncash.org", "vipbill.ru", "byzoo.org",
                "imgshot.com", "bitster.cz", "bitster.sk", "idownload.club", "streamcloud.eu", "debrid.pl", "clipsage.com", "upfile.mobi", "dopefile.com", "dopefile.me", "dopefile.pk", "fileup.st9.net", "free-sex-video.net", "hotclips24.com", "downflow.net", "drawcrowd.com", "drop.me", "dailyfiles.net", "estream.to", "estream.xyz", "firedrop.com", "frd.li", "freakshare.com", "freakshare.net", "gufiles.com", "hard55.com", "hamicloud.net", "filetrip.net", "spanktank.xxx", "birdload.com", "filetonet.com", "rapidshare.de", "gayfall.com", "k.to", "keek.com", "moviki.ru", "doperoms.com", "onecloud.media", "pururin.us", "audiorok.com", "debfile.com", "dwnup.com", "easyvid.org", "tubesss.com", "elsfile.com", "filemass.net", "killerleaks.com", "uploadstube.de", "upnito.sk", "imgtrex.com", "redfile.eu", "keeload.com", "lenfile.com", "vidzi.net", "geupload.com", "intoload.com", "vidlockers.ag",
                "vidlockers.com", "audioinbox.com", "orangefiles.me", "yourfilelink.com", "theisozone.com", "yousuwp.com", "zippyloads.com", "sonifiles.com", "rocketshare.com", "dropmega.com", "megaupload2.com", "vid.ag", "sharex.space", "sharerepo.com", "vidgot.com", "vidwatch.me", "vidwatch3.me", "vidload.net", "ydownload.org", "ishareupload.com", "videonest.net", "up07.me", "up08.net", "up09.com", "hd-arab.com", "imgfrog.pw", "imgfrog.cf", "toofile.com", "owndrives.com", "fileshd.net", "gulfup.me", "gulfup.co", "pornalized.com", "webfilehost.com", "xaxtube.com", "toplexil.com", "peredayka.com", "hellupload.com", "celeb-sex-videos.com", "cfnmhdtube.com", "chocolatesistaz.com", "cluborgasm.com", "videogirls.tv", "adultvideoarcade.com", "beam.pro", "befuck.com", "uploadmirrors.com", "uploadmirrors.org", "sharecash.org", "jafiles.net", "xshare.club", "creampiewomen.com",
                "cougarfuckclub.com", "oneload.xyz", "oneload.co", "ero-tik.com", "pornsharing.com", "fastshare.org", "dropcanvas.com", "come2store.com", "imgtiger.com", "imgdino.com", "fileflyer.com", "akatsuki-subs.net", "mega-otr.de", "pornsexwank.com", "allnetcorp.com", "nofile.io", "prochan.com", "freepornvideo.me", "upload2.com", "xxxkingtube.com", "qiannao.com", "uploadocean.com", "tunescoop.com", "udrop.net", "upload-earn.com", "uberupload.net", "xfiles.io", "void.cat", "idtbox.com", "easyfilecloud.com", "dramafever.com", "netdisk.sk", "tu.tv", "trilulilu.ro", "picsee.net", "uplod.ws", "uplod.org", "uploadshub.com", "upload.so", "fxpan.com", "linx.li", "upload.cd", "xfig.net", "vidtome.co", "vidtome.stream", "vidto.me", "vidto.se", "imgant.com", "sendvid.net", "vev.io", "thevideo.me", "thevideo.cc", "vev.red", "vidup.io", "vidup.me", "vidup.tv", "vidop.icu", "upwap.ru",
                "snowfiles.com", "oboom.com", "nzblord.com", "fileshark.pl", "depofile.info", "dbupload.co", "dbupload.in", "luckfile.com", "boostfiles.net", "liveleak.com", "backin.net", "imagezilla.net", "filemia.co", "xtube.com", "imageteam.org", "imgstudio.org", "ge.tt", "datafilehost.com", "easylinkz.net", "jetload.net", "uploadship.com", "inclouddrive.com", "speed-down.org", "4downfiles.co", "4downfile.org", "4downfiles.org", "4downfiles.com", "4downfiles.net", "lunaticfiles.com", "dbree.co", "koofile.com", "up.media1fire.com", "fileup.cc", "sfiles.org", "share-online.to", "ozofiles.com", "public.upera.co", "proxy.nsanedown.com", "rapidu.net", "vidcloud.ru", "vcstream.to", "vidcloud.ru", "pornyeah.com", "vidlox.me", "vidlox.tv", "vivo.sx", "vivo.st", "dateiload.com", "streamon.to", "yunpan.cn", "file-space.org", "freefile.me", "damimage.com", "imagedecode.com", "dimtus.com",
                "go-upload.com", "pieshare.me", "oxycloud.com", "overthumbs.com", "saruch.co", "uploadit.eu", "uploadbox.co", "vinload.com", "freaktab.org", "freshfile.pl", "intoupload.net", "duckstream.co", "share2win.xyz", "turbogb.com", "saikocloud.ml", "storex.cc", "sufile.com", "up-myfiles.com", "uploads.mobi", "uploadproper.com", "uploadproper.net", "viperfile.com", "filepup.net", "down4files.com", "faststore.org", "mon-partage.fr", "doraupload.com", "fastix.ru", "play.fm", "fastdrive.io", "imgbabes.com", "vpornvideos.com", "anavidz.com", "anavids.com", "imgflare.com", "imgbabes.com", "vupload.com", "vup.to", "filecad.com", "pornwild.to", "pornwild.com", "pornwild.su", "evilhub.com", "datoporn.com", "dato.porn", "filebit.net", "file4.net", "sexzindian.com", "bin.ge", "datator.cz", "dosyashare.com", "dosya.tv", "downster.net", "dunshare.com", "dropdoc.ru", "dropmyfiles.com",
                "dropupload.com", "easyupload.net", "anzfile.net", "eyesfile.ca", "upvideo.to", "fakirdebrid.com", "fakirserver.info", "2file.win", "filelox.com", "filezip.cc", "filetitle.com", "zippyshare.com", "stiahnito.sk", "hubic.com", "egyupload.com", "erai-ddl3.info", "evoload.io", "vishare.pl", "wrzucajpliki.pl", "laraslevelbase.org", "kvid.org", "file4safe.com", "highload.to", "embedo.co", "loadit.io", "zupload.me", "sabercathost.com", "picflash.org", "megaload.co", "cornfile.com", "retro.sx", "videobin.co", "upload.mobi", "uploadmx.com", "woodrocket.com", "sunexenus.com", "upload24x7.com", "userload.co", "yourfilestorage.com", "afile.cloud", "ninjastream.to", "batshare.net", "gottanut.com", "linkbypass.online", "zeroshare.me", "ur-files.com", "linkdrop.net", "wplik.com", "expfile.com", "modelhub.com", "pandafiles.com", "ishare.iask.sina.com.cn" };
        for (final String singleDomainHost : singleDomainHosts) {
            ret.add(new String[] { singleDomainHost });
        }
        ret.add(new String[] { "uploaded.to", "uploaded.net", "ul.to" });
        ret.add(new String[] { "anonfiles.com", "anonfile.com", "myfile.is" });
        ret.add(new String[] { "bayfiles.com", "megaupload.is" });
        ret.add(new String[] { "letsupload.cc" });
        ret.add(new String[] { "openload.cc" });
        ret.add(new String[] { "hotfile.io" });
        ret.add(new String[] { "megaupload.nz" });
        ret.add(new String[] { "upload.st" });
        ret.add(new String[] { "sockshare.ws", "putlocker.ws", "vodu.ch", "watchfreeinhd.com" });
        ret.add(new String[] { "yunfile.com", "filemarkets.com", "yfdisk.com", "needisk.com", "5xpan.com", "dix3.com", "dfpan.com", "pwpan.com", "srcpan.com", "skpan.com", "gmpan.com", "tadown.com", "putpan.com", "fourpan.com" });
        ret.add(new String[] { "flashx.net", "flashx.tv", "flash-x.tv", "flashx.pw", "flashx.co", "flashx.cc", "flashx.to" });
        ret.add(new String[] { "hellspy.cz", "hellspy.sk", "hellspy.com" });
        ret.add(new String[] { "hellshare.com", "hellshare.sk", "hellshare.hu", "hellshare.de", "hellshare.cz", "hellshare.pl" });
        ret.add(new String[] { "kufile.net", "onstclouds.com" });
        ret.add(new String[] { "streamzz.to", "streamz.ws", "streamz.cc", "streamz.bz", "streamz.vg" });
        ret.add(new String[] { "rockfile.co", "rockfile.eu", "rockfileserver.eu", "rfservers.eu" });
        ret.add(new String[] { "vidhd.co", "vidhd.me" });
        ret.add(new String[] { "xfpan.cc", "upfilex.com" });
        ret.add(new String[] { "rarbg.to", "rarbg.com", "rarbgproxied.org", "rarbgproxy.org", "rarbgp2p.org", "proxyrarbg.org", "rarbgaccessed.org", "rarbgaccess.org", "rarbgget.org", "rarbggo.org", "rarbg.is", "rarbgmirror.com", "rarbgmirrored.org", "rarbgmirror.org", "rarbgprx.org", "rarbgto.org", "rarbgunblock.com", "rarbgway.org" });
        ret.add(new String[] { "streamlare.com", "slmaxed.com", "slwatch.co", "sltube.org" });
        ret.add(new String[] { "filerio.in", "filerio.com", "filekeen.com" });
        ret.add(new String[] { "fileupup.com" });
        ret.add(new String[] { "longfiles.com" });
        ret.add(new String[] { "fas.li", "likn.xyz", "sloomp.space" });
        ret.add(new String[] { "indianpornvideos.com", "indianpornvideos2.com" });
        ret.add(new String[] { "uptobox.com", "uptobox.fr", "uptobox.eu", "uptostream.com", "uptostream.fr", "uptostream.eu", "uptobox.link" });
        ret.add(new String[] { "myvi.ru" });
        ret.add(new String[] { "easybytez.com", "easybytez.me", "easybytez.eu", "easybytez.co", "easybytez.to", "zingload.com", "easyload.to", "ezbytez.com", "ebytez.com" });
        ret.add(new String[] { "stahomat.cz", "stahomat.sk" });
        ret.add(new String[] { "storjshare.io" });
        ret.add(new String[] { "letsupload.io", "letsupload.org", "letsupload.to", "letsupload.co" });
        ret.add(new String[] { "streamhide.to", "streamhide.com", "guccihide.com", "guccihide.store", "streamhide.com", "streamtb.me", "louishide.com", "ahvsh.com" });
        ret.add(new String[] { "smoozed.com" });
        ret.add(new String[] { "asmfile.com", "filetut.com" });
        ret.add(new String[] { "chedrives.com", "chedrive.net", "chedrive.com" });
        ret.add(new String[] { "sama-share.com", "samaup.co", "samaup.com", "samaup.cc" });
        ret.add(new String[] { "file2btc.com" });
        ret.add(new String[] { "subscene.com" });
        ret.add(new String[] { "filesonic.me" });
        ret.add(new String[] { "games-database.com", "gamesdatabase.net" });
        ret.add(new String[] { "global-files.net" });
        ret.add(new String[] { "free18.net" });
        ret.add(new String[] { "restfilee.com", "restfile.ws", "restfile.ca", "restfile.co", "restfile.com", "restfile.bz", "restfile.cc", "restfile.net" });
        ret.add(new String[] { "mp4player.site", "streamembed.com" });
        ret.add(new String[] { "load.to" });
        ret.add(new String[] { "przeslij.com" });
        ret.add(new String[] { "archivos.me", "archivos.club" }); // 2024-06-25
        ret.add(new String[] { "onuploads.com" });
        ret.add(new String[] { "uploadbuzz.cc", "uploadbuzz.net", "uploadbuzz.org" });
        ret.add(new String[] { "pornxs.com", "videarn.com" }); // 2024-07-10
        ret.add(new String[] { "xueqiupan.com" }); // 2024-07-16
        ret.add(new String[] { "upload.ac" });
        ret.add(new String[] { "uploadcorner.com" });
        ret.add(new String[] { "uplod.net" });
        ret.add(new String[] { "abload.de" });
        ret.add(new String[] { "saikoanimes.net" });
        ret.add(new String[] { "streamhub.to", "streamhub.gg" });
        ret.add(new String[] { "gounlimited.to" });
        ret.add(new String[] { "uploadbank.com" });
        ret.add(new String[] { "datpiff.com" });
        ret.add(new String[] { "solidfiles.com" });
        ret.add(new String[] { "filehorst.de" });
        ret.add(new String[] { "veoh.com" });
        ret.add(new String[] { "vdisk.cn" });
        ret.add(new String[] { "freehardcore.com" });
        ret.add(new String[] { "safestream.cc" });
        ret.add(new String[] { "126disk.com", "126xy.com", "126xiazai.com" });
        ret.add(new String[] { "qqupload.com" });
        ret.add(new String[] { "femdomrip.com" });
        ret.add(new String[] { "camhoes.tv" });
        ret.add(new String[] { "sincity.is", "goodporn.to", "goodporn.se" });
        ret.add(new String[] { "heroupload.com", "heroupload.us" });
        ret.add(new String[] { "2shared.com" });
        ret.add(new String[] { "567yun.cn", "567file.com", "567pan.cn" });
        ret.add(new String[] { "userupload.net", "proapk.net" });
        ret.add(new String[] { "sharebit-upload.com" });
        ret.add(new String[] { "transfer.sh" });
        ret.add(new String[] { "thisav.com" });
        ret.add(new String[] { "rosefile.net", "rsfile.cc" });
        ret.add(new String[] { "wyslij.net" });
        ret.add(new String[] { "uploadraja.com" });
        ret.add(new String[] { "uploadingsite.lol" });
        ret.add(new String[] { "uploadify.net" });
        ret.add(new String[] { "uploadfox.net" });
        ret.add(new String[] { "gamovideo.com" });
        ret.add(new String[] { "youwatch.org", "sikafika.info", "voodaith7e.com", "gh1d4fr.host" });
        ret.add(new String[] { "xunniuyun.com", "xunniufxpan.com", "xunniuwp.com", "xunniufxp.com", "xunniu-pan.com", "xunniufile.com", "xunniupan.co", "xunniupan.com", "xun-niu.com" });
        ret.add(new String[] { "filepro.live", "akmfiles.com", "akmfls.xyz" });
        ret.add(new String[] { "cloudcapcity.com" });
        ret.add(new String[] { "deimos.click", "phobos.click" });
        ret.add(new String[] { "demonvideo.top" });
        ret.add(new String[] { "avgle.com", "avgle.io" });
        ret.add(new String[] { "androidhost.ru" });
        ret.add(new String[] { "bitporno.to", "bitporno.sx", "bitporno.com" });
        ret.add(new String[] { "bit-shares.com" });
        ret.add(new String[] { "docplayer.net" });
        ret.add(new String[] { "blazingshare.me" });
        ret.add(new String[] { "filezz.me" });
        ret.add(new String[] { "ebalka.nl", "ebalka.fun" });
        ret.add(new String[] { "pornerbros.com", "givemegay.com" });
        ret.add(new String[] { "highstream.tv", "clipwatching.com" });
        ret.add(new String[] { "hostmy.de" });
        ret.add(new String[] { "vivud.com" });
        ret.add(new String[] { "groovesharing.com", "groovestreams.com", "01files.me" });
        ret.add(new String[] { "dbree.org", "dbree.me" });
        { /* 2025-06-02: Domains that used to be in plugin "ImagemazeCom" START */
            ret.add(new String[] { "imgmaze.com", "imgmaze.pw", "imgmaze.co", "meetimgz.com" });
            ret.add(new String[] { "imgtown.net", "imgtown.co", "pictwn.com" });
            ret.add(new String[] { "outletpic.com", "imgoutlet.com", "imgoutlet.pw", "imgoutlet.co" });
            ret.add(new String[] { "tezzpic.com", "imgrock.net", "imgrock.info", "imgrock.co", "imgrock.pw", "picrok.com", "piczhq.com" });
            ret.add(new String[] { "imgdew.com", "dewimg.com" });
            ret.add(new String[] { "imgview.net", "imgview.pw", "imgview.co" });
        }
        ret.add(new String[] { "megafiles.io", "megafile.cc" });
        ret.add(new String[] { "istream.lol" });
        ret.add(new String[] { "micloudfiles.com" });
        ret.add(new String[] { "mrdeepfakes.com" });
        /* MyzukaClubCrawler */
        ret.add(new String[] { "myzuka.club", "myzuka.ru", "myzuka.org", "myzuka.fm", "myzuka.me" });
        {/* NippyShareCom */
            ret.add(new String[] { "nippyshare.com" });
            ret.add(new String[] { "nippyspace.com" });
            ret.add(new String[] { "nippyfile.com" });
            ret.add(new String[] { "yolobit.com" });
        }
        ret.add(new String[] { "pornfile.ulozto.net", "pinkfile.cz", "pornfile.cz" }); /* UlozTo */
        ret.add(new String[] { "oboom.io" });
        ret.add(new String[] { "oceanoffile.com" });
        ret.add(new String[] { "orgasm.com" });
        ret.add(new String[] { "streamvid.net" });
        ret.add(new String[] { "metaraid.io" });
        ret.add(new String[] { "oxy.cloud", "oxy.st" });
        ret.add(new String[] { "oydisk.com" });
        ret.add(new String[] { "poophd.com", "do0d.co", "pooop.online", "poop.com.co" });
        ret.add(new String[] { "cyphershare.net" });
        ret.add(new String[] { "hotshag.com" });
        /* 2025-07-22: ServePornCom domains, some of them now redirect to superporn.com */
        ret.add(new String[] { "serveporn.com", "seansporno.com", "einfachporno.com", "vielerporno.com", "pornozot.com", "voglioporno.com", "bubbaporn.com", "pornodrome.tv", "nedporno.com", "pornjam.com", "canalporno.com", "prendiporno.com", "prendiporno.tv", "guterporn.com", "guterporn.xxx", "pornalia.xxx", "bundesporno.xxx", "hierporno.com", "pornburst.xxx", "gauleporno.xxx", "pornoheit.com", "garotaporno.com" });
        ret.add(new String[] { "ronemo.com", "ronemo.net" });
        ret.add(new String[] { "spicyfile.com" });
        if (cache != null) {
            cache.put(cacheID, ret);
        }
        return ret;
    }

    public static String[] getAnnotationNames() {
        return buildAnnotationNames(getPluginDomains());
    }

    @Override
    public String[] siteSupportedNames() {
        return buildSupportedNames(getPluginDomains());
    }

    public static String[] getAnnotationUrls() {
        return buildAnnotationUrls(getPluginDomains());
    }

    public static String[] buildAnnotationUrls(final List<String[]> pluginDomains) {
        final List<String> ret = new ArrayList<String>();
        for (final String[] domains : pluginDomains) {
            ret.add("https?://(?:[A-Za-z0-9\\-]+\\.)?" + buildHostsPatternPart(domains) + "/.+");
        }
        return ret.toArray(new String[0]);
    }

    @Override
    public boolean checkLinks(final DownloadLink[] items) {
        if (items == null) {
            /* Early return */
            return true;
        }
        for (final DownloadLink link : items) {
            link.setAvailable(false);
            if (StringUtils.isEmpty(link.getComment())) {
                link.setComment(getErrorMessage(link, null));
            }
        }
        return true;
    }

    @Override
    public boolean isPremiumEnabled() {
        return false;
    }

    private String getErrorMessage(final DownloadLink link, final Account account) {
        return "Permanently Offline: Website " + getHost(link, account, true) + " no longer exists!";
    }

    @Override
    public String getHost(final DownloadLink link, final Account account, boolean includeSubdomain) {
        if (link != null) {
            return Browser.getHost(link.getPluginPatternMatcher(), includeSubdomain);
        } else if (account != null) {
            return account.getHoster();
        } else {
            return super.getHost(link, account, includeSubdomain);
        }
    }

    @Override
    public AccountInfo fetchAccountInfo(final Account account) throws Exception {
        throw new PluginException(LinkStatus.ERROR_PREMIUM, getErrorMessage(null, account), PluginException.VALUE_ID_PREMIUM_DISABLE);
    }

    @Override
    public String getAGBLink() {
        return null;
    }

    @Override
    public boolean isResumeable(DownloadLink link, Account account) {
        return false;
    }

    @Override
    public void handleFree(final DownloadLink link) throws Exception {
        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, getErrorMessage(link, null));
    }

    @Override
    public AvailableStatus requestFileInformation(DownloadLink link) throws Exception {
        throw new PluginException(LinkStatus.ERROR_FILE_NOT_FOUND, getErrorMessage(link, null));
    }

    @Override
    public void reset() {
    }

    @Override
    public void resetDownloadlink(DownloadLink link) {
    }

    @Override
    public boolean hasCaptcha(DownloadLink link, jd.plugins.Account acc) {
        return false;
    }
}