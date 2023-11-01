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
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.tests;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.appwork.testframework.AWTest;
import org.appwork.utils.ZipContentHasher;
import org.appwork.utils.formatter.HexFormatter;

/**
 * @author thomas
 * @date 20.12.2021
 *
 */
public class ZipContentHasherTest extends AWTest {
    public static final String BYTES_TEST_ZIP = "504b03040a00000808001b744d5130325447c00f00004612000015001400435353656c66546573745061636b6167652e6a6172010010004612000000000000c00f0000000000008d987754535b16c643915ea4434090128a529426109a106a00455aa81230841e7a171e8f2e2154e9a0a0a0f42a52024a68d2bb7410912a48280f22a08c336fe695598f37b3ef3a6b9d73d7f7fdf6bdebfcf5edbb5012522a00800200e0f4d13704fca12801a4007d0de3db623a069a12dffb0080bbff965200803fa4db9c4741493f64753f16f38ff59b54ffb6818ea68691b1b8bea63ea6950a778339ea58759157222989e7d2a6297863ac44639d1d2553afafc6c8bcb4789d4ee1cad0b1e787435902a5eeaad0bbae9e911b2001901b81dfadf60b1d0d872ea80b93221e51e1b17a3aa5f5c27b400a5c9eb1328c0e8d8a762a294ae97b99306aa9ae837af861c10eea041bb805370afa68dac84edc788dea538cff4f4d7a2ca8b8656ab26d7e3f5de14092bb17fe2ce98f85f2424a5caca0f9b702eee1e18ff272f91b25f37f29ed51eeee087b9fbf71705de0f04678f939d923fec609fa1f4e6f84ab830fc2fbf7e614ff4278ce30a8bfa50500083c0080c2ff8b30faf585d18fb3f18fb3b8bd2bdcdb3bcf9c32846926f03c2c7d3bf953bbad37152b9f383f89663cade8746482587f3cebccbe0571159c4dea9d7da06a3ec3b3e4845b3d0e75aeb38239656eb3824ea9b23d63b2889ca2f2445fbf9906a79c675a189bcd0569fdaa9d857a7d75bfd3560eb880a86b78d8f7c1f1e0a5bdb6dde0a55f1cdace4ea0558069addda3026e3301c55d7596ca80bd7e8295988a4d92c764c4fdbd02b2296c0083d1abb3ccf41bfcf0f7e41c123a99071f0f02666214b07ccbcd57c3267baebfd9eaa85a9e0285dcc6a1b409fa3cef3fbe0e21f0b5092cf72b97f79875aebc56225ce109657b2374f57e45d714def7e6b7e8c8a563787cd20a3d3ec6ae5f2505dff34af16b71a44a32fe0531615df925815ae2670254e23b55d84ec76bfc2d9b10ceb0ebcb60e5f9c4ebdc6779ece78bf1c7cb3c9d5561cc8961fbcb326775e4e77ce9e79311614c2b6f82f13c766777e8bf43d14b8ff14bdc6789f4e7f1bb3a0415fab65d75f62981797c21a70a9e173315552a11b7c9692340e92ace17571ab201af4b6a010ef1487496683ed5660cc4d220ceb8f7753f1dc00166a4d615310f78c49e29d08b21399ca262b638b469b92d7ab7c6afd1571cc1692bf696e7ea4d0b7d23b33bb22bcdb5282a6dc7297f06bbd29445aeded4370b3eb0479a4895e40ee275d507b445b2981e95c405c247dd129ce571e199f062755d59303c735d5d447d6a328da5af64ad31fd5ee0001ae63c4405ca8d221d4e55440995eb95998ec9556d193eecbcd1c30c95f2893b139d1cc52c56c222af6a6565710c0937e5f681411135de7d8d8c7dc2410a0faa1ab4ec45ec45d3fa3f17783733f6393043aba9f41d835ca08eaae3c1a9735c0e3bfaa04ccafe8117e5d8925aae2f8ad45058b016a91dc28ccd2d8f4b2681124e123df93c922b98f217ea074cacd90392621a9465b008aeebd348a0c877c4e70535c75258280ef859d6becb58c92d8191b8484f548a79b262a837d4172263a204ea2c97341a787ac86cde80d3694ed1693ff7a3a53bd867201385c274b74212535e8dca63771ccf88db7346c69af9ca2172de72337ee4d4dce61eee74eb2d728a96aac799859ba248bc3183caa2e6f8f80eeee1c35e428df6eec81ee4f99311acf481e5c680d5bd7b22fc55968c15eb3a3152bcdbd668fd1e7c63caa48525c6d2c2c1321c428008ce95a16bf44b0da2b2ccaec96ce9c50ddb9adf0f3b8448b3060a4ba6c57abc5d7a539f0962c6d1646165079699c0dc3b40bb531a5f81d46fa3432bbdda6a909594b3be11ca027d998ce1eaee7ef0ca1e2cf9f8b245c68799228c6b4b823624a15b2845e695a7d8b3620d42a5c7b65295819dd3d0418c4f7cacfc14422bbe23d67ed83395b1e0d864cbaae85e0b8bd4606da5e88a9dd51d395f2841fa334b53e6bd3a3e326f46b7a40d651b5df640a9223f17bea7a60be93b47140e929a4982ae168a30d0acf0b86050aa1ed1ac1e2c0c8a0e3d1277d3cb5bcabe8cb11af0aa358921a117d32b22eaa7400b5b08de28307f16ae1d16860d139ecd915b5cb842abc5dfda5b60f09d631359e2180fb1ec2a9e7b306b9010e9169f55f90a64971dddabe96c318f43071819d7f4d2f955cc8dfd04b99d957d8377dc60b68bce8b9a4b2518c392ea744ba66f422f5880aad747c533dfda5ab6071ff7b87738b2a2eb954e9e4afd886a6d0ba49135ce0231ab752acf17af23a9a93088545904f639d9cfa081bad3eae80c933a4b1df368799171a220d5e2089fd88ee03b057899f84221c31cbd9d7a8d6e819d8c8d6a3d5431bfa492368ab77ebe35587782d7a0c4a8f886afe7ecfd47117a0c5b82dbcdf1106d3918af5ffd6bf0e2d27832ba3b3806578321d53a9805ab76def4eb65efb894c55df81242dfef833e1108821487ea6a77a6700b197c704125360d61d33958bb9a2a93f23446a9ea2f3d003ed542003b9f137cef151b1d664212fb7db21b3ccc81b290a2eabde80d9af58664ea213d06299d7557d73bc38f4a72c7a3f316b65fa5c1e6acb6490287be342525336dd17332c927efcc9c2ac5adb50e9ee61c73a739c2d658e1cf3bc9e30beb41f2a5645494bc4f57899be19706b095f1571e5b5de57fe475f55656dd448d6a7b70e397795fbb3411fd6792486b4ca6b78f4d310d87246293183ebc30c91e37fd119c3ef4d59a642ff4a8c33998d9302db597cf5dfb68014a9b2222e1987117ab5791ec942bd8e8fba5c5dc659698d19a59f966af89b080e227caa1e1f658712f5cb5dca785433f3299de43baa01429af69278b1cea9889795406ae01a20c7a84d6db855814bf8090cdf94bb47e49e9cea5f753db109f4bc83178c151e0887db729bbe0a5512951eab4566725f4e87233dbb80002d8c4151cf25ea48edfa090c92e1660bbdee0de87f37cab019a19e574584e140f6279995bd2c89556e2482a03f3ec676f5597025a979abb2bf46a08c2da3ae69bdf7de931363293c2f1cdb131ed0b023f0bd7616e829033bf4cb445ec69b45f413fcadb820e4b88a53c05cb4ff798da741c178b3fdebd52ac30909f5367c244630105daaf338e19d7db95f273785ed5808846c5cce4dd950c3450186b645fe12fecc1b01ab1426cb713e26b57842726e38487595a28e70cf9928d26a0906ba20cef2ff5db54478ac8a649bcb38ab853ef6555478852d373a491ed626d17ff10779dd60824fd2d64cc03d8f01eccc8ba158a49bec414a39a24c9f6a404c3b17284ee77ad26a6932db8ebbc82a31a4f1e74a8376e1fffec556a525b8d706a6d8ca8b5b6872c6fafd60bab95df1bcc6da5a2b1206dcde391036a9cb66473fa2de84e77c5066f973b5b646f2cf8935f77a835ce9defe38aefd631147fb92e47b9ae4f77a76ce418ec6c12ea6bc3305c1ab49977bb69caf8deb289da738663cfd34135f5c2010db7f3313db6949a6b15c7aadd4da0025ae8a1386b49623ba3d260729099bfe228ff8dd586b1b9313bdf8a460bcfcfc3f9e6c2e679a9949e614ae6ed44b162736c4a1b60abf6f84a468c541d251a2b73ce1dd38f78b2b3995e72cdfee3497f65c596a8a977d3c41bdb92914c49b89b2152b7b14a1ab588d9aea141c655c5b647c5ee684c62b5150d06fc600ed36e2eb98ba6d894ebea3b1f35338661afefdf0a58e79cabddec2662c99e61d98ac2a6f46a8c37c34be120e4fa972870da4dda96c0859075163f8b942235efb64066417817d56acf94d961c38770a3e313c18e2bfdd4c83702a137895dee7108c1bf65c2738791ebab1e8b73b9e2c8b83e583e95d998bbabcd9855a144c07468cf24bc06803c48af4eee2c9a924cd0eb6540978a546af2bc546c5dd8e17c191cc02990e62a2cf879b554b67276771f8758792a40d32f4d57940c1a9572e5adb90e5f4f79bc5c1cc917d4c1c1ff82dbe4103e7a69bb72de51edd9aa410db384566a315fed4bcbd69735e7a82ae11a13afde16a57553dc9ce749779feb4f83770d5161f61ee19d9cd74231fba29dfc7bb0566fe0e1cf0452a2842732471ffd72575e6c5ffda028a918d475e9494d66a6f250006395924a4c69bb53635015ae3598a4291b69681d91683bc3e54cc6f9dc29a0f27e943fab32ee03d1f1232165deabfb1f9c8fa5d3ad973643397c232acb60f99938aafc09fae6ec172337221e63abb74ccaddcbdad0ab6e32de30f046555677f3e723d9efcee1abaebf189633d3a0dcf9a659dd08c91f77ce269fd83fa45ba1b90723795de1c6b9456822e126ada1d39ffe79c1f1b694fd0e02fb103d835b73890aff160024c7dbcbc53ee9e00d61dd69883ac9daa7e17c0fc1ec3cc791530736d05a92ad64560f7e5714b71b6bf87410408c425007e1b916a35f4b594be5e3c0ec5b6b57b8453d02536e92d07813da98262fb9dfd9dbb08d7be5bfe77ac5d9dd363ef205eaed1d7e46f105d58a4864436557cbcec119af721d4f77a21c72ed84dde86b40e029cd59955deb412e2e5a647eaada932cc7015f6c4e4a2135ad182316526a696f5633d8b0e6277e35dfba6e7a591c898f7e1a3cc5ec37643b554671b4dd46f61d95dfceedf76884eb0bd22098d3a278643684f6b4b83a8c8abc18afcab0fd9404dcd9410ccb21719120076fbf5db4a2f3eb0c5792a377d9b83ccee1136a4c80045417b800933fd32ec81c14fa1f37625c1f8cf264e346ef7c84fe12396cf6ba292ae274a14a8f7fd67470510b8dc777a7bbc8adf99f962119cabadf720591cd7404cd3973d9e67cca58bbcc6315d5e674f82636f24d741e587e631f1ac7e6397f9fff27654e1f3b3a12d1dd676edc6b62027b5304a53a761ffba7da59a9e5514b4a11468ab28add89396a1e998a8b1e16aa4a46ec8ca9520754f7bd46576f59107249e7cc854dd81b071856c5ccf782e5add468911ce7925b7634da932d6f942396d9a6bf9e6cc834b7b78746d8b0f1b70c655f5389c1f4d3e5257a4f03ae38aa0abde71b28188fca8bfd20b6a1c9c3ddedc4658f65cfbf4d1fa32761a3c9b5f4e09dff1e05c34909abd0bb4422e7ce6713e8db0a91a91331463e8c89a3e68a066c1ed9f99d9795e2a50ee1c2875c4363feece732e891e3248738cd52e7e4cad20ec749e003986485b3d290891bee6ba95b6e292440b95d645bbad2e31d76ca00f5a4b38e00b0c22e892ac7d2ef7972db6d247a099b1eb40d7b0584b1cb604f4167a4a1548cbba714427589edea9c78993369f2d278c477b250b227b6a160f3f42788fcce5385f03b7c58bc4848f6312b1130729dec442064ed3460931c26be2ccffaadcc133371722b3352c08a3511aa7c86cd0b2ac35313344243dfd32ddd577bff1e2eef23edb5115f359d1040dff9621db5fcedf2fd38cebd1545c1c0331d6ef2775d44d7da646fed7f0df8aa079c39b231704d2048bd66f3f43294e98cc16b2a504974056c2fd764d831af7ba2d715125108ecebf6d3f3bb50226266928b2700bfd665c0ce6dc01f43feaff380ff58ff7a22f09f7a1e7efd82f9c0df3527fd6df7cfe63bbfe5f38b1d347f72d013fd775ebfd8c9fc27a73cd145f9fd6202d79f082e17107ecff31793407f2215fd0fd21ff2fdef97f15709fff7cbd8f87f917f99f7ef422f91fd9343f1e3b941fcafaffc51ff00504b03040a0000080800b873f650687a781aa2000000fa0000000a0014006c61756e63682e62617401001000fa00000000000000a200000000000000758e310e83300c457724ee6021b1951c808ea843b74a616431910b8190444e1aae5f103074e8f4a42ffff75dd7a44607680c904d9add42364242d6d81b0a029e8b771cd1c61bf4a4f0130856821d71a405b4dd0981cc3b5288799667f2d1ee28cac6594b2a4ae2a41555e3e62ebb89a9ebb5ed264c5840a53cfc39fc0dc5847c6fa4dc76da6de7856ac6e14845018e0781deaf8e67a18e9e0867f17a4d9ca64bf105504b010214030a00000808001b744d5130325447c00f000046120000150000000000000000000000a48100000000435353656c66546573745061636b6167652e6a6172504b010214030a0000080800b873f650687a781aa2000000fa0000000a0000000000000000000000a481071000006c61756e63682e626174504b050600000000020002007b000000e5100000000049616d424144";

    public static void main(String[] args) throws IOException {
        run();

    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.tests.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        byte[] hash = ZipContentHasher.getContentSHA256("", new ByteArrayInputStream(HexFormatter.hexToByteArray(BYTES_TEST_ZIP)), new ZipContentHasher.Customizer() {

            @Override
            public boolean handle(String path, ZipInputStream zipStream, ZipEntry entry, HashMap<String, byte[]> results) throws IOException {
                if ("META-INF/MANIFEST.MF".equals(entry.getName())) {

                    results.put(path + entry.getName(), "ignoreMe".getBytes("UTF-8"));
                    return true;
                } else {
                    return false;
                }
            }
        });
        assertTrue("9c388c71496023b1ad50f3527d8a1e89b876ed2aecd17aef3034e6374b00ea46".equals(HexFormatter.byteArrayToHex(hash)), "ZipContentHasher.getContentSHA256 returned a not-expected result: " + HexFormatter.byteArrayToHex(hash));

    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.tests.PostBuildTestInterface#runPostBuildTest(java.lang.String[], java.io.File)
     */
    @Override
    public void runPostBuildTest(String[] args, File workingDirectory) throws Exception {
        runTest();

    }
}
