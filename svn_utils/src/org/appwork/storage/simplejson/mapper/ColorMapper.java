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
package org.appwork.storage.simplejson.mapper;

import java.awt.Color;

import org.appwork.storage.simplejson.JSonNode;
import org.appwork.storage.simplejson.JSonValue;

/**
 * @author thomas
 * 
 */
public class ColorMapper extends TypeMapper<Color> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.appwork.storage.simplejson.mapper.TypeMapper#obj2Json(java.lang.Object
     * )
     */
    @Override
    public JSonNode obj2Json(Color c) {
      
        StringBuilder sb = new StringBuilder();
        sb.append("#");
        String hex;
        int length;
        if (c.getAlpha() == 255) {
            hex = Integer.toHexString(c.getRGB() & 0x00ffffff);
            length = 6;
        } else {
            hex = Integer.toHexString(c.getRGB() & 0xffffffff);
            length = 8;
        }
        for (int i = 0; i < length - hex.length(); i++) {
            sb.append("0");
        }
        sb.append(hex);
        return new JSonValue(sb.toString());

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.appwork.storage.simplejson.mapper.TypeMapper#json2Obj(org.appwork
     * .storage.simplejson.JSonNode)
     */
    @Override
    public Color json2Obj(JSonNode json) {
        // TODO Auto-generated method stub
        String str = getString(json);
        if(str.length()>7){
            return new Color(Integer.parseInt(str.substring(1), 16),true);
        }else{
            return new Color(Integer.parseInt(str.substring(1), 16),false);
        }
    
        
   
    }

}
