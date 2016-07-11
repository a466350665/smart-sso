package com.smart.tool.system;

import java.util.Random;

/**
 * 字符串公用类
 */
public class StringUtils {

	/**
	 * 验证可能为空格或者""及null的字符串
	 * 
	 * <pre>
	 *   StringUtils.isBlank(null)      = true
	 *   StringUtils.isBlank(&quot;&quot;)        = true
	 *   StringUtils.isBlank(&quot; &quot;)       = true
	 *   StringUtils.isBlank(&quot;bob&quot;)     = false
	 *   StringUtils.isBlank(&quot;  bob  &quot;) = false
	 * </pre>
	 * 
	 * @param cs
	 *            可能为空格或者""及null的字符序列
	 * @return
	 */
	public static boolean isBlank(final CharSequence cs) {
		int strLen;
		if (cs == null || (strLen = cs.length()) == 0) {
			return true;
		}
		for (int i = 0; i < strLen; i++) {
			if ((Character.isWhitespace(cs.charAt(i)) == false)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 验证可能为""或者null的字符串
	 * 
	 * @param cs
	 *            可能为空的字符串
	 * @return
	 */
	public static boolean isNotBlank(final CharSequence cs) {
		return !isBlank(cs);
	}
	
	/**
	 * 产生随机数
	 */
	public static String getRandom(int len) {
		StringBuffer sBuffer = new StringBuffer();
		Random random = new Random();
		for (int i = 0; i < len; i++) {
			sBuffer.append(random.nextInt(10));
		}
		return sBuffer.toString();
	}
}
