package com.smart.util;

import java.util.Date;
import java.util.Random;

/**
 * 生成工具类 [单据、编码等]
 * 
 * @author Joe
 */
public class GeneratorUtils {

	/**
	 * 获取: 时间戳 + n位随机数
	 * 
	 * @return
	 */
	public static String getTimeStampRandom() {
		return getTimeStampRandom(0);
	}

	/**
	 * 随机指定范围内N个不重复的数 最简单最基本的方法
	 * 
	 * @param min
	 *            指定范围最小值
	 * @param max
	 *            指定范围最大值
	 * @param n
	 *            随机数个数
	 */
	public static int[] randomCommon(int min, int max, int n) {
		if (n > (max - min + 1) || max < min) {
			return null;
		}
		int[] result = new int[n];
		int count = 0;
		while (count < n) {
			int num = (int) (Math.random() * (max - min)) + min;
			boolean flag = true;
			for (int j = 0; j < n; j++) {
				if (num == result[j]) {
					flag = false;
					break;
				}
			}
			if (flag) {
				result[count] = num;
				count++;
			}
		}
		return result;
	}

	public static String getTimeStampRandom(int len) {
		StringBuffer sb = new StringBuffer();
		sb.append(getTimeStamp());
		if (len > 0) {
			sb.append(getRandom(len));
		}
		return sb.toString();
	}

	/**
	 * 获取时间戳
	 * 
	 * @return
	 */
	public static String getTimeStamp() {
		return DateUtils.format(new Date(), "yyyyMMddHHmmssSSS");
	}

	/**
	 * 获取随机数 (由纯数字组成)
	 * 
	 * @param len
	 * @return
	 */
	public static String getRandom(int len) {
		StringBuffer sBuffer = new StringBuffer();
		Random random = new Random();
		for (int i = 0; i < len; i++) {
			sBuffer.append(random.nextInt(10));
		}
		return sBuffer.toString();
	}

	/**
	 * <b>Description:</b>获取随机数(由大小写字母和数字混合组成) <br>
	 */
	public static String getRandomCharAndNum(int length) {
		String val = "";
		Random random = new Random();
		for (int i = 0; i < length; i++) {
			String charOrNum = random.nextInt(2) % 2 == 0 ? "char" : "num"; // 输出字母还是数字
			if ("char".equalsIgnoreCase(charOrNum)) { // 字母
				int choice = random.nextInt(2) % 2 == 0 ? 65 : 97; // 取得大写字母还是小写字母
				val += (char) (choice + random.nextInt(26));
			}
			else if ("num".equalsIgnoreCase(charOrNum)) { // 数字
				val += String.valueOf(random.nextInt(10));
			}
		}
		return val;
	}
}
