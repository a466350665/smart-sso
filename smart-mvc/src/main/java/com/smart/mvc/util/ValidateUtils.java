package com.smart.mvc.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 验证工具类
 * 
 * @author Joe
 */
@SuppressWarnings("static-access")
public class ValidateUtils {
	/**
	 * 是否是整数
	 * 
	 * @param value
	 */
	public static boolean isInteger(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^-?[1-9]\\d*$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是正整数
	 * 
	 * @param value
	 */
	public static boolean isUnsignedInteger(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[1-9]\\d*$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是负整数
	 * 
	 * @param value
	 */
	public static boolean isIntege2(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^-[1-9]\\d*$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是数字
	 * 
	 * @param value
	 */
	public static boolean isNum(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^([+-]?)\\d*\\.?\\d+$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是正数（正整数 + 0）
	 * 
	 * @param value
	 */
	public static boolean isNum1(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[1-9]\\d*|0$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是负数（负整数 + 0）
	 * 
	 * @param value
	 */
	public static boolean isNum2(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^-[1-9]\\d*|0$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是浮点数
	 * 
	 * @param value
	 */
	public static boolean isDecmal(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^([+-]?)\\d*\\.\\d+$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是正浮点数
	 * 
	 * @param value
	 */
	public static boolean isDecmal1(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[1-9]\\d*.\\d*|0.\\d*[1-9]\\d*$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 判断是否金额格式
	 * 
	 * @param value
	 * @return
	 */
	public static boolean isMoney(String value) {

		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^(([1-9]{1}\\d*)|([0]{1}))(\\.(\\d){0,8})?$");
		m = p.matcher(value);
		b = m.matches();
		return b;

	}

	/**
	 * 是否是负浮点数
	 * 
	 * @param value
	 */
	public static boolean isDecmal2(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^-([1-9]\\d*.\\d*|0.\\d*[1-9]\\d*)$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是浮点数
	 * 
	 * @param value
	 */
	public static boolean isDecmal3(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^-?([1-9]\\d*.\\d*|0.\\d*[1-9]\\d*|0?.0+|0)$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是非负浮点数（正浮点数 + 0）
	 * 
	 * @param value
	 */
	public static boolean isDecmal4(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[1-9]\\d*.\\d*|0.\\d*[1-9]\\d*|0?.0+|0$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是非正浮点数（负浮点数 + 0）
	 * 
	 * @param value
	 */
	public static boolean isDecmal5(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^(-([1-9]\\d*.\\d*|0.\\d*[1-9]\\d*))|0?.0+|0$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是邮件
	 * 
	 * @param value
	 */
	public static boolean isEmail(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^\\w+((-\\w+)|(\\.\\w+))*\\@[A-Za-z0-9]+((\\.|-)[A-Za-z0-9]+)*\\.[A-Za-z0-9]+$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是颜色
	 * 
	 * @param value
	 */
	public static boolean isColor(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[a-fA-F0-9]{6}$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是url
	 * 
	 * @param value
	 */
	public static boolean isUrl(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^http[s]?:\\/\\/([\\w-]+\\.)+[\\w-]+([\\w-./?%&=]*)?$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是中文
	 * 
	 * @param value
	 */
	public static boolean isChinese(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[\\u4E00-\\u9FA5\\uF900-\\uFA2D]+$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是ACSII字符
	 * 
	 * @param value
	 */
	public static boolean isAscii(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[\\x00-\\xFF]+$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是邮编
	 * 
	 * @param value
	 */
	public static boolean isZipcode(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^\\d{6}$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是手机
	 * 
	 * @param value
	 */
	public static boolean isMobile(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^1[345678][0-9]{9}$");// p.compile("^(13|15|18)[0-9]{9}$"); 1\\d{10}
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是ip地址
	 * 
	 * @param value
	 */
	public static boolean isIp(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^(25[0-5]|2[0-4]\\d|[0-1]\\d{2}|[1-9]?\\d)\\.(25[0-5]|2[0-4]\\d|[0-1]\\d{2}|[1-9]?\\d)\\.(25[0-5]|2[0-4]\\d|[0-1]\\d{2}|[1-9]?\\d)\\.(25[0-5]|2[0-4]\\d|[0-1]\\d{2}|[1-9]?\\d)$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是非空
	 * 
	 * @param value
	 */
	public static boolean isNotempty(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^\\S+$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是图片
	 * 
	 * @param value
	 */
	public static boolean isPicture(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("(.*)\\.(jpg|bmp|gif|ico|pcx|jpeg|tif|png|raw|tga)$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是压缩文件
	 * 
	 * @param value
	 */
	public static boolean isRar(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("(.*)\\.(rar|zip|7zip|tgz)$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是日期
	 * 
	 * @param value
	 */
	public static boolean isDate(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^((\\d{2}(([02468][048])|([13579][26]))[\\-\\/\\s]?((((0?[13578])|(1[02]))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])))))|(\\d{2}(([02468][1235679])|([13579][01345789]))[\\-\\/\\s]?((((0?[13578])|(1[02]))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\\-\\/\\s]?((0?[1-9])|(1[0-9])|(2[0-8]))))))(\\s(((0?[0-9])|([1-2][0-3]))\\:([0-5]?[0-9])((\\s)|(\\:([0-5]?[0-9])))))?$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是QQ号码
	 * 
	 * @param value
	 */
	public static boolean isQq(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[1-9]*[1-9][0-9]*$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是电话号码的函数(包括验证国内区号,国际区号,分机号)
	 * 
	 * @param value
	 */
	public static boolean isTel(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^(([0\\+]\\d{2,3}-)?(0\\d{2,3})-)?(\\d{7,8})(-(\\d{3,}))?$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 用来用户注册。只能包含英文字母、数字、"-"和"_"
	 * 
	 * @param value
	 */
	public static boolean isUsername(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[a-zA-Z][\u4e00-\u9fa5A-Za-z0-9-_]+$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是字母
	 * 
	 * @param value
	 */
	public static boolean isLetter(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[A-Za-z]+$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是大写字母
	 * 
	 * @param value
	 */
	public static boolean isLetter_u(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[A-Z]+$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是大写字母
	 * 
	 * @param value
	 */
	public static boolean isLetter_l(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^[a-z]+$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是价格
	 * 
	 * @param value
	 */
	public static boolean isPrice(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("^([1-9]{1}[0-9]{0,}(\\.[0-9]{0,2})?|0(\\.[0-9]{0,2})?|\\.[0-9]{1,2})$");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否是身份证号
	 * 
	 * @param value
	 */
	public static boolean isIdNo(String value) {
		Pattern p = null;// 正则表达式
		Pattern p1 = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		Matcher m1 = null;// 操作符表达式
		boolean b = false;
		boolean b1 = false;
		p = p.compile("^[1-9]\\d{7}((0\\d)|(1[0-2]))(([0|1|2]\\d)|3[0-1])\\d{3}$");
		p1 = p1.compile("^[1-9]\\d{5}[1-9]\\d{3}((0\\d)|(1[0-2]))(([0|1|2]\\d)|3[0-1])\\d{3}([0-9]|X|x)$");
		m = p.matcher(value);
		b = m.matches();
		m1 = p1.matcher(value);
		b1 = m1.matches();
		return b || b1;
	}

	/**
	 * 是否是中国人姓名
	 * 
	 * @param value
	 */
	public static boolean isPersonName(String value) {
		Pattern p = null;// 正则表达式
		Matcher m = null;// 操作符表达式
		boolean b = false;
		p = p.compile("[\\u4E00-\\u9FA5]{2,5}(?:·[\\u4E00-\\u9FA5]{2,5})*");
		m = p.matcher(value);
		b = m.matches();
		return b;
	}

	/**
	 * 是否符合密码格式
	 * 
	 * @param value
	 * @return
	 */
	public static boolean isPassword(String value) {
		Pattern pattern = Pattern.compile("^(?!\\W*[0-9]+\\W*$)(?!\\W*[a-zA-Z]+\\W*$)[^\\s'\"]{6,16}$");// 先得判断是否含有特殊字符
		Matcher matcher = pattern.matcher(value);
		return matcher.matches();
	}
}
