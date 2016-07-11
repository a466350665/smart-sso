package com.smart.util;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

/**
 * 日期公用类
 * 
 * @author Joe
 */
public class DateUtils {

	public static final String DATE_SHORT_FORMAT = "yyyy-MM-dd";

	public static final String DATE_FULL_FORMAT = "yyyy-MM-dd HH:mm:ss";

	public static final String DEFAULT_DATE_FORMAT = DATE_SHORT_FORMAT;

	/**
	 * 字符串按默认格式转日期
	 * 
	 * @param strDate
	 *            日期字符串
	 * @return 日期
	 * @throws ParseException
	 */
	public static Date parse(String strDate) throws ParseException {
		return parse(strDate, DEFAULT_DATE_FORMAT);
	}

	/**
	 * 字符串按指定格式转日期
	 * 
	 * @param strDate
	 *            日期字符串
	 * @param pattern
	 *            指定的日期转换格式
	 * @return 日期
	 * @throws ParseException
	 */
	public static Date parse(String strDate, String pattern) throws ParseException {
		return createDateFormat(pattern).parse(strDate);
	}

	/**
	 * 日期按默认格式转字符串
	 * 
	 * @param date
	 *            这个日期值将格式化为一个日期字符串
	 * @return 日期字符串
	 */
	public static String format(Date date) {
		return format(date, DEFAULT_DATE_FORMAT);
	}

	/**
	 * 日期按指定格式转字符串
	 * 
	 * @param date
	 *            这个日期值将格式化为一个日期字符串
	 * @param pattern
	 *            指定的日期转换格式
	 * @return 日期字符串
	 */
	public static String format(Date date, String pattern) {
		if (date == null) {
			throw new IllegalArgumentException("The date must not be null");
		}
		else {
			return createDateFormat(pattern).format(date);
		}
	}

	/**
	 * 创建日期格式化实现类
	 * 
	 * @param pattern
	 *            指定的日期转换格式
	 * @return 日期格式化实现类
	 */
	private static DateFormat createDateFormat(String pattern) {
		return new SimpleDateFormat(pattern);
	}

	/**
	 * 指定日期增加（年）
	 * 
	 * @param date
	 *            指定的一个原始日期
	 * @param amount
	 *            数值增量
	 * @return 新日期
	 */
	public static Date addYear(Date date, int amount) {
		return add(date, Calendar.DAY_OF_YEAR, amount);
	}

	/**
	 * 指定日期增加（月）
	 * 
	 * @param date
	 *            指定的一个原始日期
	 * @param amount
	 *            数值增量
	 * @return 新日期
	 */
	public static Date addMonth(Date date, int amount) {
		return add(date, Calendar.DAY_OF_MONTH, amount);
	}

	/**
	 * 指定日期增加（周）
	 * 
	 * @param date
	 *            指定的一个原始日期
	 * @param amount
	 *            数值增量
	 * @return 新日期
	 */
	public static Date addWeek(Date date, int amount) {
		return add(date, Calendar.WEEK_OF_YEAR, amount);
	}

	/**
	 * 指定日期增加（天）
	 * 
	 * @param date
	 *            指定的一个原始日期
	 * @param amount
	 *            数值增量
	 * @return 新日期
	 */
	public static Date addDay(Date date, int amount) {
		return add(date, Calendar.DATE, amount);
	}

	/**
	 * 指定日期减少（天）
	 * 
	 * @param date
	 *            指定的一个原始日期
	 * @param amount
	 *            数值减量
	 * @return 新日期
	 */
	public static Date reduceDay(Date date, int amount) {
		return add(date, Calendar.DATE, -amount);
	}

	/**
	 * 指定日期增加数量（年，月，日，小时，分钟，秒，毫秒）
	 * 
	 * @param date
	 *            指定的一个原始日期
	 * @param field
	 *            日历类Calendar字段
	 * @param amount
	 *            数值增量
	 * @return
	 */
	private static Date add(Date date, int field, int amount) {
		if (date == null) {
			throw new IllegalArgumentException("The date must not be null");
		}
		else {
			Calendar c = createCalendar(date);
			c.add(field, amount);
			return c.getTime();
		}
	}

	/**
	 * 获取日期是一年中的第几周
	 * 
	 * @param date
	 *            指定的一个原始日期
	 * @return
	 */
	public static int getWeekOfYear(Date date) {
		if (date == null) {
			throw new IllegalArgumentException("The date must not be null");
		}
		else {
			Calendar calendar = createCalendar(date);
			calendar.setFirstDayOfWeek(Calendar.MONDAY);
			return calendar.get(Calendar.WEEK_OF_YEAR);
		}
	}

	/**
	 * 获取日期是星期几(0~6,0为星期日)
	 * 
	 * @param date
	 *            指定的一个原始日期
	 * @return
	 */
	public static int getWeekOfDate(Date date) {
		if (date == null) {
			throw new IllegalArgumentException("The date must not be null");
		}
		else {
			return createCalendar(date).get(Calendar.DAY_OF_WEEK) - 1;
		}
	}

	/**
	 * 根据当前日期及生日,计算年龄,返回”*岁*个月*天”
	 * 
	 * @param birthday
	 *            生日日期
	 * @param desDay
	 *            目标日期
	 * @return
	 */
	public static Age getAge(Date birthday, Date desDay) {
		if (birthday == null || desDay == null) {
			throw new IllegalArgumentException("The birthday and desDay must not be null");
		}
		Calendar bCalendar = createCalendar(birthday);
		Calendar dCalender = createCalendar(desDay);
		int day = dCalender.get(Calendar.DAY_OF_MONTH) - bCalendar.get(Calendar.DAY_OF_MONTH);
		int month = dCalender.get(Calendar.MONTH) - bCalendar.get(Calendar.MONTH);
		int year = dCalender.get(Calendar.YEAR) - bCalendar.get(Calendar.YEAR);
		// 按照减法原理，先day相减，不够向month借；然后month相减，不够向year借；最后year相减。
		if (day < 0) {
			month--;
			// 得到上一个月，用来得到上个月的天数。
			dCalender.add(Calendar.MONTH, -1);
			day = day + dCalender.getActualMaximum(Calendar.DAY_OF_MONTH);
		}
		if (month < 0) {
			year--;
			month = (month + 12) % 12;
		}
		return new Age(year, month, day);
	}

	/**
	 * 创建日历类
	 * 
	 * @param date
	 *            指定日期
	 * @return
	 */
	private static Calendar createCalendar(Date date) {
		Calendar c = Calendar.getInstance();
		c.setTime(date);
		return c;
	}

	/**
	 * 年龄类
	 */
	public static final class Age {
		private int year;
		private int month;
		private int day;

		public Age(int year, int month, int day) {
			this.year = year;
			this.month = month;
			this.day = day;
		}

		public int getYear() {
			return year;
		}

		public int getMonth() {
			return month;
		}

		public int getDay() {
			return day;
		}

		@Override
		public String toString() {
			return new StringBuilder().append(year).append("岁").append(month).append("个月").append(day).append("天")
					.toString();
		}
	}
}
