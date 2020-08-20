package com.smart.mvc.model;

import java.io.Serializable;
import java.util.regex.Pattern;

/**
 * 分页基类
 * 
 * @author Joe
 */
public class PageSupport implements Serializable {

	private static final long serialVersionUID = 2234283310680151858L;
	/** 默认当前页码 */
    public static final int DEFAULT_PAGE_NO = 1;
	/** 默认每页行数 */
	public static final int DEFAULT_PAGE_SIZE = 20;

	/** 当前页码 */
	private int pageNo = DEFAULT_PAGE_NO;
	/** 每页行数 */
	private int pageSize = DEFAULT_PAGE_SIZE;
	/** 记录总数 */
	private long rowCount;
	
	private String orderBy = ""; // 标准查询有效， 实例： updatedate desc, name asc

	protected PageSupport() {
    }

	protected PageSupport(int pageNo, int pageSize) {
        this.pageNo = pageNo;
        this.pageSize = pageSize;
    }

	public int getPageSize() {
		return pageSize;
	}

	public void setPageSize(int pageSize) {
		this.pageSize = pageSize;
	}

	public long getRowCount() {
		return rowCount;
	}

	public void setRowCount(long rowCount) {
		this.rowCount = rowCount;
	}

	public int getPageNo() {
		return pageNo;
	}

	public void setPageNo(int pageNo) {
		this.pageNo = pageNo;
	}

	/**
	 * 获取总页数
	 */
	public long getPageCount() {
		if (rowCount % pageSize == 0)
			return rowCount / pageSize;
		else
			return (rowCount / pageSize) + 1;
	}

	public int getFirstResult(){
		int firstResult = (getPageNo() - 1) * getPageSize();
		if (firstResult >= getRowCount()) {
			firstResult = 0;
		}
		return firstResult;
	}

	public int getMaxResults(){
		return getPageSize();
	}
	
	public String getOrderBy() {
		// SQL过滤，防止注入 
		String reg = "(?:')|(?:--)|(/\\*(?:.|[\\n\\r])*?\\*/)|"
					+ "(\\b(select|update|and|or|delete|insert|trancate|char|into|substr|ascii|declare|exec|count|master|into|drop|execute)\\b)";
		Pattern sqlPattern = Pattern.compile(reg, Pattern.CASE_INSENSITIVE);
		if (sqlPattern.matcher(orderBy).find()) {
			return "";
		}
		return orderBy;
	}
	
	/**
	 * 设置查询排序，标准查询有效， 实例： updatedate desc, name asc
	 */
	public void setOrderBy(String orderBy) {
		this.orderBy = orderBy;
	}
}