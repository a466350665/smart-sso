package com.smart.sso.server.model;

import java.io.Serializable;

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
	private long pageNo = DEFAULT_PAGE_NO;
	/** 每页行数 */
	private long pageSize = DEFAULT_PAGE_SIZE;
	/** 记录总数 */
	private long rowCount;
	
	protected PageSupport() {
    }

	protected PageSupport(long pageNo, long pageSize) {
        this.pageNo = pageNo;
        this.pageSize = pageSize;
    }

	public long getPageSize() {
		return pageSize;
	}

	public void setPageSize(long pageSize) {
		this.pageSize = pageSize;
	}

	public long getRowCount() {
		return rowCount;
	}

	public void setRowCount(long rowCount) {
		this.rowCount = rowCount;
	}

	public long getPageNo() {
		return pageNo;
	}

	public void setPageNo(long pageNo) {
		this.pageNo = pageNo;
	}

	/**
	 * 获取总页数
	 */
	public long getPageCount() {
		if (rowCount % pageSize == 0){
			return rowCount / pageSize;
		}
		else{
			return (rowCount / pageSize) + 1;
		}
	}
}