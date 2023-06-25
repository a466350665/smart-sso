package com.smart.sso.server.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;


/**
 * 分页包含列表list属性基类
 * 
 * @author Joe
 * @param <T>
 */
public class Page<T> extends PageSupport implements Serializable {
	
	private static final long serialVersionUID = 7002501955628078021L;
	/** 当前页的数据 */
	private List<T> list = Collections.emptyList();

	protected Page() {
	    super();
	}
	
	protected Page(long pageNo, long pageSize) {
        super(pageNo, pageSize);
    }
	
    public static <T> Page<T> create() {
        return new Page<>();
    }

    public static <T> Page<T> create(long pageNo, long pageSize) {
        return new Page<>(pageNo, pageSize);
    }

	/**
	 * 获得分页内容
	 * 
	 * @return
	 */
	public List<T> getList() {
		return list;
	}

	/**
	 * 设置分页内容
	 * 
	 * @param list
	 */
	public void setList(List<T> list) {
		this.list = list;
	}
}
