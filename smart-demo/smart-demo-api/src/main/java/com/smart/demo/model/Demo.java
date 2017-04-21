package com.smart.demo.model;

import com.smart.mvc.model.PersistentObject;


/**
 * 测试
 * 
 * @author Joe
 */
public class Demo extends PersistentObject {

	private static final long serialVersionUID = 1106412532325860697L;

	/** 名称 */
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
