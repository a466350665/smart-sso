package com.smart.mvc.model;

/**
 * 键值对存储模型
 * 
 * @author Joe
 */
public class Item implements Itemable {
	
	private String label;
	private Object value;

	public Item() {
	}

	public Item(String label, Object value) {
		this.label = label;
		this.value = value;
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public Object getValue() {
		return value;
	}

	public void setValue(Object value) {
		this.value = value;
	}
}
