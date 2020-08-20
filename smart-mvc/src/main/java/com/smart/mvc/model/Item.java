package com.smart.mvc.model;

import java.io.Serializable;

/**
 * 键值对存储模型
 * 
 * @author Joe
 */
public class Item implements Itemable, Serializable {
	
    private static final long serialVersionUID = -447313839033608947L;
    
    private String label;
    private Object value;
	
    protected Item() {
    }

    protected Item(String label, Object value) {
        this.label = label;
        this.value = value;
    }

    public static Item create() {
        return new Item();
    }
    
    public static Item create(String label, Object value) {
        return new Item(label, value);
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
