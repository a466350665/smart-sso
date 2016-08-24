package com.smart.mvc.enums;

import com.smart.mvc.model.EnumItemable;

/**
 * 是否枚举
 * 
 * @author Joe
 */
public enum TrueFalseEnum implements EnumItemable<TrueFalseEnum> {

	TRUE("是", true), 
	FALSE("否", false);

	private String label;
	private Boolean value;

	private TrueFalseEnum(String label, Boolean value) {
		this.label = label;
		this.value = value;
	}

	public String getLabel() {
		return this.label;
	}

	public Boolean getValue() {
		return this.value;
	}
}
