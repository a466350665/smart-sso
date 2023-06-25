package com.smart.sso.server.enums;

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

	@Override
	public String getLabel() {
		return this.label;
	}

	@Override
	public Boolean getValue() {
		return this.value;
	}
}
