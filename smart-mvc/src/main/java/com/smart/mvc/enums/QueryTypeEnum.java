package com.smart.mvc.enums;

import com.smart.mvc.model.EnumItemable;

/**
 * 查询类型
 * 
 * @author Joe
 */
public enum QueryTypeEnum implements EnumItemable<QueryTypeEnum> {
    IS_NULL("is null", ValueTypeEnum.NO), 
    IS_NOT_NULL("is not null", ValueTypeEnum.NO), 
    EQUAL("=", ValueTypeEnum.SINGLE), 
    NOT_EQUAL("<>", ValueTypeEnum.SINGLE),
    GREATER(">", ValueTypeEnum.SINGLE),
    GREATER_EQUAL(">=", ValueTypeEnum.SINGLE),
    LESS("<", ValueTypeEnum.SINGLE),
    LESS_EQUAL("<=", ValueTypeEnum.SINGLE),
    IN("in", ValueTypeEnum.COLLECTION),
    NOT_IN("not in", ValueTypeEnum.COLLECTION),
    BETWEEN("between", ValueTypeEnum.TWO),
    NOT_BETWEEN("not between", ValueTypeEnum.TWO),
    LIKE("like", ValueTypeEnum.SINGLE),
    NOT_LIKE("not like", ValueTypeEnum.SINGLE);

	private String label;
	private ValueTypeEnum valueType;

	private QueryTypeEnum(String label, ValueTypeEnum valueType) {
		this.label = label;
		this.valueType = valueType;
	}

	@Override
	public String getLabel() {
		return this.label;
	}

	@Override
	public String getValue() {
		return this.label;
	}

    public ValueTypeEnum getValueType() {
        return valueType;
    }
}