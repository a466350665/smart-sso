package com.smart.sso.server.model;

import com.smart.mvc.enums.TrueFalseEnum;
import com.smart.mvc.model.PersistentObject;

/**
 * 角色
 * 
 * @author Joe
 */
public class Role extends PersistentObject {

	private static final long serialVersionUID = 564115576254799088L;

	/** 名称 */
	private String name;
	/** 排序 */
	private Integer sort = Integer.valueOf(1);
	/** 描述 */
	private String description;
	/** 是否启用 */
	private Boolean isEnable = Boolean.valueOf(true);

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Integer getSort() {
		return this.sort;
	}

	public void setSort(Integer sort) {
		this.sort = sort;
	}

	public String getDescription() {
		return this.description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Boolean getIsEnable() {
		return this.isEnable;
	}

	public void setIsEnable(Boolean isEnable) {
		this.isEnable = isEnable;
	}
	
	/** 以下为显示辅助参数 */
	private Boolean isChecked = Boolean.valueOf(false);
	
	public Boolean getIsChecked() {
		return isChecked;
	}

	public void setIsChecked(Boolean isChecked) {
		this.isChecked = isChecked;
	}
	
	public String getIsEnableStr() {
		return (isEnable != null && isEnable) ? TrueFalseEnum.TRUE.getLabel() : TrueFalseEnum.FALSE.getLabel();
	}
}
