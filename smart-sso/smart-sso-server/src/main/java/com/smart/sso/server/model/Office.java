package com.smart.sso.server.model;


import java.beans.Transient;

import com.smart.mvc.model.PersistentObject;
import com.smart.sso.server.enums.TrueFalseEnum;

/**
 * 机构
 */
public class Office extends PersistentObject {

	private static final long serialVersionUID = 26416795235226335L;
	
	/** 父ID */
	private Integer parentId;
	/** 名称 */
	private String name;
	/** 排序 */
	private Integer sort;
	/** 是否启用 */
	private Boolean isEnable;
	
	
	public Integer getParentId() {
		return this.parentId;
	}
	
	public void setParentId(Integer parentId) {
		this.parentId = parentId;
	}
	
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
	
	public Boolean getIsEnable() {
		return this.isEnable;
	}
	
	public void setIsEnable(Boolean isEnable) {
		this.isEnable = isEnable;
	}
	
	@Transient
	public Integer getPId() {
		return this.parentId;
	}
	
	@Transient
	public String getIsEnableStr() {
		return (isEnable != null && isEnable) ? TrueFalseEnum.TRUE.getLabel() : TrueFalseEnum.FALSE.getLabel();
	}
}
