package com.smart.sso.server.model;

import java.util.Date;

import com.alibaba.fastjson.annotation.JSONField;
import com.smart.mvc.enums.TrueFalseEnum;
import com.smart.mvc.model.PersistentObject;

/**
 * 应用
 * 
 * @author Joe
 */
public class App extends PersistentObject {

	private static final long serialVersionUID = 7902814112969375973L;
	
	/** 名称 */
	private String name;
	/** 编码  */
	private String code;
	/** 排序 */
	private Integer sort = Integer.valueOf(1);
	/** 创建时间 */
	@JSONField(format = "yyyy-MM-dd HH:mm:ss")
	private Date createTime;
	/** 是否启用 */
	private Boolean isEnable = Boolean.valueOf(true);
	
	public App(){
	}
	
	public App(String name, String code, Integer sort, Boolean isEnable) {
		super();
		this.name = name;
		this.code = code;
		this.sort = sort;
		this.isEnable = isEnable;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public Integer getSort() {
		return this.sort;
	}

	public void setSort(Integer sort) {
		this.sort = sort;
	}

	public Date getCreateTime() {
		return this.createTime;
	}

	public void setCreateTime(Date createTime) {
		this.createTime = createTime;
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
