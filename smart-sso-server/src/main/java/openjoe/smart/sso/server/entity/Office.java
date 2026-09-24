package openjoe.smart.sso.server.entity;


import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import openjoe.smart.stage.mybatisplus.entity.BaseEntity;

import java.beans.Transient;

/**
 * 机构
 */
@TableName
public class Office extends BaseEntity {
	
	/** 父ID（允许为null，表示顶级机构；更新时必须强制写入，否则会被MyBatis-Plus的NOT_NULL策略忽略） */
	@TableField(updateStrategy = FieldStrategy.ALWAYS)
	private Long parentId;
	/** 名称 */
	private String name;
	/** 排序 */
	private Integer sort;
	/** 是否启用 */
	private Boolean isEnable;

	public Long getParentId() {
		return this.parentId;
	}
	
	public void setParentId(Long parentId) {
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
	public Long getPId() {
		return this.parentId;
	}
}
