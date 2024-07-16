package openjoe.smart.sso.server.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import openjoe.smart.stage.mybatisplus.entity.BaseEntity;

/**
 * 应用
 * 
 * @author Joe
 */
@TableName("sys_app")
public class App extends BaseEntity {
	
	/** 名称 */
	private String name;
	/** 编码  */
	private String code;
	/** 排序 */
	private Integer sort;
	/** 是否启用 */
	private Boolean isEnable;
	
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

	public Boolean getIsEnable() {
		return this.isEnable;
	}

	public void setIsEnable(Boolean isEnable) {
		this.isEnable = isEnable;
	}
}
