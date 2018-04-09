package com.smart.sso.server.model;

import com.alibaba.fastjson.annotation.JSONField;
import com.smart.mvc.model.PersistentObject;

/**
 * 权限
 * 
 * @author Joe
 */
public class Permission extends PersistentObject {

	private static final long serialVersionUID = 4368792338865943489L;

	/** 应用ID */
	private Integer appId;
	/** 父ID */
	private Integer parentId;
	/** 图标 */
	@JSONField(serialize = false)
	private String icon;
	/** 名称 */
	private String name;
	/** 权限URL */
	@JSONField(serialize = false)
	private String url;
	/** 排序 */
	private Integer sort = Integer.valueOf(1);
	/** 是否菜单 */
	private Boolean isMenu;
	/** 是否启用 */
	private Boolean isEnable;
	
	public Integer getAppId() {
		return this.appId;
	}

	public void setAppId(Integer appId) {
		this.appId = appId;
	}

	public Integer getParentId() {
		return this.parentId;
	}

	public void setParentId(Integer parentId) {
		this.parentId = parentId;
	}
	
	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public Integer getSort() {
		return this.sort;
	}

	public void setSort(Integer sort) {
		this.sort = sort;
	}

	public Boolean getIsMenu() {
		return this.isMenu;
	}

	public void setIsMenu(Boolean isMenu) {
		this.isMenu = isMenu;
	}

	public Boolean getIsEnable() {
		return this.isEnable;
	}

	public void setIsEnable(Boolean isEnable) {
		this.isEnable = isEnable;
	}
	
	public String getUrlStr() {
		return url;
	}
	
	public String getPermissionIcon() {
		return icon;
	}

	public Integer getpId() {
		return this.parentId;
	}
	
	/** 以下为显示辅助参数 */
	private boolean checked = false;

	public boolean isChecked() {
		return checked;
	}

	public void setChecked(boolean checked) {
		this.checked = checked;
	}
}
